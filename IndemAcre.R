##############################################
# Read and clean Indemnity and Irrigation Data
##############################################
# Paloch Suchato
# April 9, 2019
# psuchato@huskers.unl.edu
setwd("Set your working directory here")
dir_loc = "sobs/"
library(data.table)
library(tidyverse)

###################
# Helper Function
###################

source("path to function file here")
#------------------
# Read in data
#-----------------
data <- read_text_file()
cov_by_county <- data[[1]] %>% data.table()
irr_by_county <- data[[2]] %>% data.table()
# Calculate Indemnity paments per acre in dollar
cov_by_county[,IndemPerAcerDollar := ifelse(is.nan(IndemnityAmount/NetReportQuantity),0,IndemnityAmount/NetReportQuantity)]


#----------------
# Data Cleaning 
#----------------
crop_data <- cov_by_county[CommodityName %in% c("CORN","SOYBEANS","WHEAT") & InsurancePlanAbb %in% c("RP","YP")]%>%
            group_by(.,LocationStateAbb,CommodityYear,LocationCountyName,InsurancePlanAbb,CommodityName)%>% 
            summarise(TotalIndemDollar = sum(as.numeric(IndemnityAmount)),TotalUnitInsure = sum(UnitEarningPremiumCount),
            TotalUnitIndem = sum(UnitIndemtifyCount), CountyReportQuantityAcre = sum(NetReportQuantity),
            TotalPremiumDollar = sum(TotalPremiumAmount),TotalSubsidyDollar = sum(SubsidyAmount),AvgIndemPerAcre = mean(IndemPerAcerDollar)) %>%
            data.table() %>%
            .[,"IndemPerInsureUnit" := ifelse(is.nan(TotalUnitIndem/TotalUnitInsure),0,TotalUnitIndem/TotalUnitInsure)]

irr_data <- irr_by_county[CommodityName %in% c("Corn","Soybeans","Wheat") & InsurancePlanAbb %in% c("RP","YP")] %>%
  group_by(.,StateAbb,CommodityYear,CountyName,InsurancePlanAbb,CommodityName,PracticeName)%>%
  summarise(NetReportAcreByPractice = sum(NetReportLevelAmount)) %>%
  data.table() %>%
  .[,"NetReportAcre" := sum(NetReportAcreByPractice), by = .(CommodityYear,StateAbb,CountyName,CommodityName)] %>%
  .[,"PercentIrrigate" := round((NetReportAcreByPractice/NetReportAcre)*100)]
  
irrigate <- irr_data[PracticeName == "Irrigated"]
irrigate[,CommodityName := toupper(CommodityName)]
crop_inurance_by_county <- merge(crop_data,irrigate,by.x = c("LocationStateAbb","CommodityYear","LocationCountyName","InsurancePlanAbb","CommodityName"),
                                 by.y = c("StateAbb","CommodityYear","CountyName","InsurancePlanAbb","CommodityName"))

corn2018 <- crop_inurance_by_county[CommodityName == "CORN" & CommodityYear == 2018 & InsurancePlanAbb == "RP"]
wheat2018 <- crop_inurance_by_county[CommodityName == "WHEAT" & CommodityYear == 2018 & InsurancePlanAbb == "RP"]
soybean2018 <- crop_inurance_by_county[CommodityName == "SOYBEANS" & CommodityYear == 2018 & InsurancePlanAbb == "RP"]
# saveRDS(crop_inurance_by_county,"CorpInsuranceDataByCounty.rds")
# saveRDS(corn2018,"corn_2018.rds")
# saveRDS(wheat2018,"wheat_2018.rds")
# saveRDS(soybean2018, "soybean_2018.rds")


#--------------------------
# Add yield Exclusion Data
#--------------------------
ye_corn <- read_csv("YE_data/YE_corn.csv", col_names = TRUE) %>% data.table()
ye_corn[,"StateAbb" := state2abb(StateName)]
ye_corn <- ye_corn[,!c("StateName","FIPS","Primary","Contiguous","YearEligible"),with=FALSE]
corn_ye_2018 <- merge(corn2018,ye_corn,by.x=c("LocationStateAbb","LocationCountyName"),by.y=c("StateAbb","CountyName"))
# write_csv(corn_ye_2018,"corn_ye_2018.csv")
