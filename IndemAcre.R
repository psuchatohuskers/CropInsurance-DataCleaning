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

#----- Load Helper Functions -----#

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

#---- Delete Unuse data ----#
rm(data)
rm(cov_by_county)
rm(irr_by_county)

irrigate <- irr_data[PracticeName == "Irrigated"]
irrigate[,CommodityName := toupper(CommodityName)]
crop_insurance_by_county <- merge(crop_data,irrigate,by.x = c("LocationStateAbb","CommodityYear","LocationCountyName","InsurancePlanAbb","CommodityName"),
                                  by.y = c("StateAbb","CommodityYear","CountyName","InsurancePlanAbb","CommodityName"))

# saveRDS(crop_inurance_by_county,"CorpInsuranceDataByCounty.rds")
# saveRDS(corn2018,"corn_2018.rds")
# saveRDS(wheat2018,"wheat_2018.rds")
# saveRDS(soybean2018, "soybean_2018.rds")


#--------------------------
# Add yield Exclusion Data
# https://legacy.rma.usda.gov/news/currentissues/aph/index.html
#--------------------------

ye_data <- readYE()
crop_insurance_after_15 <- merge(crop_insurance_by_county[CommodityYear >= 2015],ye_data,by.x=c("LocationStateAbb","LocationCountyName","CommodityName")
                                 ,by.y=c("StateAbb","CountyName","Commodity"), all.x = TRUE)
crop_insurance_before_15 <- crop_insurance_by_county[CommodityYear < 2015]
crop_insurance_before_15[,NumYearEligible := NA]
crop_insurance_by_county <- rbind(crop_insurance_before_15,crop_insurance_after_15)
crop_insurance_by_county[is.na(NumYearEligible),NumYearEligible := 0]

#----- Create Dummy Variables ----#
crop_insurance_by_county[,YEEligible := ifelse(NumYearEligible > 0,1,0)]
crop_insurance_by_county[,IsRP := ifelse(PracticeName == "RP",1,0)]
crop_insurance_by_county[,IsCorn := ifelse(CommodityName == "CORN",1,0)]
crop_insurance_by_county[,IsSoybeans := ifelse(CommodityName == "SOYBEANS",1,0)]