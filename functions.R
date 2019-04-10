##########################
# helper functions
#########################
# Paloch Suchato
# April 9,2019
# psuchato@huskers.unl.edu

# this function take in folder location name, file name and colum name as an input arguemnt
# The function read in text file with | separate and return data.table of the data read
read_with_header <- function(folder="cov/",data_file,name_vec){
  data <- read.csv(paste(dir_loc,folder,data_file,sep=""),sep = "|",strip.white = TRUE) %>% data.table()
  names(data) <- name_vec
  
  return(data)
}


# This function read in text file from Data directory and turn it into data.table. The fuction return
# a list of coverage and irrigation file
read_text_file <- function(){
  cov_file_list <-list.files(path=paste(dir_loc,"cov",sep=""),pattern="sobcov[0-9][0-9].txt")
  col_name <- c("CommodityYear","LocationStateCode","LocationStateAbb","LocationCountyCode","LocationCountyName",
                "CommodityCode","CommodityName","InsurancePlan","InsurancePlanAbb",
                "CoverageCategory","DeliveryType","CoverageLevel","PoliciesSoldCount",
                "PoliciesEarningPremuiumCount","PoliciesIndemtifyCount","UnitEarningPremiumCount",
                "UnitIndemtifyCount","QuantityType","NetReportQuantity","EndorsedAcre",
                "LabilityAmount","TotalPremiumAmount","SubsidyAmount","IndemnityAmount",
                "LossRatio")
  
  
  cov_data <-lapply(cov_file_list, function(x) read_with_header(data_file = x,name_vec = col_name))
  
  cov_file_list1 <-list.files(path=paste(dir_loc,"irr_practice",sep=""),pattern="SOBSCCTPU[0-9][0-9].TXT")
  
  col_name1 <- c("CommodityYear","StateCode","StateName","StateAbb","CountyCode","CountyName",
                 "CommodityCode","CommodityName","InsurancePlanCode","InsurancePlanAbb",
                 "CoverageTypeCode","CoverageLevelPercent","DeliveryID","TypeCode","TypeName","PracticeCode",
                 "PracticeName","UnitStructureCode","UnitStructureName","NetReportLevelAmount","ReportingLevelType",
                 "LiabilityAmount","TotalPremiumAmount","SubsidyAmount","IndemnityAmount","LossRatio","EndorsedCommodityAmount")
  
  irr_data <-lapply(cov_file_list1, function(x) read_with_header(folder="irr_practice/",data_file = x,name_vec = col_name1))
  
  cov_by_county <- rbindlist(cov_data)
  irr_by_county <- rbindlist(irr_data)
  return(list(cov_by_county, irr_by_county))
}

state2abb <- function(state_name){
  states <- state.name
  abb <- state.abb
  return(abb[match(state_name,states)])
}