# Cleans up HCV Ordering Report.
rm(list =ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###############################################################################
#
# Edit these values each time
# 
###############################################################################

file_loc <- "A:/03 Projects/Incentive Referrals/"
file_name <- "NOV16-JAN2017_RedeemedVouchers_TAM.xlsx"
file_path <- paste0(file_loc,file_name)
RDS_Name <- "Data/NOV16-JAN17_Clients_TAM.rds" 
startdate <- "'11/22/2016'" # Note: Dates must be in wacky US format
enddate <- "'02/28/2017'"  # i.e mm/dd/yyyy
output_name <- "Data/TAM_IncentivesData_Nov16-Jan17.xlsx"

###############################################################################
#
# Run these as a block - They will generate the report and save to Data.
# 
###############################################################################


source("setup.R")
source("import.R")
source("SQL.R")
source("datamanipulation.R")
source("write.R")
