# HIV Testing Report.

## Setup =======================================================================

require(XLConnect)
require(RODBC)
require(data.table)
require(stringr)
require(dplyr)


file.path <- "O:/HARP_Data/04 Templates/HIVSTI/" 
file.name <- "HIV testing and CD4 reports layout.xlsx"
full.path <- paste0(file.path,file.name)

scratch.path <- "O:/HARP_Data/01 Reports/Scratch/"

template <- loadWorkbook(full.path, create = FALSE)
setStyleAction(template,XLC$"STYLE_ACTION.NONE")


startdate <- "'10/01/2016'" # Note: Dates must be in wacky US format
enddate <- "'12/31/2016'"  # i.e mm/dd/yyyy

source("HIVTestingData.R")

HIVTestingData(startdate,enddate)

# Write Values to imported Template ============================================

values <- list(lab_age,lab_atsi,lab_idu,lab_risk,lab_sw,
               poct_age,poct_atsi,poct_idu,poct_risk,poct_sw,
               poct_condom_use,poct_diag,poct_partners,
               poct_testing_history_total,poct_testing_history_last,
               poct_testing_history_type,poct_testing_history_where)

ranges <- list("lab_age","lab_atsi","lab_idu","lab_risk","lab_sw",
               "poct_age","poct_atsi","poct_idu","poct_risk","poct_sw",
               "poct_condom_use","poct_diag","poct_partners",
               "poct_testing_history_total","poct_testing_history_last",
               "poct_testing_history_type","poct_testing_history_where")

# Lab Values

for(i in 1:length(values)) {
  writeNamedRegion(template, values[[i]], header = FALSE, name = ranges[i])
}

#rm(list = setdiff(ls(),c("template","scratch.path")))

# Save imported template to file ===============================================

setForceFormulaRecalculation(template, sheet = c(1, 2), TRUE)
saveWorkbook(template, paste0(scratch.path,"HIVTestingReport.xlsx"))


