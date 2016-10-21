# HIV Testing Report.

## Setup =======================================================================

require(XLConnect)
require(RODBC)
require(data.table)
require(stringr)



file.path <- "O:/HARP_Data/04 Templates/HIVSTI/" 
file.name <- "HIV testing and CD4 reports layout.xlsx"
full.path <- paste0(file.path,file.name)

scratch.path <- "O:/HARP_Data/01 Reports/Scratch/"

template <- loadWorkbook(full.path, create = FALSE)
setStyleAction(template,XLC$"STYLE_ACTION.NONE")


startdate <- "'07/01/2016'" # Note: Dates must be in wacky US format
enddate <- "'09/30/2016'"  # i.e mm/dd/yyyy

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
  writeNamedRegion(template, (values)[[i]], header = FALSE, name = ranges[i])
}

# writeNamedRegion(template, lab_age, header=FALSE, 
#                  name = "lab_age")
# writeNamedRegion(template, lab_atsi, header=FALSE, 
#                  name = "lab_atsi")
# writeNamedRegion(template, lab_idu, header=FALSE, 
#                  name = "lab_idu")
# writeNamedRegion(template, lab_risk, header=FALSE, 
#                  name = "lab_risk")
# writeNamedRegion(template, lab_sw, header=FALSE, 
#                  name = "lab_sw")
# 
# # POCT Values
# 
# writeNamedRegion(template, poct_age, header=FALSE, 
#                  name = "poct_age")
# writeNamedRegion(template, poct_atsi, header=FALSE, 
#                  name = "poct_atsi")
# writeNamedRegion(template, poct_condom_use, header=FALSE, 
#                  name = "poct_condom_use")
# writeNamedRegion(template, poct_diag, header=FALSE, 
#                  name = "poct_diag")
# writeNamedRegion(template, poct_idu, header=FALSE, 
#                  name = "poct_idu")
# writeNamedRegion(template, poct_partners, header=FALSE, 
#                  name = "poct_partners")
# writeNamedRegion(template, poct_risk, header=FALSE, 
#                  name = "poct_risk")
# writeNamedRegion(template, poct_sw, header=FALSE, 
#                  name = "poct_sw")
# writeNamedRegion(template, poct_testing_history_last, header=FALSE, 
#                  name = "poct_testing_history_last")
# writeNamedRegion(template, poct_testing_history_total, header=FALSE, 
#                  name = "poct_testing_history_total")
# writeNamedRegion(template, poct_testing_history_type, header=FALSE, 
#                  name = "poct_testing_history_type")
# writeNamedRegion(template, poct_testing_history_where, header=FALSE, 
#                  name = "poct_testing_history_where")

rm(list = setdiff(ls(),c("template","scratch.path")))

# Save imported template to file ===============================================

setForceFormulaRecalculation(template, sheet = c(1, 2), TRUE)
saveWorkbook(template, paste0(scratch.path,"HIVTestingReport.xlsx"))


