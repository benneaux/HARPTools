# Save imported template to file ===============================================

XLConnect::setForceFormulaRecalculation(template, 
                                        sheet = c(1, 2),
                                        TRUE
                                        )

XLConnect::saveWorkbook(template, 
                        paste0(scratch.path,
                               "HIVTestingReport.xlsx"
                               )
                        )
