# Write Values to imported Template ============================================

values <- list(lab_age,
               lab_atsi,
               lab_idu,
               lab_risk,
               lab_sw,
               rapid_age,
               rapid_atsi,
               rapid_idu,
               rapid_risk,
               rapid_sw,
               poct_age,
               poct_atsi,
               poct_idu,
               poct_risk,
               poct_sw,
               poct_condom_use,
               poct_diag,
               poct_partners,
               poct_testing_history_total,
               poct_testing_history_last,
               poct_testing_history_type,
               poct_testing_history_where
               )

ranges <- list("lab_age",
               "lab_atsi",
               "lab_idu",
               "lab_risk",
               "lab_sw",
               "rapid_age",
               "rapid_atsi",
               "rapid_idu",
               "rapid_risk",
               "rapid_sw",
               "poct_age",
               "poct_atsi",
               "poct_idu",
               "poct_risk",
               "poct_sw",
               "poct_condom_use",
               "poct_diag",
               "poct_partners",
               "poct_testing_history_total",
               "poct_testing_history_last",
               "poct_testing_history_type",
               "poct_testing_history_where"
               )

# Lab Values

for(i in 1:length(values)) {
  
  XLConnect::writeNamedRegion(template, 
                              values[[i]],
                              header = FALSE, 
                              name = ranges[i]
                              )
}

#rm(list = setdiff(ls(),c("template","scratch.path")))