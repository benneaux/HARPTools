

wb <- loadWorkbook(output_name, create = TRUE)
createSheet(wb, name = "Results")
writeWorksheet(wb, results, "Results")

createSheet(wb, name = "Tests")
writeWorksheet(wb, testing, "Tests")

createSheet(wb, name = "Visits")
writeWorksheet(wb, visitdata, "Visits")

createSheet(wb, name = "Clients")
writeWorksheet(wb, client_data, "Clients")

saveWorkbook(wb)
