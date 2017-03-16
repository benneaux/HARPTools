saveRDS(
  data.table::as.data.table(
    readxl::read_excel(file_path,
                       col_names = TRUE,
                       sheet = 1
                       )
    ),
  RDS_Name
  )
