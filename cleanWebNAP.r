#===============================================================================    
#
# Setup 
#
#===============================================================================

  require(data.table)
  require(DT)
  require(RODBC)
  
  # Connect to Access db
  
  # Change these values to match the file structure in A:\01 Reports. Here I
  # usually stick with month number and name: e.g. 01 January. Or I go with 
  # Quarter number and name: e.g. 01 Jul-Sep.
  
    Month.Num <- "11"
    Month.Name <- "November"
  
  # Sets the directory path to use for both retrieving the data and saving the
  # reports afterwards.
  
    directory.path <- paste0(
      "A:\\01 Reports\\HIVSTI\\2016\\HIVSTI Minimum Data Set\\",
      Month.Num, # defined above
      " ",
      Month.Name # defined above
      )
  
  # The location of the cdu.mdb file exported by SHIP. Remember to save it into
  # a folder named "01 Data" prior to starting this. 
  
    db.path <- paste0(directory.path, "\\01 Data\\cdu.mdb")
  
  # The connection channel used by RODBC to access cdu.mdb
  
    channel <- odbcConnectAccess(db.path)
  
  # This checks to see if there is a file called "03 Reports" in the directory.path
  # If there is it WILL NOT OVERRIDE IT so don't worry. It won't crash R either.
    
    dir.create(file.path(directory.path,"\\03 Reports"), showWarnings = FALSE)
  
  # The path to save the reports into
    
    report.path <- paste0(
      directory.path,
      "\\03 Reports"
      )

#===============================================================================    
#
# DATA MANIPULATION 
#
#===============================================================================

# Patient Data
    
  # Import the 'patient' data file
    wn.patient <- data.table(sqlFetch(channel, "tmpWebNAPMDSPatient"))
    
  # Find any missing postcodes and update to "9999"
    wn.patient <- wn.patient[is.na(`Post Code`),`Post Code`:=9999]
    
  # Find all postcodes = '9999' and change the corres. suburb field to "UNUN9999"  
    wn.patient <- wn.patient[`Post Code`==9999, Suburb:="UNUN9999"]
  
  # Do the same for the street address.
    wn.patient <- wn.patient[`Post Code`==9999, Street:="UNUN9999"]
  
  # Delete any records that are incomplete (these will cause the upload to fail).    
    wn.patient <- wn.patient[-which(is.na(`Service Unit Code`)), ]

  # Write the data as a .csv in the report.path specified above.    
    write.csv(wn.patient,  paste0(report.path,"\\WebNAPMDSPatient.csv"))

# Service Unit data    
    
  # Import the 'Service Unit' data file
    wn.service <- data.table(sqlFetch(channel, "tmpWebNAPMDSServiceUnit"))
  
  # Delete any records that are incomplete (these will cause the upload to fail).    
    wn.service <- wn.service[-which(is.na(Code)), ]
    
  # Write the data as a .csv in the report.path specified above.   
    write.csv(wn.service,  paste0(report.path,"\\WebNAPMDSServiceUnit.csv"))
    
# Summary data    
    
  # Import the 'Summary' data file
    wn.summary <- data.table(sqlFetch(channel, "tmpWebNAPMDSSummary"))
    
  # Delete any records that are incomplete (these will cause the upload to fail).    
    wn.summary <- wn.summary[-which(is.na(`Service Unit Code`)), ]
    
  # Write the data as a .csv in the report.path specified above.   
    write.csv(wn.summary,  paste0(report.path,"\\WebNAPMDSSummary.csv"))

rm(list=ls())