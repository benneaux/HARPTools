## Adding Glenda's checking info

Program <- "HCV" # i.e. HBV or HCV
Period <- "August" # i.e. Month, FY

# name of the file path in quotes (ending in /) 
#NOTE: REPLACE EACH BACKSLASH (\) WITH A FORWARD SLASH (/)!!!!
file.path <- "O:/HARP_Data/01 Reports/HCV/2016/HCV Treatment Dispensing/08 August/01 Data/" 

# name of the file to be imported in quotes
file.name <- "HCV August 2016.XLS"
full.path <- paste(file.path,file.name, sep = "")

import.data <- 
  as.data.table(
    read_excel(
      full.path,
      col_names = FALSE,
      sheet = 2
    )
  )

clean.data <- import.data[
  rowSums(is.na(import.data))# Calculates no. of NAs in each row 
  != ncol(import.data),] # Calculates number of columns present.
clean.data <- clean.data[which(!is.na(clean.data$X2)),]
names(clean.data) <- c("MRN", "Type")

rm(file.name, import.data)

clients <- clean.data[which(str_detect(clean.data$Type,Program)),]
clients$Type <- Program
clients <- unique(clients)

#HCV.clients <- clean.data[which(str_detect(clean.data$Type,"HCV")),]
#HIV.clients <- clean.data[which(str_detect(clean.data$Type,"HIV")),]
#Transplant.clients <- clean.data[which(str_detect(clean.data$Type,"transplant")),]

rm(clean.data)

clients$MRN <- gsub("O", 0, clients$MRN)

# Removes # marks placed at the front of some of the MRNs.
clients$MRN <- gsub("#", "", clients$MRN)

# Some of the MRNs are recognised as numbers and R formats them accordingly,
# i.e. with decimal places. After some other conversions they become 
# characters again, so we need to trim the decimals.

condition <- str_detect(clients$MRN,"\\..*")==TRUE

for(i in 1:nrow(clients)){
  if(condition[i]){
    clients[i,1] = gsub("\\..*","", clients[i,MRN])
  }
}

# Remove unnescessary variables.
rm(condition)

dispensing <- left_join(final.Dispensing, clients, by = "MRN")
dispensing <- dispensing[which(dispensing$Type=="HBV"),]
#write.csv(HBV.dispensing, "Final_HBV_Dispensing_FY1516.csv")
setwd(file.path)
write.csv(
  final,
  paste(Program,Period,"Dispensing.csv",sep = "_"),
  row.names = FALSE
)


Unique.clients <- uniqueN(clients)
Unique.scripts <- nrow(dispensing)
Total.cost <- sum(dispensing$Cost)

Unique.clients
Unique.scripts
Total.cost

rm(final, final.Reversals, final.Dispensing, i)
