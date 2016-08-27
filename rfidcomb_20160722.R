##################################
# RFID Data merging skript
##################################
#
# This skript can be used to combine the single-day files from RFID-Antennas 
# to one single dataframe which can than be used for further analysis
#
# Main source of code https://psychwire.wordpress.com/2011/06/03/merge-all-files-in-a-directory-using-r-into-a-single-dataframe/
# Adapted by Dominic Martin Imperial College London

#### Skript for feederAll data ####
# Folder with all the single-day csv files which you want to combine must be wd
setwd("~/Documents/Studium/Msc EEC/Msc Project/Analysis/feederAll")

#create a list with all file names
file_list <- list.files()

# main code: basically creates combined file first and than adds other to it
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("rfidcomb3")){
    rfidcomb3 <- read.table(file, header=TRUE, sep=";", quote = "", fill = TRUE, row.names=NULL)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("rfidcomb3")){
    temp_dataset <-read.table(file, header=TRUE, sep=";", quote = "", fill = TRUE, row.names=NULL)
    if(ncol(temp_dataset)!=7){print(file)} else {
    rfidcomb3<-rbind(rfidcomb3, temp_dataset)
    rm(temp_dataset)
  }
  
 }
  
}
  

# save dataframe as txt
write.table(rfidcomb3, "rfid_combineddataset_rfidcomb3_20160722.txt", sep="\t")

setwd("~/Documents/Studium/Msc EEC/Msc Project/Analysis")


##### Adapted Skript for in-and-out data ####
# Folder with all the single-day csv files which you want to combine must be wd
setwd("~/Documents/Studium/Msc EEC/Msc Project/Analysis/birds_in_and_out-nointeractions")

#create a list with all file names
file_list <- list.files()

# main code: basically creates combined file first and than adds other to it
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("iocomb")){
    iocomb <- read.table(file, header=TRUE, sep=",", quote = "", fill = TRUE, row.names=NULL)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("iocomb")){
    temp_dataset <-read.table(file, header=TRUE, sep=",", quote = "", fill = TRUE, row.names=NULL)
    if(ncol(temp_dataset)!=15){print(file)} else {
      iocomb<-rbind(iocomb, temp_dataset)
      rm(temp_dataset)
    }
    
  }
  
}


# save dataframe as txt
write.table(iocomb, "in_and_out_dataset_iocomb_20160722.txt", sep="\t")

setwd("~/Documents/Studium/Msc EEC/Msc Project/Analysis")

