######################################################
# Analysis MSc Project: Social Hierarchy & Survival #
#####################################################

# This R-Script does all the analysis presented in the MSc-Thesis by Dominic Martin

# TITLE: "High social status is associated with improved first-winter survival in a wild passerine population"
# SUPERVISOR: Dr. Julia Schroeder
# INSTITUTION: Imperial College London, Silwood Park Campus
# DATA SOURCE: Lundy Sparrow Project (University of Sheffield, Imperial College London)
# AUTHOR: Dominic Martin 1) 2) 3)
# 1) Current address: PhD Student, Biodiversity, Macroecology & Conservation Biogeography, Georg-August University of Goettingen
# 2) E-Mail: dominic.martin@uni-goettingen.de
# 3) Github Profile: https://github.com/DominicMart

# This script has 8 parts:
# Part 1: Getting last live record data for each bird recorded in the dominance videos
# Part 2: Combining yuhu database with last live record data
# Part 3: Converts Last live record date for each BirdID into Season when last recorded (Summer/Winter)
# Part 4: Puts together Yuhu database and llrComplete 4 based on BirdID to yuhu_llr
# Part 5: Subsetting, overview metrics & Repeatability
# Part 6: Builds mixed models with the binomial response variable "Alive" which are shown in the Thesis
# Part 7: Odd ratios and plots shown in Thesis
# Part 8: Counting number of breeding pairs in study period

# REQUIRED INPUT DATA (Will be made available after publication): 
# 1) "lastliverecord_dbextraction_20160721.csv": This is a extraction from the Lundy House Sparrow database all last live records (2001-20160721)
# 2) "deathdates_20160706.csv": This is a extraction from the Lundy House Sparrow database containing all recorded deathdates (2001-20160706)
# 3) "allcaptures_dbextraction_20160721.csv": This is an extraction from the Lundy House Sparrow database containing all captures (2001-20160721)
# 4) "rfid_combineddataset_rfidcomb3_20160722.txt": This is a combined dataset of all PIT (i.e. RFID-Chip) readings at an antenna at a permanent
#              feeder on Lundy Island. The single RFID files for each day were combined using the code rfid.comb.R by Dominic Martin
# 5) "allcodes_dbextraction_20160721.csv": This is an extraction from the Lundy House Sparrow database containing all codes (rings, BirdID, 
#              nestlingID, PIT's (i.e. RFID Chips)) given to birds between 2001 - 20160721
# 6) "MegaDataBase-v97-201311-201606-FY-Dominance_Lundy_20160713.csv": This is the database with all recorded interactions at the social status
#              sampling events. Here it is not used to calculate social status but to get the observations on the video as evidence of survival.
# 7) "in_and_out_dataset_iocomb_20160722.txt": This is a dataset of all birds going in and out the videoframe but which did not interact. This
#              method was only applied for a small part of the time analysed and was not applied at events W2016 and S2016 as this proofed too 
#              time-consuming.
# 8) "yuhu.csv": Database containing BirdID's and social status measures for all events (Winter 2014 - Summer 2016) based on 
#              "MegaDataBase-v97-201311-201606-FY-Dominance_Lundy_20160713.csv". This database is compiled, checked and Elo-ratings are 
#              calcluated with a separate R-Script by Alfredo S??nchez-T??jar. The skript for that analysis can be found here: 
#              "https://github.com/ASanchez-Tojar/dominance".

# CODE SOURCES:
# Code chunks from Alfredo S??nchez-T??jar (see R-script "https://github.com/ASanchez-Tojar/dominance")
# Advice by Alfredo S??nchez-T??jar, Julia Schroeder and Mirre P. Simons 

# LAST TESTED
# 2016-08-21 with R version 3.0.2 (2013-09-25), Computing Platform: x86_64-apple-darwin10.8.0 (64-bit)

# PACKAGES USED
library(ggplot2) # used
library(rptR) # used
library(dplyr) # used
library(reshape) # used
library(stringr) # used
library(graphics) # used
library(arm) # used
library(lme4) # used

# Get ready
getwd()
setwd()
rm(list = ls())

##### Defyning theme for plots #####
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times', size=12),
        axis.text.x  = element_text(size=12),
        axis.text.y  = element_text(size=12),
        legend.title=element_blank())

####################################
# Part 1: Last live record dates from last live record query, deathdates and RFID data
###################################

##### Part 1.1: Source 1: database extraction from Database0.77_20160716_AS. Result: llr ######
# Data includes last live record from:
# C: Captures
# SP: Social Parents
# EP: Extra Pair Parents (Fathers)
# S: Sightings

llr <- read.csv("lastliverecord_dbextraction_20160721.csv")

# Before anything I've got to tell R that the column LastLiveRecord, CaptureDate and DeathDate
# are dates
#
#     This lct (seeting the C locale) is to avoid as.Date failing to recognize some of the month
#abbreviations (e.g. Aug)
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

llr$LastLiveRecord<-as.Date(llr$LastLiveRecord,format='%d-%b-%y')

##### Part 1.2: Source 2: database extraction from Database0.7X_20160705_AS with death dates. Result: llrDeath ######
# Data includes death dates for certain individuals

dr <- read.csv("deathdates_20160706.csv")

# Before anything I've got to tell R that the column LastLiveRecord, CaptureDate and DeathDate
# are dates
#
#     This lct (seeting the C locale) is to avoid as.Date failing to recognize some of the month
#abbreviations (e.g. Aug)
dr$DeathDate<-as.Date(dr$DeathDate,format='%d-%b-%y')
dr$LastLiveRecord<-as.Date(dr$LastLiveRecord,format='%d-%b-%y')

# remove all without death record
dr2 <- dr
dr2$DeathDate <- sub("^$", "0", dr2$DeathDate) # make all birds without deathdate 0

# subset only those which are not 0 (i.e. which have a deathdate)
dr2 <- subset(dr2, DeathDate != "0") 

# subset only those which died after October 2013 (after first dominance event)
dr3 <- subset(dr2, DeathDate >= "2013-11-01") 

# subset only those which fledged (because those dead in nest will not have interacted in the videos)
dr4 <- subset(dr3, LastStage==3)

# I need a uniqe identifier combining deathdate and birdID for merging it later
dr4$BirdID_DeathDate = paste(dr4$BirdID, dr4$DeathDate, sep="_")

# combine this with records on how they died because deathdate refers to the date found rather than the date they died
cc <- read.csv("allcaptures_dbextraction_20160721.csv")

# Before anything I've got to tell R that the column LastLiveRecord, CaptureDate and DeathDate
# are dates
#
#     This lct (seeting the C locale) is to avoid as.Date failing to recognize some of the month
#abbreviations (e.g. Aug)
cc$CaptureDate<-as.Date(cc$CaptureDate,format='%d-%b-%y')

# remove all without comments because we need to look at comments to see when they died
cc2 <- cc
cc2$Notes <- sub("^$", "0", cc2$Notes) # make all birds without comments 0

# subset only those which are not 0 (i.e. which have a deathdate)
cc2 <- subset(cc2, Notes != "0") 

# subset only those which fledged (because those dead in nest will not have interacted in the videos)
cc3 <- subset(cc2, Stage==3)

# subset only those which died after October 2013 (after first dominance event)
cc4 <- subset(cc3, CaptureDate >= "2013-11-01") 

# I need a uniqe identifier combining deathdate and birdID for merging it later
cc4$BirdID_DeathDate = paste(cc4$BirdID, cc4$CaptureDate, sep="_")

# Merging the two so that I have comments for each death
drcc <- merge(dr4,cc4,by="BirdID_DeathDate")

# Rename column BirdID.x to BirdID
drcc$BirdID <- drcc$BirdID.x

# Make new column with time between DeathDate and Last live record
drcc$llr_death <- difftime(drcc$DeathDate, drcc$LastLiveRecord, units = c("days"))

# drcc is now manually checked so that all death which where considerably before DeathDate can be detected
# first remove all unneded columns for better overview
drcc$CaptureRef <- NULL
drcc$BirdID.y <- NULL
drcc$BirdID.x <- NULL
drcc$CaptureTime<- NULL
drcc$Stage <- NULL
drcc$NestBoxRef <- NULL
drcc$LocationRef <- NULL
drcc$CaptureDate <- NULL

# 86 Birds to check

# Birds are excluded:
#  1) if more than 80 days between death date and last live record & comment for death date does not indicate recent death
#  2) if less than 80 days between death date and last live record & comment for death date does indicate death a long time ago

# Results in:
#  All birds with llr_death of >= 80 days are not freshly dead according to comments
# --> exclude them
drcc2 <- subset(drcc, llr_death <= 80) 

# At this stage, all the comments relating to the way the birds were found dead were checked manually
# All birds with lld_death of < 80 days are freshly dead with the exception of two birds (revealed by comments):
#  BirdID: 5616
#  BirdID: 7580
drcc3 <- subset(drcc2, BirdID!=5616)
drcc4 <- subset(drcc3, BirdID!=7580)

# 56 Birds remain. For these bird, DeathDate can now be regarded as last live record
# as they all died only shortly before they werd found death

# Additionally an odd one (Possible mistake, should be corrected in Database --> AST):
# 5424 has a last live record 4 days after the Death Date

# This is to make a new simple dataframe with Death Date, BirdID and Source
llrDeath <- drcc4[, c("BirdID", "DeathDate", "Source")]

# New source for Death Date is D=Death
llrDeath$Source <- sub("SP", "D", llrDeath$Source)
llrDeath$Source <- sub("EP", "D", llrDeath$Source)
llrDeath$Source <- sub("C", "D", llrDeath$Source)
llrDeath$Source <- sub("S", "D", llrDeath$Source)

# Because all birds in llrDeath died just before they were found dead, we consider DeathDate to be equal to a LastLiveRecord date
llrDeath <- rename(llrDeath, c("DeathDate"="LastLiveRecord"))

##### Part 1.3: Source 3: RFID data. Result: llrTransp ####
llrRFID <- read.table("rfid_combineddataset_rfidcomb3_20160722.txt", header = TRUE)

llrRFID3 <- llrRFID[,c("Date","Transponder.code")]

# Before anything I've got to tell R that the date is a date
#
#     This lct (seeting the C locale) is to avoid as.Date failing to recognize some of the month
#abbreviations (e.g. Aug)
llrRFID3$Date<-as.Date(llrRFID3$Date,format='%d-%m-%Y')

# select last date for each transponder
# code from http://stackoverflow.com/questions/26647801/r-subset-by-latest-date
llrRFID4 <- llrRFID3 %>% 
              group_by(Transponder.code) %>%
              mutate(Date=as.Date(Date, format= "%d-%m-%Y"))%>% 
              filter(Date==max(Date))

# This produces 129091 entries. This is because on last day, they went in and out repeatedly

# select only one row per transponder code
llrRFID5 <- llrRFID4[!duplicated(llrRFID4$Transponder.code),]

# Add column which says Source="T" for Transponder
llrRFID5$Source <- rep("T",nrow(llrRFID5))

# Get list with Transponder.code and BirdID to add to llrRFID5
# This one has all codes and BirdID's 
codes <- read.csv("allcodes_dbextraction_20160721.csv")

# I only need Transponder/BirdID's
tp <- subset(codes, CodeType=="T")

# Number of rows (i.e. Transponder codes) contained in TP
length(tp$BirdID)

# Number of BirdID's contained in TP
length(unique(tp$BirdID))
# Note that a Bird can have several PIT's over it's lifetime explaining this difference

# only need columns 3 & 4 = BirdID & Transponder code
tp2 <- tp[,c("BirdID","Code")]

# rename code to transponder code so that I have a common basis for merging
tp2 <- rename(tp2, c("Code"="Transponder.code"))

# Merging the two on criteria "Transponder.code"
llrRFID5_tp2 <- merge.data.frame(llrRFID5, tp2, by="Transponder.code", all.x = TRUE)

# 789 Transpondercodes do not have a fitting BirdID which is odd. Enquiry at transponder company on its way...
sum(is.na(llrRFID5_tp2$BirdID))
# Consequently, 328 "real" Transponders visited the feeder. We continue with these.

# Those 328 Transponders are owned by 317 birds (Unique BirdID's)
length(unique(llrRFID5_tp2$BirdID))

# remove all those rows with BirdID NA
llrRFID5_tp2_2 <- subset(llrRFID5_tp2,!(is.na(llrRFID5_tp2$BirdID)))

# 10 birds have had more than one RFID chip while visiting the feeder. 
# I'm only interested in the last live record, so I remove the row with the later date if BirdID is repeated 

# Set time correctly
llrRFID5_tp2_2$Date<-as.Date(llrRFID5_tp2_2$Date,format='%d-%m-%Y')

# select last date for each BirdID
# code from http://stackoverflow.com/questions/26647801/r-subset-by-latest-date
llrRFID5_tp2_3 <- llrRFID5_tp2_2 %>% 
  group_by(BirdID) %>%
  mutate(Date=as.Date(Date, format= "%d-%m-%Y"))%>% 
  filter(Date==max(Date))

# get rid of Transponder code
llrTransp <- llrRFID5_tp2_3[,c("BirdID","Date", "Source")]

# rename Date to LastLiveRecord for transponder (same as llr and llrDeath)
llrTransp <- rename(llrTransp, c("Date"="LastLiveRecord"))

##### Part 1.4: Sightings in Dominance Database. Result: llrDom ####
mdb <- read.csv("MegaDataBase-v97-201311-201606-FY-Dominance_Lundy_20160713.csv")

# take two subsamples so that date and each bird has an own row rather than having two birds in the same row
mdb2.1 <- mdb[,c("date","individual1","IDcertain1")]
mdb2.2 <- mdb[,c("date","individual2","IDcertain2")]

mdb2.1 <- rename(mdb2.1, c("individual1"="Code"))
mdb2.2 <- rename(mdb2.2, c("individual2"="Code"))

mdb2.1 <- rename(mdb2.1, c("IDcertain1"="IDcertain"))
mdb2.2 <- rename(mdb2.2, c("IDcertain2"="IDcertain"))

#putting the two subsamples back together
mdb2 <- rbind(mdb2.1, mdb2.2)

# subsetting all with IDcertain=yes
mdb3 <- subset(mdb2, IDcertain=="yes")

# subsetting all with notidentified
mdb4 <- subset(mdb3, Code!="notidentified")

# set date correctly 
mdb4$date <- as.Date(as.character(mdb4$date), "%Y%m%d")

# select last event for each Code / this is now only done in the very end with llrComplete
# code from http://stackoverflow.com/questions/26647801/r-subset-by-latest-date
mdb5 <- mdb4        %>% 
        group_by(Code) %>%
        mutate(date=as.Date(date, format= "%Y%m%d"))%>% 
        filter(date==max(date))

# Some have records of different Source on the same day
# select only one row per Code
mdb6 <- mdb5[!duplicated(mdb5$Code),]
mdb2.2 <- rename(mdb2.2, c("IDcertain2"="IDcertain"))

mdb6 <- rename(mdb6, c("date"="LastLiveRecord"))

# make codes upper case so that they mach those found in "codes"
mdb6$Code <- toupper(mdb6$Code)

# Get the birdID for each ring
# I only need Transponder/BirdID's
cr <- subset(codes, CodeType=="R")

length(cr$BirdID)
length(unique(cr$BirdID))

# only need columns BirdID & colour ring code
cr2 <- cr[,c("BirdID","Code")]

# Merging the two on criteria "Code"
mdb6_cr2 <- merge.data.frame(mdb6, cr2, by="Code", all.x = TRUE, fill=TRUE)

# 53 ColourCodes do not have a fitting BirdID which means these are mistakes
sum(is.na(mdb6_cr2$BirdID))

# 706 birds (Unique BirdID's)
length(unique(mdb6_cr2$BirdID))

# remove all those rows with BirdID NA
mdb6_cr2_2 <- subset(mdb6_cr2,!(is.na(mdb6_cr2$BirdID)))

# only need BirdID and LastLiveRecord
mdb6_cr2_3 <- mdb6_cr2[,c("BirdID", "LastLiveRecord")]

# Adding column for Source = DO
mdb6_cr2_3$Source <- rep("DO",nrow(mdb6_cr2_3))

# making llrDom from mdb6_cr2_3
llrDom <- mdb6_cr2_3

##### Part 1.5: Observations in in-and-out Database for first video. Result llrIo #####
Io <- read.table("in_and_out_dataset_iocomb_20160722.txt",header=TRUE,sep='\t')

Io2 <- rename(Io, c("Idcertain"="IDcertain"))

# Note that this dataset needs intensive error-checking as it is raw-data, this is what all the following code is for!!!

# Checking that all colour codes have 5 characters (4 letter + the /)
#       Keep in search of typos. Excluding, of course, the notidentified which have 13 characters and 
# are of not interest.
Io2$individual1 <- as.character(Io2$individual1)

for (i in 1:nrow(Io2)){
  if(Io2$individual1[i]!="notidentified" & 
     Io2$individual1[i]!="not identified" & 
     Io2$individual1[i]!="starling" &
     Io2$individual1[i]!="crow" &
     Io2$individual1[i]!="chaffinch" &
     Io2$individual1[i]!="blackbird"){
    
    if(nchar(Io2$individual1[i])!=5){
      
      print("Check this row for Codes < or > than 5 expected characters") #it prints the line so that you can go and check yourself
      print(i+1)
    }
    
  } 
}

# all individual1 have now 5 letters

# I delete those rows with other species and notidentified, makes it easier
Io3 <- subset(Io2, individual1!="notidentified" &
                  individual1!="robin" & 
                  individual1!="not identified" & 
                  individual1!="starling" &
                  individual1!="crow" &
                  individual1!="chaffinch" &
                  individual1!="blackbird")

#  Checking that any code contains two "m" (metal rings)

for (i in 1:nrow(Io3)){
  
  if(str_count(Io3$individual1[i],"m")>1){
    
    print("Check this row for Codes for > 1 metal ring") #it prints the line so that you can go and check yourself
    print(i+1)
    
  }
}

# all birds have only one metalring 

# subsetting all with IDcertain=yes
Io4 <- subset(Io3, IDcertain=="yes")

# make one date categorie from the separat day, month, year columns 
Io4$Date <- as.Date(paste(Io4$day, Io4$month, Io4$year, sep = "-" )  , format = "%d-%m-%Y" )

# select last event for each Code. This in now only done in the end with llrComplete
# code from http://stackoverflow.com/questions/26647801/r-subset-by-latest-date
Io5 <- Io4 %>% 
  group_by(individual1) %>%
  mutate(Date=as.Date(Date, format= "%Y%m%d"))%>% 
  filter(Date==max(Date))

# Many obviously occur on same event day have records of different Source on the same day
# select only one row per Code
Io6 <- Io5[!duplicated(Io5$individual1),]

# rename individual1 to Code
Io7 <- rename(Io6, c("individual1"="Code"))

# make codes upper case so that they mach those found in "codes"
Io7$Code <- toupper(Io7$Code)

# get rid of all the unneeded stuff
Io8 <- Io7[,c("Date","Code")]

# Get the birdID for each ring from codes (used previously)
# I only need Transponder/BirdID's
cr <- subset(codes, CodeType=="R")

length(cr$BirdID)
length(unique(cr$BirdID))

# only need columns BirdID & colour ring code
cr2 <- cr[,c("BirdID","Code")]

# Merging the two on criteria "Code"
Io8_cr2 <- merge.data.frame(Io8, cr2, by="Code", all.x = TRUE, fill=TRUE)

# 35 ColourCodes do not have a fitting BirdID which means these are mistakes
sum(is.na(Io8_cr2$BirdID))

# 561 birds (Unique BirdID's)
length(unique(Io8_cr2$BirdID))

# remove all those rows with BirdID NA
Io8_cr2_2 <- subset(Io8_cr2,!(is.na(Io8_cr2$BirdID)))

# rename date to LastLiveRecord
Io8_cr2_2 <- rename(Io8_cr2_2, c("Date"="LastLiveRecord"))

#only need BirdID and LastLiveRecord
Io8_cr2_3 <- Io8_cr2_2[,c("BirdID", "LastLiveRecord")]

# Adding column for Source = IO
Io8_cr2_3$Source <- rep("IO",nrow(Io8_cr2_3))

# making llrIo from mdb6_cr2_3
llrIo <- Io8_cr2_3


####################################
# Part 2: Combining yuhu database with last live record data
###################################

##### This leads to llrComplete4: Day of LastLiveRecord, BirdID and Source for birds present in yuhu database ####
# note: llrDeath dates from death records count now as last live records as they were alive just before found
# hence, llrDeath dates for last live records are all later than last live records from llr

# putting all three llr files together
llrComplete <- rbind(llr, llrDeath, llrTransp, llrDom, llrIo)

summary(llrComplete$Source)

# loading yuhu databae
yuhu <- read.csv("yuhu.csv")
  
# Get all BirdID's which occur in "yuhu" database twice (one for merging, one to keep, little workaround)
yuhu_BirdID <- yuhu[,c("BirdID","BirdID")]

# merge yuhu_BirdID's to llrComplete2
llrComplete2 <- merge(yuhu_BirdID, llrComplete, by="BirdID", fill=TRUE)

length(unique(llrComplete2$BirdID))

# Get rid of temporary variable "BirdID.1"
llrComplete2$BirdID.1 <- NULL

# This summary is the number of total records for birds in the yuhu 
summary(llrComplete2)

# This is to count and get sd for number of unique resightings per individual
BirdID <- as.vector(llrComplete2$BirdID)
a <- rle(sort(BirdID))
a
llrComplete2COUNT <- data.frame(number=a$values, n=a$lengths)

min(llrComplete2COUNT$n)
max(llrComplete2COUNT$n)
sd(llrComplete2COUNT$n)
mean(llrComplete2COUNT$n)

# select "last" LastLiveRecord for each BirdID
# code from http://stackoverflow.com/questions/26647801/r-subset-by-latest-date
llrComplete3 <- llrComplete2 %>% 
  group_by(BirdID) %>%
  mutate(LastLiveRecord=as.Date(LastLiveRecord, format= "%Y-%m-%d"))%>% 
  filter(LastLiveRecord==max(LastLiveRecord))

# This summary is the number of LLR from different sources for birds in yuhu with duplicated days 
summary(llrComplete3$Source)

# Some have records of different Source on the same day
# select only one row per BirdID code
llrComplete4 <- llrComplete3[!duplicated(llrComplete3$BirdID),]

# This summary is the number of LLR from different sources for birds in yuhu with only one LLR per bird 
# Note, however that this still contains those 63 birds from S2016 which are excluded later on
summary(llrComplete4$Source)

length(unique(llrComplete4$BirdID))


####################################
# Part 3: Converts Last live record date for each BirdID into Season when last recorded (Summer/Winter)
###################################

#### Add column to llrComplete4 which states the Season with the Last Live Record per BirdID #####
# set LastLiveRecord as POSICct time format
llrComplete4$LastLiveRecord <- as.POSIXct(llrComplete4$LastLiveRecord, format("%Y-%m-%d"))

# set LastLiveRecord as Date time format
llrComplete4$LastLiveRecord <- as.Date(llrComplete4$LastLiveRecord, format("%Y-%m-%d"))
 
# Add events instead of date for season
 for(i in 1:nrow(llrComplete4)){
    if(llrComplete4$LastLiveRecord[i] < as.Date("2014-03-01")){
      llrComplete4$Season[i] <- "2013.5"
      } else{
        
    if(llrComplete4$LastLiveRecord[i] > as.Date("2014-02-28") &
       (llrComplete4$LastLiveRecord[i] < as.Date("2014-09-01"))){
          llrComplete4$Season[i] <- "2014"    
      } else{  
        
        if(llrComplete4$LastLiveRecord[i] > as.Date("2014-08-31") &
           (llrComplete4$LastLiveRecord[i] < as.Date("2015-03-01"))){
          llrComplete4$Season[i] <- "2014.5"    
        } else{        
        
          if(llrComplete4$LastLiveRecord[i] > as.Date("2015-02-28") &
             (llrComplete4$LastLiveRecord[i] < as.Date("2015-09-01"))){
            llrComplete4$Season[i] <- "2015"    
          } else{ 
            
            if(llrComplete4$LastLiveRecord[i] > as.Date("2015-08-31") &
               (llrComplete4$LastLiveRecord[i] < as.Date("2016-03-01"))){
              llrComplete4$Season[i] <- "2015.5"    
            } else{ 
              
              if(llrComplete4$LastLiveRecord[i] > as.Date("2016-02-28") &
                 (llrComplete4$LastLiveRecord[i] < as.Date("2016-09-01"))){
                llrComplete4$Season[i] <- "2016"    
              } else{ 
            
        llrComplete4$Season[i] <- "test"  # this is just to see if any are missed
      }
     }
    }
   }
  }
 }
}


####################################
# Part 4: Puts together Yuhu database and llrComplete 4 based on BirdID to yuhu_llr
###################################

##### Part 4.1: produces 0/1 values for survival to next season per bird per event with ranking ####

yuhu_llr <- merge(yuhu, llrComplete4, by="BirdID", fill=TRUE)

# add new column "Dead" 1 = dead, 0 = alive
yuhu_llr$Season <- as.numeric(yuhu_llr$Season)
yuhu_llr$eventSW <- as.numeric(yuhu_llr$eventSW)

# if season with llr is > eventSW, than bird was still alive following season
yuhu_llr$Dead <- (yuhu_llr$Season - yuhu_llr$eventSW) 

# make all > 0 to 1
yuhu_llr$Dead[yuhu_llr$Dead > 0] <- 10
yuhu_llr$Dead[yuhu_llr$Dead == 0] <- 1
yuhu_llr$Dead[yuhu_llr$Dead == 10] <- 0

##### Part 4.2: Adds additional column season (SW) to do analysis on winter vs. summer mortality ####

yuhu_llr$SW <- yuhu_llr$eventSW
yuhu_llr$SW[yuhu_llr$SW == 2013.5] <- "0"
yuhu_llr$SW[yuhu_llr$SW == 2014] <- "1"
yuhu_llr$SW[yuhu_llr$SW == 2014.5] <- "0"
yuhu_llr$SW[yuhu_llr$SW == 2015] <- "1"
yuhu_llr$SW[yuhu_llr$SW == 2015.5] <- "0"
yuhu_llr$SW[yuhu_llr$SW == 2016] <- "1"
yuhu_llr$SW <- as.numeric(yuhu_llr$SW )

##### Part 4.4: kicks out all observations from Summer 2016 because we do not yet have resighting data! Result: forsurv #####

# However, all which interacted in summer 2016 are "dead" now because there is no data yet 
# for winter 2016/2017. Hence, we can not use summer 2016 interactions which is why we kick them out

forsurv <- subset(yuhu_llr, yuhu_llr$eventSW!=2016.0)

##### Part 4.5: Add column "Alive" to forsurv (just opposite of "Dead") #####
# add column
forsurv$Alive <- forsurv$Dead

# make all 0 -> 1 and all 1 -> 0
forsurv$Alive[forsurv$Alive > 0] <- 10
forsurv$Alive[forsurv$Alive == 0] <- 1
forsurv$Alive[forsurv$Alive == 10] <- 0

##### Part 4.6: Add sex "0" (m) for wo/dm = BirdID 7102 (mistake in Lundy Database, should be changed there too) ####
forsurv$SexEstimate[forsurv$BirdID==7102] <-  0


# forsurv is now ready for survival analysis!!!!!!!!!!


####################################
# Part 5: Subsetting, overview metrics & Repeatability
###################################

##### Part 5.1 Subsetting forsurv into age classes (all, juv+first winter, juv, first winter, adults, first winter + adults #####

# Juvenile and first winter
forsurv_j <- subset(forsurv, forsurv$age < 1)

# first winter
forsurv_jj <- subset(forsurv, forsurv$age == 0.5)

# Juvenile 
forsurv_jjj <- subset(forsurv, forsurv$age == 0)

# adult 
forsurv_a <- subset(forsurv, forsurv$age > 0.5)

# first winter + adults
forsurv6.3 <- forsurv
forsurv6.3 <- subset(forsurv6.3, forsurv6.3$age > 0.2)

##### Part 5.2 Overview metrics forsurv6.3 (without Juveniles) ::: USED IN THESIS #####

# This is a subset to get one record per bird (without juveniles) to have only one llr per bird so that sources can be counted
forsurv6.3_uniqueBirdID <- subset(forsurv6.3, !duplicated(forsurv6.3$BirdID))

# Overview forsurv_uniqueBirdID sources as shown in Thesis
summary(forsurv6.3_uniqueBirdID$Source)

# Number of females in forsurv6.3
length(which(forsurv6.3_uniqueBirdID$SexEstimate==0))

# number of males in forsurv6.3
length(which(forsurv6.3_uniqueBirdID$SexEstimate==1))

# number of birds of unknown sex in forsurv6.3
length(forsurv6.3_uniqueBirdID$SexEstimate)-
  length(which(forsurv6.3_uniqueBirdID$SexEstimate==0))-
  length(which(forsurv6.3_uniqueBirdID$SexEstimate==1))

# Number of birds present in forsurv6.3
length(unique(forsurv6.3_uniqueBirdID$BirdID))  # 262 Birds

# Mean age for all birds in forsurv6.3
mean(forsurv6.3_uniqueBirdID$age)

##### Part 5.3: Counting number of individuals per event (=number of observations) ::: OUTPUT IN THESIS (Flowchart) ####
forsurv_uniqueBirdID_2013.5 <- subset(forsurv_uniqueBirdID, forsurv_uniqueBirdID$eventSW==2013.5)

forsurv_uniqueBirdID_2014 <- subset(forsurv_uniqueBirdID, forsurv_uniqueBirdID$eventSW==2014)

forsurv_uniqueBirdID_2014.5 <- subset(forsurv_uniqueBirdID, forsurv_uniqueBirdID$eventSW==2014.5)

forsurv_uniqueBirdID_2015 <- subset(forsurv_uniqueBirdID, forsurv_uniqueBirdID$eventSW==2015)

forsurv_uniqueBirdID_2015.5 <- subset(forsurv_uniqueBirdID, forsurv_uniqueBirdID$eventSW==2015.5)

##### Part 5.4: Repeatability of Elo-Rating for those birds in forsurv ::: OUTPUT IN THESIS ####
rpt.St.MCMCforsurv <- rpt(forsurv$StElo,
                   forsurv$BirdID,
                   datatype="Gaussian",
                   method="MCMC",
                   nboot=1000,
                   npermut=1000)

rpt.St.MCMCforsurv

# Number of birds with more than one event in forsurv (this is N for the repeatability)
forsurv_count <- forsurv[duplicated(forsurv$BirdID),]
forsurv_count <- forsurv[unique(forsurv_count$BirdID),]
length(forsurv_count$BirdID)


####################################
# Part 6: Survival model with binomial response variable
###################################

##### Part 6.1: Using all birds but juveniles with age first winter vs. rest as an extra variable ::: OUTPUT IN THESIS APPENDIX#####

# new variable agetwo for forsurv6.3 which is age in two categories (first-winter vs. all older ones)
forsurv6.3$agetwo <- forsurv6.3$age

# do the classification
forsurv6.3$agetwo[forsurv6.3$agetwo > 0.6] <- 1
forsurv6.3$agetwo[forsurv6.3$agetwo == 0.5] <- 0

# actual model 
survmod6.3 <-  glmer(Alive ~ 
                              scale(StElo)*
                              scale(agetwo)+
                              scale(age)+
                              scale(SexEstimate)+
                              scale(SW)+
                              I(scale(StElo)^2)+
                              I(scale(age)^2) + 
                              (1 | eventSW) + 
                              (1 | BirdID)
                            , data=forsurv6.3, family = binomial) 
summary(survmod6.3)
display(survmod6.3)

# getting mean and CrI from the bayesian model for fixed effects
smod1<-sim(survmod6.3,1000) #simulating 1000 times, no prior information

apply(smod1@fixef,2, mean)
apply(smod1@fixef,2, quantile, c(0.025, 0.975))

# getting mean and CrI from the bayesian model for random effects
round(mean(apply(smod1@ranef$BirdID,1, var)),3)
round(quantile(apply(smod1@ranef$BirdID,1, var),c(0.025, 0.975)),3)

##### Part 6.2: Using all birds but juveniles with age first winter vs. rest as an extra variable; without squared terms ::: OUTPUT IN THESIS #####
# actual model 
survmod6.3.2 <-  glmer(Alive ~ 
                       scale(StElo)*
                       scale(agetwo)+
                       scale(age)+
                       scale(SexEstimate)+
                       scale(SW)+
                     # I(scale(StElo)^2)+
                     # I(scale(age)^2) + 
                       (1 | eventSW) + 
                       (1 | BirdID)
                     , data=forsurv6.3, family = binomial) 
summary(survmod6.3.2)
display(survmod6.3.2)

# getting mean and CrI from the bayesian model for fixed effects
smod1<-sim(survmod6.3.2,1000) #simulating 1000 times, no prior information

apply(smod1@fixef,2, mean)
apply(smod1@fixef,2, quantile, c(0.025, 0.975))

# getting mean and CrI from the bayesian model for random effects
round(mean(apply(smod1@ranef$BirdID,1, var)),3)
round(quantile(apply(smod1@ranef$BirdID,1, var),c(0.025, 0.975)),3)

##### Part 6.3: Using only first winter birds ::: OUTPUT IN THESIS APPENDIX ####
# note that age and birdID are not needed as all birds have the same age and each bird can only have that age once
survmod3 <- glmer(Alive ~ 
                    scale(StElo)+
                    I(scale(StElo)^2)+
                    scale(SexEstimate) +
                    (1 | eventSW) 
                    , data=forsurv_jj, family = binomial) 
summary(survmod3)
display(survmod3)

# getting mean and CrI from the bayesian model for fixed effects
smod1<-sim(survmod3,1000) #simulating 1000 times, no prior information

apply(smod1@fixef,2, mean)
apply(smod1@fixef,2, quantile, c(0.025, 0.975))

# getting mean and CrI from the bayesian model for random effects
round(mean(apply(smod1@ranef$BirdID,1, var)),3)
round(quantile(apply(smod1@ranef$BirdID,1, var),c(0.025, 0.975)),3)

##### Part 6.4: Using only first winter birds without squared term StElo^2::: OUTPUT IN THESIS ####
# note that age and birdID are not needed as all birds have the same age and each bird can only have that age once
survmod3.2 <- glmer(Alive ~ 
                    scale(StElo)+
                    #I(scale(StElo)^2)+
                    scale(SexEstimate) +
                    (1 | eventSW) 
                  , data=forsurv_jj, family = binomial) 
summary(survmod3.2)
display(survmod3.2)

# getting mean and CrI from the bayesian model for fixed effects
smod1<-sim(survmod3.2,1000) #simulating 1000 times, no prior information

apply(smod1@fixef,2, mean)
apply(smod1@fixef,2, quantile, c(0.025, 0.975))

# getting mean and CrI from the bayesian model for random effects
round(mean(apply(smod1@ranef$BirdID,1, var)),3)
round(quantile(apply(smod1@ranef$BirdID,1, var),c(0.025, 0.975)),3)

####################################
# Part 7: Odd ratios and plots shown in Thesis
####################################
# http://www.ats.ucla.edu/stat/mult_pkg/faq/general/odds_ratio.htm
# http://www.ats.ucla.edu/stat/r/dae/logit.htm

##### Part 7.2: First Winter Survival odds ####
# making dataframe for this analysis
forsurv_jjodds2 <- forsurv_jj

# adding AlivePlot for nice representation in plot (adds 0.05 to dead and substracts 0.05 from alive)
forsurv_jjodds2$AlivePlot <- forsurv_jjodds2$Alive

forsurv_jjodds2$AlivePlot[forsurv_jjodds2$AlivePlot == 0] <- 0.01
forsurv_jjodds2$AlivePlot[forsurv_jjodds2$AlivePlot == 1] <- 1.01

# this is the glm on which the plot is based on
Jglmodds <- glm(Alive  ~ scale(StElo), data = forsurv_jjodds2, family = "binomial")

summary(Jglmodds)

# calculating average elo rating per eventSW
JDFodds <- with(forsurv_jjodds2, data.frame(StElo = mean(StElo)))

# predict response with model
JDFodds$StEloP <- predict(Jglmodds, newdata = JDFodds, type = "response")

# putting data together
JDFodds2 <- with(forsurv_jjodds2,
                data.frame(StElo = rep(seq(from = 0, to = 1, length.out = 100), 3)))

# simulating odds based on glmodds
JDFodds3 <- cbind(JDFodds2, predict(Jglmodds, newdata = JDFodds2, type="link", se=TRUE))

# adding confidence intervals for predicted values
JDFodds3 <- within(JDFodds3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# Plot the odd ratios
ggplot(JDFodds3, aes(x = StElo, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .2) +
  geom_line(aes(x = StElo, y = PredictedProb)) + 
  apatheme

##### Part 7.3: All other ages Winter Survival odds ####
# making dataframe for this analysis
forsurv_oddsOld <- subset(forsurv, forsurv$SW=="0" & forsurv$age!="0.5")

# adding AlivePlot for nice representation in plot (adds 0.05 to dead and substracts 0.05 from alive)
forsurv_oddsOld$AlivePlot <- forsurv_oddsOld$Alive

forsurv_oddsOld$AlivePlot[forsurv_oddsOld$AlivePlot == 0] <- -0.01
forsurv_oddsOld$AlivePlot[forsurv_oddsOld$AlivePlot == 1] <- 0.99

# this is the glm on which the plot is based on
Oldglmodds <- glm(Alive  ~ scale(StElo), data = forsurv_oddsOld, family = "binomial")
summary(Oldglmodds)

# calculating average elo rating per eventSW
OldDFodds <- with(forsurv_oddsOld, data.frame(StElo = mean(StElo)))

# predict response with model
OldDFodds$StEloP <- predict(Oldglmodds, newdata = OldDFodds, type = "response")

# putting data together
OldDFodds2 <- with(forsurv_oddsOld,
                 data.frame(StElo = rep(seq(from = 0, to = 1, length.out = 100), 3)))

# simulating odds based on glmodds
OldDFodds3 <- cbind(OldDFodds2, predict(Oldglmodds, newdata = OldDFodds2, type="link", se=TRUE))

# adding confidence intervals for predicted values
OldDFodds3 <- within(OldDFodds3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# Plot the odd ratios
ggplot(OldDFodds3, aes(x = StElo, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .2) +
  geom_line(aes(x = StElo, y = PredictedProb)) +
  apatheme

##### Part 7.4: Combined plot from survival odds from part 7.2 and 7.3 ::: USED IN THESIS#####
# This is for the legend
cols <- c("Older individuals"="OrangeRed4","First Winter"="SlateGrey")

# This is the actual plot
supiplot=ggplot() +
  geom_ribbon(data = OldDFodds3, aes(x = StElo, y = PredictedProb, ymin = LL, ymax = UL, fill="Older individuals"), alpha = .2) +
  geom_line(data = OldDFodds3, aes(x = StElo, y = PredictedProb, colour="Older individuals"))+
  
  geom_ribbon(data = JDFodds3, aes(x = StElo, y = PredictedProb, ymin = LL, ymax = UL, fill="First Winter"), alpha = .2) +
  geom_line(data = JDFodds3, aes(x = StElo, y = PredictedProb, colour="First Winter"))+
  
  geom_point(aes(forsurv_oddsOld$StElo, forsurv_oddsOld$AlivePlot, colour="Older individuals"), alpha = 0.6)+    #add data points for old birds
  geom_point(aes(forsurv_jjodds2$StElo, forsurv_jjodds2$AlivePlot, colour="First Winter"), alpha = 0.6)+    #add data points for 1st winter birds

  ylab("Predicted Survival Probability") +
  scale_y_continuous(labels = c("0"="Dead", "0.25"="0.25", "0.5"="0.5", "0.75"="0.75", "1"="Alive"), breaks=c(0,0.25,0.5,0.75,1))+
  xlab("Social Status")+
  
  scale_colour_manual(name="Legend",values=cols)+
  scale_fill_manual(name="Legend",values=cols) +
  apatheme

supiplot

supiplot+ggsave("supiplot.pdf", width=7, height=5)

####################################
# Part 8: Counting number of breeding pairs in study period
###################################

#### Part 8.1: Loading file, selecting only certain ID's ####
broods <- read.csv("broods_2013-2016_plusyear.csv")

broods2 <- subset(broods, broods$SocialMumCertain==TRUE)

#### Part 8.2: Subset per year and only one observation per BirdID ####
broods2013 <- subset(broods2, broods2$year=="M")
broods2013_uniqueBirdID <- subset(broods2013, !duplicated(broods2013$SocialMumID))

broods2014 <- subset(broods2, broods2$year=="N")
broods2014_uniqueBirdID <- subset(broods2014, !duplicated(broods2014$SocialMumID))

broods2015 <- subset(broods2, broods2$year=="O")
broods2015_uniqueBirdID <- subset(broods2015, !duplicated(broods2015$SocialMumID))

broods2016 <- subset(broods2, broods2$year=="P")
broods2016_uniqueBirdID <- subset(broods2016, !duplicated(broods2016$SocialMumID))

#### Part 8.3: Counting number of females as a proxy for number of breeding pairs ::: OUTPUT IN THESIS ####
length(broods2013_uniqueBirdID$SocialMumID)

length(broods2014_uniqueBirdID$SocialMumID)

length(broods2015_uniqueBirdID$SocialMumID)

length(broods2016_uniqueBirdID$SocialMumID)

# Mean 2013-2016
((length(broods2013_uniqueBirdID$SocialMumID)+ 
  length(broods2014_uniqueBirdID$SocialMumID)+ 
  length(broods2015_uniqueBirdID$SocialMumID)+
  length(broods2016_uniqueBirdID$SocialMumID))/4)

se(length(broods2013_uniqueBirdID$SocialMumID), 
     length(broods2014_uniqueBirdID$SocialMumID), 
     length(broods2015_uniqueBirdID$SocialMumID),
     length(broods2016_uniqueBirdID$SocialMumID))
