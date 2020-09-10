##########################################################
# Running the WAFT fish ticket checker
# to look at landings 
# Created by: Katelyn Bosley
# Date: 10/8/2019
# Updated 12/30/2019
###########################################################

# This code is to replace the clunky spreadsheet set_up that Don R had for fish ticket reconciliation
# and it also works up the CPUE based on fish tickets

#load libraries
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(openxlsx)
library(imputeTS)

###########################################################
{
#read in data
raw<-read.csv("./Input/PS Crab.csv", skip=9,header = T)
head(raw)
sapply(raw,class)

#run the code

#enter the raw data
#raw$Catch_Area_Code<-str_replace(raw$Catch_Area_Code," ", "")
raw$Catch_Area_Code<-trimws(raw$Catch_Area_Code, "r")

###########################################################
# Pull out only state commercial data. 
raw <- raw%>%
  filter(Fisher_Type_Code == "1")

# Enter the regions table
#reg<-read.csv("./Input/Region_Area.csv")
#head(reg)
#sapply(reg,class)
#names(reg)[1]<-"Catch_Area_Code"

###########################################################
# merge all the data 
raw$Catch_Area_Code<-paste0(raw$Catch_Area_Code,raw$Split_SubUnit_Code)

# regions <- readxl::read_excel("./Input/lu_Harvest_Areas.xlsx")

full.data <- left_join(raw, regions, by = c("Catch_Area_Code"="Sub-area")) # this is kicking errors

full.data[which(is.na(full.data$Region)),]

table(full.data$Region,full.data$`Region-SubArea`)

#change the 2 to 2E
#area2<-which(full.data$Region==2 & full.data$Split_SubUnit_Code=="E")
#area3<-which(full.data$Region==2 & full.data$Split_SubUnit_Code=="W")

#look to see what area these 2 values are
#full.data[area2,]
#full.data$Region[area2]="2E"
#full.data$Region[area3]="2W"

full.data$Landing_Date_2 = as.Date(full.data$Landing_Date, format = "%m-%d-%Y")

#summarize by date
daily_sum_save <- full.data%>%
  group_by(Landing_Date_2,Region)%>%
  summarize(Total_Pounds = sum(Round_Lbs_Qty))%>%complete(Landing_Date_2)

daily_FT_area_save <- spread(daily_sum_save,Region,Total_Pounds, fill = 0)

write.xlsx(daily_FT_area_save, paste("./Output/", "Daily_FT","-", Sys.Date(),".xlsx", sep = ""), 
           row.names = FALSE)

#calculate the CPUE
daily_cpue<-full.data%>%
  group_by(Landing_Date_2,License_ID, Region)%>%
  summarize(Total_Pounds = sum(Round_Lbs_Qty))

daily_cpue_FT<-full.data%>%
  group_by(Landing_Date_2,License_ID,Fish_Ticket_Num,Region)%>%
  summarize(Total_Pounds = sum(Round_Lbs_Qty),
            N=length(License_ID))

write.xlsx(daily_cpue_FT, paste("./Output/", "Daily_CPUE_",Sys.Date(),".xlsx", sep = ""), 
           row.names = FALSE)


#################################################################################################
# create the full landing summary for website
#################################################################################################

#pull FT data and fill NA with 0
a <- daily_FT_area_save[,1:7]
colnames(a)[1]<-"LandDate"
a$LandDate <- as.Date(a$LandDate)
ts = data.frame(LandDate=seq.Date(min(a$LandDate), max(a$LandDate), by="day"))
FT_sub_temp <- merge(a,ts,all=T,fill=0)%>%
  na_replace()

#pull the QR data and fill the NA with 0
b<- QR.region.summary
b2 <- b[,1:7]
b2$LandDate <- as.Date(b2$LandDate)

#complete the date timeseries enteries and put in order
qr_temp <- b2%>%
  arrange(LandDate)

#fill the empty rows with zero
# qr_temp <- qr_temp[-which(is.na(qr_temp$LandDate)),]

qr_temp<-na_replace(qr_temp)

#subset the FT and QR ata from based on the ndays input initially 
FT_sub<-subset(FT_sub_temp,LandDate<Sys.Date() - ndays)
QR_sub<-subset(qr_temp,LandDate>=Sys.Date()- ndays)

#combine into a single data frame
daily_landings_complete<-rbind(FT_sub,QR_sub)

#print!
write.xlsx(daily_landings_complete, paste("./Output/", "Daily_Landings_Complete","_", Sys.Date(),".xlsx", sep = ""), row.names = FALSE)
}
