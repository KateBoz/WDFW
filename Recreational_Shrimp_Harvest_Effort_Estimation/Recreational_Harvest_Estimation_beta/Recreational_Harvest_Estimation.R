##############################################################################
# Developing a program to run catch estimates for recreational shrimp fishery
#
# Created By: Katelyn Bosley
# Date: 7/21/2020
#############################################################################

rm(list = ls())


#load libraries for the program
load_libraries<-function() {
  library(data.table)
  library(ggplot2)
  library(reshape2)
  library(gridExtra)
  library(gplots)
  library(colorspace)
  library(RColorBrewer)
  library(dplyr)
  library(matrixStats) 
  library(gridExtra)
  library(grid)
  library(tidyr)
  library(png)
  library(MASS)
  library(chron)
  library(stringr)
  library(lubridate)
  library(readxl)
  library(openxlsx)
}
load_libraries()



###################################################################
# Start by entering any input values that are needed for the code
###################################################################

#enter any input values for running the code
Year<-2020

#input the shrimp weight in lbs.
mean_shrimp_wt<-0.078

#enter the Region
area<- "1"

#Areas for reference
# "1"      "2 East" "2 West" "3"      "4"      "6" 

#enter the shrimp quota area
SQA<-"7 West (1A)"

# SQA for reference
# "7 East (1B)"             "7 West (1A)"             "6 - 23AC.23B"            "7 South"              
# "6 - 23AS.23D"            "2 East"                  "6 - 25A outside Disco"   "Discovery Bay District" 
# "2 West"                  "10 - outside Elliot Bay" "10 - Elliot Bay"         "26D"  


#Choose report out by region or spot_quota area
#Use 1 for areas without mulitple SQAs and 2 for Region with many SQA 
report_out<-2
  ## 1 == Region(area)
  ## 2 == Spot Quota Area

#Using a boat or buoy count for the expansion factor?
effort_type<-2
  ## 1 == Boat count
  ## 2 == Buoy count

survey_type<-1
## 1 == aerial survey
## 2 == vessel survey 


#rec.estimate.run(survey_type,effort_type,report_out,SQA,area,mean_shrimp_wt,Year)
#run the program
#rec.estimate.run<-function(survey_type=survey_type,effort_type=effort_type,report_out=report_out,SQA=SQA,area=area,mean_shrimp_wt=mean_shrimp_wt,Year=Year){

{
#############################################################
# Enter the data 
#############################################################

#using the project function you don't need to set up the working directory. Simply load the file you need. 

#enter the creel data
creel_dat<-read_xlsx(paste0(Year," Shrimp Creel Data outside HC Complete.xlsx"))
head(creel_dat)
sapply(creel_dat,class)

#format the time and date correctly
creel_dat$Time_Interviewed<-format(creel_dat$Time_Interviewed,format="%H:%M:%S") #adjust time format
creel_dat$Time_Pots_Started_Soaking<-format(creel_dat$Time_Pots_Started_Soaking,format="%H:%M:%S")
creel_dat$Time_Last_Pot_Pulled<-format(creel_dat$Time_Last_Pot_Pulled,format="%H:%M:%S")
creel_dat$Date<-format(creel_dat$Date,format="%m/%d/%Y") #adjust date format


#enter the aerial count data
count_dat<-read_xlsx(paste0("Extract_Shrimp_",Year,".xlsx"))
head(count_dat)
sapply(count_dat,class)

#format time and date correctly
count_dat$Obs_Date<-format(count_dat$Obs_Date,format="%m/%d/%Y")  #adjust time format
count_dat$Time_2<-format(count_dat$Time_2,format="%H:%M:%S")
count_dat$Date<-format(count_dat$Obs_Date,format="%m/%d/%Y") #adjust date format


#enter the vessel count data
vessel_count_dat<-read_xlsx(paste0("Boat_Bouy_Counts_",Year,"_final.xlsx"))
vessel_count_dat$Date<-format(vessel_count_dat$Date,format="%m/%d/%Y")  #adjust time format
vessel_count_dat$Observation_Start_Time<-format(vessel_count_dat$Observation_Start_Time,format="%H:%M:%S")
vessel_count_dat$Observation_End_Time<-format(vessel_count_dat$Observation_End_Time,format="%H:%M:%S")


#enter the area converter
area_table<-read_xlsx("Shrimp Creel Area Translation Table.xlsx")
head(area_table)

area_table_2<-(area_table[2:3])

#create a table of distinct quota areas
area_table_2<-area_table_2 %>% distinct()

##############################################################################################
# Now that the data is all entered and nice, add the Spot Quota Areas to the boat/buoy counts
##############################################################################################


#merge the areas into each creel data.frame
creel_dat2<-merge(creel_dat, area_table, by = "Creel_Area") # Includes all rows
head(creel_dat2)
dim(creel_dat)
dim(creel_dat2)

#add day of week and weekend/weekday
creel_dat2$weekday<-weekdays(creel_dat2$Date)
creel_dat2$WDWE<-creel_dat2$weekday

creel_dat2<-creel_dat2 %>% mutate(WDWE = recode(WDWE,'Mon'="Weekday",
                                                'Tue'="Weekday",
                                                'Wed'="Weekday",
                                                'Thu'="Weekday",
                                                'Fri'="Weekday",
                                                'Sat'="Weekend",
                                                'Sun'="Weekend"))


#merge the areas into aerial count data.frame
count_dat2<-merge(count_dat, area_table_2, by = "Spot_Quota_Area") # Includes all rows
head(count_dat2)
dim(count_dat)
dim(count_dat2)

#add day of week and weekend/weekday
count_dat2$weekday<-weekdays(count_dat2$Date)
count_dat2$WDWE<-count_dat2$weekday

count_dat2<-count_dat2 %>% mutate(WDWE = recode(WDWE,'Mon'="Weekday",
                      'Tue'="Weekday",
                      'Wed'="Weekday",
                      'Thu'="Weekday",
                      'Fri'="Weekday",
                      'Sat'="Weekend",
                      'Sun'="Weekend"))


#merge the areas into vessel count data.frame
vessel_count_dat2<-merge(vessel_count_dat, area_table, by ="Creel_Area") # Includes all rows

head(vessel_count_dat2)
dim(vessel_count_dat)
dim(vessel_count_dat2)

#add day of week and weekend/weekday
vessel_count_dat2$weekday<-weekdays(vessel_count_dat2$Date)
vessel_count_dat2$WDWE<-vessel_count_dat2$weekday

vessel_count_dat2<-vessel_count_dat2 %>% mutate(WDWE = recode(WDWE,'Mon'="Weekday",
                                                'Tue'="Weekday",
                                                'Wed'="Weekday",
                                                'Thu'="Weekday",
                                                'Fri'="Weekday",
                                                'Sat'="Weekend",
                                                'Sun'="Weekend"))





###########################################################################
#make a table of boats and buoys by area by method

#can make any iteration of this to summarize
effort_table_aerial<-count_dat2%>%group_by(Date,WDWE,weekday,Spot_Quota_Area) %>% summarise(N_dates=length(unique(Date)),sum_buoys=sum(Rec_Buoy_count,na.rm=T),mean_buoys=mean(Rec_Buoy_count,na.rm=T), sd_buoys=sd(Rec_Buoy_count,na.rm=T),sum_boats=sum(Rec_Boat_Count,na.rm=T),mean_boats=sum(Rec_Boat_Count,na.rm=T),sd_boats=sd(Rec_Boat_Count,na.rm=T))

effort_table_aerial

write.xlsx(effort_table_aerial,paste0(Year,"_Aerial_Shrimp_Effort_Summary.xlsx"))


effort_table_vessel<-vessel_count_dat2%>%group_by(Date,WDWE,weekday,Spot_Quota_Area) %>% summarise(N_dates=length(unique(Date)),sum_buoys=sum(Rec_Buoy_count,na.rm=T),mean_buoys=mean(Rec_Buoy_count,na.rm=T), sd_buoys=sd(Rec_Buoy_count,na.rm=T),sum_boats=sum(Rec_Boat_Count,na.rm=T),mean_boats=sum(Rec_Boat_Count,na.rm=T),sd_boats=sd(Rec_Boat_Count,na.rm=T))

effort_table_vessel

write.xlsx(effort_table_vessel,paste0(Year,"_Vessel_Shrimp_Effort_Summary.xlsx"))


###################################################################
# This piece is omitted for 2020 becuase we did not weight shrimp
###################################################################

#Determine the mean pounds per shrimp from creel data
#creel_dat$weight_per_shrimp<-(creel_dat$Spot_Shrimp_Weight*0.00220462)/(creel_dat$Spot_Shrimp_No_Weighed)


# pretty clear there is a difference in weights for shrimp with and without heads on
#shrimp_weights<-creel_dat%>%group_by(Date,Spot_Shrimp_Heads_On) %>% summarise(mean_weight_per_shrimp=mean(weight_per_shrimp,na.rm = T),sd_weight_per_shrimp=sd(weight_per_shrimp,na.rm=T), n = length(Spot_Shrimp_Heads_On))


#ggplot(creel_dat, aes(x=weight_per_shrimp, fill=Spot_Shrimp_Heads_On))+
#  geom_histogram(color="black",alpha=0.8, position="identity")+
#  theme_bw()+
#  guides(fill=guide_legend(title="Heads On"))+
#  xlab("Weight per shrimp (lbs)")+
#  ylab("Frequency")+
#  geom_vline(dat=shrimp_weights, aes(xintercept=mean_weight_per_shrimp), color = "grey20",lwd=1,lty=2)+
#  facet_grid(Date~.)+
#  ggtitle("Weight per Shrimp")


#interesting...calculating the mean weight for only shrimp with heads and also propogating the error through calculations

######################################################################################
# Summarize creel data for fishery statistics
######################################################################################

#starting with 1 day fisheries.

#subsetting the area of interest

if(report_out==1){
creel_dat_sub<-creel_dat2[creel_dat2$Region==area,]
count_dat_sub<-count_dat2[count_dat2$Region==area,]
vessel_count_sub<-vessel_count_dat2[vessel_count_dat2$Region==area,]

#get some information about the total number of boats from pots
hist(creel_dat_sub$Total_No_of_Pots_Fished, main = paste(area), xlab="# of pots")
hist(creel_dat_sub$Spot_Shrimp_Number_Retained, main = paste(area), xlab = "# Spot Shrimp Retained")
}

if(report_out==2){
creel_dat_sub<-creel_dat2[creel_dat2$Spot_Quota_Area==SQA,]
count_dat_sub<-count_dat2[count_dat2$Spot_Quota_Area==SQA,]
vessel_count_sub<-vessel_count_dat2[vessel_count_dat2$Spot_Quota_Area==SQA,]

#get some information about the total number of boats from pots
hist(creel_dat_sub$Total_No_of_Pots_Fished, main = paste(area,": ",SQA), xlab="# of pots")
hist(creel_dat_sub$Spot_Shrimp_Number_Retained, main = paste(area,": ",SQA), xlab = "# Spot Shrimp Retained")
}  


#look at number of pots per day
ggplot(creel_dat_sub, aes(x=Total_No_of_Pots_Fished))+
  geom_histogram(color="black",alpha=0.8, position="identity")+
  theme_bw()+
  xlab("Number of Pots per Boat")+
  ylab("Frequency")+
  #geom_vline(dat=shrimp_weights, aes(xintercept=mean_weight_per_shrimp), color = "grey20",lwd=1,lty=2)+
  facet_grid(Date~Spot_Quota_Area)+
  ggtitle("Shrimp Pots per Boat")


###############################################################################
# in 2020 we did not take weights for spot shrimp so the below code is omitted
###############################################################################

#impute the weight

creel_dat_sub$mean_shrimp_wt = mean_shrimp_wt
creel_dat_sub$shrimp_weight_per_boat = creel_dat_sub$mean_shrimp_wt * creel_dat_sub$Spot_Shrimp_Number_Retained

#find errors
#err<-which(creel_dat_sub$Total_No_of_Pots_Fished==5)
#creel_dat_sub[err,]

#unique(creel_dat_sub$Date)

#err2<-which(creel_dat_sub$Date=="06/21/2020") #no shrimp were retained...
#creel_dat_sub<-creel_dat_sub[-err2,]


#by day
#creel_summary_day<-creel_dat_sub%>%group_by(Date) %>% summarise(median_pots=median(Total_No_of_Pots_Fished,na.rm=T),mean_pots=mean(Total_No_of_Pots_Fished,na.rm=T),sd_pots=sd(Total_No_of_Pots_Fished,na.rm=T),n_interviews=length(Total_No_of_Pots_Fished),mean_shrimp_per_boat=mean(Spot_Shrimp_Number_Retained,na.rm=T),sd_shrimp_per_boat=sd(Spot_Shrimp_Number_Retained,na.rm = T),mean_weight_per_boat=mean(shrimp_weight_per_boat,na.rm = T),sd_weight_per_boat=sd(shrimp_weight_per_boat,na.rm=T), n_boats_weighted=sum(shrimp_measured,na.rm=T))


creel_summary_day<-creel_dat_sub%>%group_by(Date,WDWE,weekday) %>% summarise(median_pots=median(Total_No_of_Pots_Fished,na.rm=T),mean_pots=mean(Total_No_of_Pots_Fished,na.rm=T),sd_pots=sd(Total_No_of_Pots_Fished,na.rm=T),n_interviews=length(Total_No_of_Pots_Fished),mean_wt_per_shrimp=mean(mean_shrimp_wt,na.rm = T),sd_wt_per_shrimp=sd(mean_shrimp_wt,na.rm = T),mean_shrimp_per_boat=mean(Spot_Shrimp_Number_Retained,na.rm=T),sd_shrimp_per_boat=sd(Spot_Shrimp_Number_Retained,na.rm = T),mean_weight_per_boat=mean(shrimp_weight_per_boat,na.rm = T),sd_weight_per_boat=sd(shrimp_weight_per_boat,na.rm=T))

creel_summary_day


#look at the data
ggplot(creel_dat_sub, aes(x=shrimp_weight_per_boat))+
  geom_histogram(color="black",alpha=0.8, position="identity",binwidth = 4)+
  theme_bw()+
  xlab("Shrimp Weight per Boat (lbs)")+
  ylab("Frequency")+
  geom_vline(dat=creel_summary_day, aes(xintercept=mean_weight_per_boat), color = "grey20",lwd=1,lty=2)+
  facet_grid(Date~.)+
  ggtitle("Shrimp Weight per Boat (lbs)")


ggplot(creel_summary_day, aes(y=mean_shrimp_per_boat,x=Date))+
  geom_point(color="black",alpha=0.8, size = 3)+
  theme_bw()+
  geom_hline(yintercept = mean(creel_summary_day$mean_shrimp_per_boat), col = "red")+
  ylab("Shrimp Weight per Boat (lbs)")+
  xlab("Date")+
  #facet_grid(Date~.)+
  ggtitle("Shrimp Weight per Boat (lbs)")


#by area and day
creel_summary_area<-creel_dat_sub%>%group_by(Spot_Quota_Area,WDWE) %>% summarise(median_pots=median(Total_No_of_Pots_Fished,na.rm=T),mean_pots=mean(Total_No_of_Pots_Fished,na.rm=T),n_interviews=length(Total_No_of_Pots_Fished),mean_shrimp_per_boat=mean(Spot_Shrimp_Number_Retained))

creel_summary_area$mean_lbs_per_boat<-creel_summary_area$mean_shrimp_per_boat*mean_shrimp_wt
creel_summary_area


##############################################################################################################
# Linking the effort data to the creel data for determining the expansion factor
############################################################################################################## 

########################################
# Calculate boats missed for Exp factor
########################################

c_dat<-creel_dat_sub
c_dat$Time_Last_Pot_Pulled<-times(c_dat$Time_Last_Pot_Pulled)
c_dat$Time_Pots_Started_Soaking<-times(c_dat$Time_Pots_Started_Soaking)
c_dat$Time_Pots_Started_Soaking<-times(c_dat$Time_Pots_Started_Soaking)

c_dat$Time_Boat_Departed<-format(c_dat$Time_Boat_Departed,format="%H:%M:%S")
c_dat$Time_Boat_Returned<-format(c_dat$Time_Boat_Returned,format="%H:%M:%S")
c_dat$Time_Boat_Departed<-times(c_dat$Time_Boat_Departed)
c_dat$Time_Boat_Returned<-times(c_dat$Time_Boat_Returned)


#create an vector to tally the trips that were missed by the survey
c_dat$missing_boats<-NA


if(survey_type==1)
{
boat_dat<-count_dat_sub
boat_dat$Time_2<-times(boat_dat$Time_2)  

#using boat data
if(effort_type==1) {
for(j in 1:dim(boat_dat)[1])
  for(i in 1:dim(c_dat)[1]) 
  {
    if((c_dat$Date[i]==boat_dat$Date[j]) && (c_dat$Spot_Quota_Area[i]==boat_dat$Spot_Quota_Area[j]) && ((c_dat$Time_Boat_Returned[i]<min(boat_dat$Time_2)) || (c_dat$Time_Boat_Departed[i]>max(boat_dat$Time_2))))
    {c_dat$missing_boats[i]=1}}
  effort_tally<-boat_dat %>% group_by(Date) %>% summarise(effort_count=sum(Rec_Boat_Count,na.rm=T))
}


#using the buoy data
if(effort_type==2) {
  
for(j in 1:dim(boat_dat)[1])
  for(i in 1:dim(c_dat)[1]) 
    {
    if((c_dat$Date[i]==boat_dat$Date[j]) && (c_dat$Spot_Quota_Area[i]==boat_dat$Spot_Quota_Area[j]) && ((c_dat$Time_Last_Pot_Pulled[i]<min(boat_dat$Time_2)) || (c_dat$Time_Pots_Started_Soaking[i]>max(boat_dat$Time_2))))
    {c_dat$missing_boats[i]=1}}
  
  effort_tally<-boat_dat %>% group_by(Date) %>% summarise(effort_count=sum(Rec_Buoy_count,na.rm=T))
  
}}


if(survey_type==2)
  {
boat_dat<-vessel_count_sub
#boat_dat$Time_2<-times(boat_dat$Time_2) 
  
    #using boat data
    if(effort_type==1) {
      for(j in 1:dim(boat_dat)[1])
        for(i in 1:dim(c_dat)[1]) 
        {
          if((c_dat$Date[i]==boat_dat$Date[j]) && (c_dat$Creel_Area[i]==boat_dat$Creel_Area[j]) && ((c_dat$Time_Last_Pot_Pulled[i]<min(boat_dat$Observation_Start_Time[j])) | (c_dat$Time_Pots_Started_Soaking[i]>max(boat_dat$Observation_End_Time[j]))))
            
          {c_dat$missing_boats[i]=1}}
      
      effort_tally<-boat_dat %>% group_by(Date) %>% summarise(effort_count=sum(Rec_Boat_Count,na.rm=T))
      
      
    }
    
    
    #using the buoy data
    if(effort_type==2) {
      for(j in 1:dim(boat_dat)[1])
        for(i in 1:dim(c_dat)[1]) 
        {
          
          if((c_dat$Date[i]==boat_dat$Date[j]) && (c_dat$Creel_Area[i]==boat_dat$Creel_Area[j]) && ((c_dat$Time_Last_Pot_Pulled[i]<min(boat_dat$Observation_Start_Time[j])) | (c_dat$Time_Pots_Started_Soaking[i]>max(boat_dat$Observation_End_Time[j]))))
             
          {c_dat$missing_boats[i]=1}}
      
      effort_tally<-boat_dat %>% group_by(Date) %>% summarise(effort_count=sum(Rec_Buoy_count,na.rm=T))
      
    }}


#get summary information to calculate the expasion factor
exp.fac.table<-c_dat %>% group_by(Date) %>% summarise(tot_missed_int = sum(na.omit(missing_boats)),tot_int=length(missing_boats))

temp1<-merge(exp.fac.table,effort_tally,all=T)

#identifying those dates that have both interviews and effort counts
temp1$tot_missed_int[which(is.na(temp1$effort_count==T))]=NA

Final_Table<-merge(creel_summary_day,temp1,all=T)

#calculate the expansion factor

#add in the # of boats based on the # buoys calculations
if(effort_type==1){
  Final_Table$N_Boats<-Final_Table$effort_count}
  
if(effort_type==2){
Final_Table$N_Boats<-Final_Table$effort_count/Final_Table$mean_pots
}

Final_Table$exp.factor<-(Final_Table$tot_int-Final_Table$tot_missed_int)/(Final_Table$tot_int)

#Correcting weekends and weekdays
Final_Table$weekday<-weekdays(Final_Table$Date)
Final_Table$WDWE<-Final_Table$weekday

Final_Table<-Final_Table %>% mutate(WDWE = recode(WDWE,'Mon'="Weekday",
                                                'Tue'="Weekday",
                                                'Wed'="Weekday",
                                                'Thu'="Weekday",
                                                'Fri'="Weekday",
                                                'Sat'="Weekend",
                                                'Sun'="Weekend"))

#calculating the catch for given days with expansion factor

Final_Table$expanded_effort<-(Final_Table$N_Boats/Final_Table$exp.factor)
Final_Table$daily_catch<-Final_Table$expanded_effort*Final_Table$mean_weight_per_boat


#check the missing boats
#c_dat[which(c_dat$missing_boats==1),]



#####################################################
# save summary report
#####################################################


if(report_out==1){
Final_Table$Area<-area
Final_Table$SQA<-SQA
if(effort_type==1)
{write.xlsx(Final_Table,paste0(area,"_2020_shrimp_catch_estimate_boat_",strftime(Sys.time(), "%Y%m%d"),".xlsx"))}

if(effort_type==2)
{write.xlsx(Final_Table,paste0(area,"_2020_shrimp_catch_estimate_buoy_",strftime(Sys.time(), "%Y%m%d"),".xlsx"))}

}


if(report_out==2){
Final_Table$Area<-area
Final_Table$SQA<-SQA

if(effort_type==1)
{write.xlsx(Final_Table,paste0(area,"_",SQA,"_2020_shrimp_catch_estimate_boat_",strftime(Sys.time(), "%Y%m%d"),".xlsx"))}

if(effort_type==2)
{write.xlsx(Final_Table,paste0(area,"_",SQA,"_2020_shrimp_catch_estimate_buoy_",strftime(Sys.time(), "%Y%m%d"),".xlsx"))}
}

}








#############################################################################################################################
#
#####################################################################

############
#Method 1

#estimating catch from buoy counts
effort_table_boat<-effort_table[effort_table$Survey_Method=="Boat",]

buoy_total<-effort_table_boat%>%group_by(Date)%>%summarise(bouy_total=sum(sum_buoys))
rec_summary_day<-merge(creel_summary_day,buoy_total)
rec_summary_day$est_num_boats<-rec_summary_day$bouy_total/rec_summary_day$mean_pots
rec_summary_day$sd_num_boats<-(rec_summary_day$bouy_total/rec_summary_day$mean_pots)*(rec_summary_day$sd_pots/rec_summary_day$mean_pots)

rec_summary_day$exp.fac<-c(exp.factor.boat1,exp.factor.boat2)
rec_summary_day$Total_Boats<-rec_summary_day$est_num_boats/rec_summary_day$exp.fac
rec_summary_day$sd_Total_Boats<-(rec_summary_day$est_num_boats/rec_summary_day$exp.fac)*(rec_summary_day$sd_num_boats/rec_summary_day$est_num_boats)
rec_summary_day$Total_Catch<-rec_summary_day$mean_lbs_per_boat*rec_summary_day$Total_Boats
rec_summary_day$sd_Total_Catch<-((rec_summary_day$sd_lbs_per_boat/rec_summary_day$mean_lbs_per_boat)+(rec_summary_day$sd_Total_Boats/rec_summary_day$Total_Boats))*rec_summary_day$Total_Catch


write.csv(rec_summary_day,"Shrimp_harvest_2019_2E_Bouy.csv")


############
#Method 2
############

#estimating catch from buoy counts
effort_table_boat<-effort_table[effort_table$Survey_Method=="Boat",]

boats_total<-effort_table_boat%>%group_by(Date)%>%summarise(Boat_shrimping=sum(sum_boats_shrimp),Boat_not_shrimping=sum(sum_boats_not_shrimping))

rec_summary_day<-merge(creel_summary_day,boats_total)
rec_summary_day$exp.fac<-c(exp.factor.boat1,exp.factor.boat2)
rec_summary_day$Total_Boats<-rec_summary_day$Boat_shrimping/rec_summary_day$exp.fac
rec_summary_day$Total_Catch<-rec_summary_day$mean_lbs_per_boat*rec_summary_day$Total_Boats

#here we assume that the number of boats is correct
rec_summary_day$sd_Total_Catch<-(rec_summary_day$sd_lbs_per_boat*rec_summary_day$Total_Boats)
write.csv(rec_summary_day,"Shrimp_harvest_2019_2E_Boat_survey.csv")

##############
#Method 3
##############

#estimating effort from arial counts
#need to adjust the expansion factor for the survey duration of the flights

#automating calculation of trips in an area that were not caught
flight_dat<-subset(effort_dat,Survey_Method=="Arial" & Date == "5/11/2019")
c_dat<-subset(creel_dat,Date == "5/11/2019")


#create an vector to tally the trips that were missed by the survey
c_dat$missing_boats<-NA


for(j in 1:dim(boat_dat)[1])
  for(i in 1:dim(c_dat)[1]) 
  {
    if((c_dat$Date[i]==flight_dat$Date[j]) && (c_dat$Shrimp_Catch_Area_Map[i]==flight_dat$Shrimp_Creel_Area[j]) && ((c_dat$Time_Last_Pot_Pulled[i]<flight_dat$Observation_Start_Time[j]) | (c_dat$Time_Pots_Started_Soaking[i]>flight_dat$Observation_End_Time[j])))
    {c_dat$missing_boats[i]=1}}

#calculate the Expansion factor for boat survey
exp.factor.flight1<-1-(sum(c_dat$missing_boats,na.rm = T)/length(c_dat$missing_boats))
exp.factor.flight1


####################
#day 2
####################
flight_dat<-subset(effort_dat,Survey_Method=="Arial" & Date == "5/15/2019")
c_dat<-subset(creel_dat,Date == "5/15/2019")


#create an vector to tally the trips that were missed by the survey
c_dat$missing_boats<-NA


for(j in 1:dim(boat_dat)[1])
  for(i in 1:dim(c_dat)[1]) 
  {
    if((c_dat$Date[i]==flight_dat$Date[j]) && (c_dat$Shrimp_Catch_Area_Map[i]==flight_dat$Shrimp_Creel_Area[j]) && ((c_dat$Time_Last_Pot_Pulled[i]<flight_dat$Observation_Start_Time[j]) | (c_dat$Time_Pots_Started_Soaking[i]>flight_dat$Observation_End_Time[j])))
    {c_dat$missing_boats[i]=1}}

#calculate the Expansion factor for boat survey
exp.factor.flight2<-1-(sum(c_dat$missing_boats,na.rm = T)/length(c_dat$missing_boats))
exp.factor.flight2

################################
# now work up the totals
###########################

effort_table_flight<-effort_dat[effort_dat$Survey_Method=="Arial",]%>%group_by(Date,Survey_Method,Shrimp_Creel_Area) %>% summarise(sum_buoys=sum(Buoys),sum_boats_shrimp=sum(Boats_Shrimping),sum_boats_not_shrimping=sum(Boats_Not_Shrimping),sum_boats_uncert=sum(Boats_Uncertain))

boats_total<-effort_table_flight%>%group_by(Date)%>%summarise(Boat_shrimping=sum(sum_boats_shrimp,na.rm = T),Boat_not_shrimping=sum(sum_boats_not_shrimping,na.rm = T),sum_boats_uncert=sum(sum_boats_uncert,na.rm = T))
boats_total$Boat_shrimping_high<-boats_total$Boat_shrimping+boats_total$sum_boats_uncert

rec_summary_day<-merge(creel_summary_day,boats_total)
rec_summary_day$exp.fac<-c(exp.factor.flight1,exp.factor.flight2)
rec_summary_day$Total_Boats<-rec_summary_day$Boat_shrimping/rec_summary_day$exp.fac
rec_summary_day$Total_Boats_high<-rec_summary_day$Boat_shrimping_high/rec_summary_day$exp.fac
rec_summary_day$Total_Catch<-rec_summary_day$mean_lbs_per_boat*rec_summary_day$Total_Boats
rec_summary_day$sd_Total_Catch<-(rec_summary_day$sd_lbs_per_boat*rec_summary_day$Total_Boats)
rec_summary_day$Total_Catch_high<-rec_summary_day$mean_lbs_per_boat*rec_summary_day$Total_Boats_high
rec_summary_day$sd_Total_Catch_high<-(rec_summary_day$sd_lbs_per_boat*rec_summary_day$Total_Boats_high)

write.csv(rec_summary_day,"Shrimp_harvest_2019_2E_Arial_survey.csv")

############################################################################
#MAKE a plot

flight<-read.csv("Shrimp_harvest_2019_2E_Arial_survey.csv")
boat<-read.csv("Shrimp_harvest_2019_2E_Boat_survey.csv")
buoy<-read.csv("Shrimp_harvest_2019_2E_Bouy.csv")

data<-data.frame(Method = c("Buoy","Buoy","Boat","Boat","Aerial","Aerial","Aerial_High","Aerial_High"))
data$Method <- factor(data$Method, c("Buoy","Boat","Aerial","Aerial_High"))

data$Date<-rep(c("5/11/2019","5/15/2019"),4)
data$Total_Catch<-c(buoy$Total_Catch,boat$Total_Catch,flight$Total_Catch,flight$Total_Catch_high)
data$sd_Total_Catch<-c(buoy$sd_Total_Catch,boat$sd_Total_Catch,flight$sd_Total_Catch,flight$sd_Total_Catch_high)

catch_amts<-data.frame(Total_Catch=tapply(data$Total_Catch,data$Method,sum),SD_Total_Catch=tapply(data$sd_Total_Catch,data$Method,sum))
catch_amts$Method<-rownames(catch_amts)
catch_amts$quota<-rep(16768,4)
catch_amts$diff<-catch_amts$Total_Catch-catch_amts$quota
catch_amts$Per_diff<-((catch_amts$Total_Catch-catch_amts$quota)/catch_amts$quota)*100
catch_amts$lower<-qnorm(0.25,mean=catch_amts$Total_Catch, sd=catch_amts$SD_Total_Catch)
catch_amts$upper<-qnorm(0.75,mean=catch_amts$Total_Catch, sd=catch_amts$SD_Total_Catch)

catch_amts
write.csv(catch_amts,"Rec_Shrimp_Catch_Amounts_2019.csv")


#make the plot
ggplot(data, aes(Method,Total_Catch,fill=Date))+
  geom_bar(color="black",alpha=0.8,stat="identity")+
  scale_fill_manual(values=c("grey80","grey60"))+
  theme_bw()+
  #guides(fill=guide_legend(title="Heads On?"))+
  ylab("Total Catch (lbs)")+
  geom_hline(yintercept = 16786, color = "red",lwd=1.0)+
  xlab("Method")+
 geom_segment(aes(x = 1, y = catch_amts$lower[1], xend = 1, yend = catch_amts$upper[1]), colour = "grey50")+
 geom_segment(aes(x = 2, y = catch_amts$lower[2], xend = 2, yend = catch_amts$upper[2]), colour = "grey50")+
 geom_segment(aes(x = 3, y = catch_amts$lower[3], xend = 3, yend = catch_amts$upper[3]), colour = "grey50")+
  geom_segment(aes(x = 4, y = catch_amts$lower[4], xend = 4, yend = catch_amts$upper[4]), colour = "grey50")+
  annotate("text",x=1,y=catch_amts$Total_Catch[1]+2000,label=round(catch_amts$Total_Catch[1],0),fontface =2,color = "blue")+
  annotate("text",x=2,y=catch_amts$Total_Catch[2]+2000,label=round(catch_amts$Total_Catch[2],0),fontface =2,color = "blue")+
  annotate("text",x=3,y=catch_amts$Total_Catch[3]+2000,label=round(catch_amts$Total_Catch[3],0),fontface =2,color = "blue")+
  annotate("text",x=4,y=catch_amts$Total_Catch[4]+2000,label=round(catch_amts$Total_Catch[4],0),fontface =2,color = "blue")+
  ggtitle("Estimated Total Recreational Catch (lbs)")
  #annotate("text",x=1,y=catch_amts$Total_Catch[1]-9000,label=round(catch_amts$diff[1],0))+
  #annotate("text",x=2,y=catch_amts$Total_Catch[1]-9000,label=round(catch_amts$diff[2],0))+
  #annotate("text",x=3,y=catch_amts$Total_Catch[1]-9000,label=round(catch_amts$diff[3],0))+
  #annotate("text",x=4,y=catch_amts$Total_Catch[1]-9000,label=round(catch_amts$diff[4],0))+

####################################################################################

#and that is a wrap






