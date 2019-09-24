==##############################################################################
# Determining the difference in recreationa shrimp effort using the different 
# methods, based on bouy counts, boat counts by boat survey and arial survey
#
# Created By: Katelyn Bosley
# Date: 7/20/2019
#############################################################################

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
  library(gtools)
  library(TeachingDemos)
  library(snowfall)
  library(snow)
  library(foreach)
  library(doSNOW)
  library(parallel)
  library(spatstat)
  library(alphahull)
  library(beanplot)
  library(tidyr)
  library(png)
  library(MASS)
  library(chron)
  library(stringr)
  library(lubridate)
}
load_libraries()


# Set working directory
wd<-("D:\\WDFW\\MANAGEMENT\\SHRIMP\\Recreational\\2019_Shrimp_Methods_Reveiw\\methods_comparison\\Recreational_Shrimp_Harvest_Effort_Estimation")
setwd(wd)

#enter the creel data
creel_dat<-read.csv("Shrimp_Creel_2019_2E.csv")
head(creel_dat)

#adjust creel data time variables to something I can work with
creel_dat$Time_Interviewed<-times(creel_dat$Time_Interviewed,out.format="h:m")
creel_dat$Time_Pots_Started_Soaking<-times(creel_dat$Time_Pots_Started_Soaking,out.format="h:m")
creel_dat$Time_Last_Pot_Pulled<-times(creel_dat$Time_Last_Pot_Pulled,out.format="h:m")


#enter the effort data
effort_dat<-read.csv("Boat_Bouy_Counts_2019_2E.csv")

#remove the records where no survey occured
effort_dat<-effort_dat[!effort_dat$Observation_Start_Time %in% NA,]
effort_dat$Observation_Start_Time<-times(effort_dat$Observation_Start_Time,out.format="h:m")
effort_dat$Observation_End_Time<-times(effort_dat$Observation_End_Time,out.format="h:m")

#look at the data and the classes
head(effort_dat)
sapply(effort_dat,class)



####################################################
#make a table of boats and buoys by area by method

effort_table<-effort_dat%>%group_by(Date,Survey_Method,Shrimp_Creel_Area) %>% summarise(sum_buoys=sum(Buoys),sum_boats_shrimp=sum(Boats_Shrimping),sum_boats_not_shrimping=sum(Boats_Not_Shrimping))
effort_table

#Determine the mean pounds per shrimp from creel data
creel_dat$weight_per_shrimp<-(creel_dat$Spot_Shrimp_Weight*0.00220462)/(creel_dat$Spot_Shrimp_No_Weighed)

# pretty clear there is a difference in weights for shrimp with and without heads on
shrimp_weights<-creel_dat%>%group_by(Date,Spot_Shrimp_Heads_On) %>% summarise(mean_weight_per_shrimp=mean(weight_per_shrimp,na.rm = T),sd_weight_per_shrimp=sd(weight_per_shrimp,na.rm=T), n = length(Spot_Shrimp_Heads_On))


ggplot(creel_dat, aes(x=weight_per_shrimp, fill=Spot_Shrimp_Heads_On))+
  geom_histogram(color="black",alpha=0.8, position="identity")+
  theme_bw()+
  guides(fill=guide_legend(title="Heads On"))+
  xlab("Weight per shrimp (lbs)")+
  ylab("Frequency")+
  geom_vline(dat=shrimp_weights, aes(xintercept=mean_weight_per_shrimp), color = "grey20",lwd=1,lty=2)+
  facet_grid(Date~.)+
  ggtitle("Weight per Shrimp")

#################################################################################
# Calculating the converted estimate for mean weight per shrimp
#################################################################################


creel_dat$adj_weight<-creel_dat$weight_per_shrimp

for(i in 1:nrow(creel_dat))
{if(creel_dat$Spot_Shrimp_Heads_On[i]=="No")

creel_dat$adj_weight[i]=creel_dat$weight_per_shrimp[i]*2.3}

#I know there is an outlier so removing it

rem<-which(creel_dat$adj_weight>0.125)
creel$dat<-creel$dat[-rem,]

shrimp_weights_adj<-creel_dat%>%group_by(Date,Spot_Shrimp_Heads_On) %>% summarise(mean_weight_per_shrimp=mean(adj_weight,na.rm = T),sd_weight_per_shrimp=sd(adj_weight,na.rm=T), n = length(Spot_Shrimp_Heads_On))

shrimp_weights_adj_all<-creel_dat%>%group_by(Date) %>% summarise(mean_weight_per_shrimp=mean(adj_weight,na.rm = T),sd_weight_per_shrimp=sd(adj_weight,na.rm=T), n = length(Spot_Shrimp_Heads_On))



ggplot(creel_dat, aes(x=adj_weight, fill=Spot_Shrimp_Heads_On))+
  geom_histogram(color="black",alpha=0.8)+
  theme_bw()+
  guides(fill=guide_legend(title="Adj Heads On"))+
  xlab("Weight per shrimp (lbs)")+
  ylab("Frequency")+
  geom_vline(dat=shrimp_weights_adj_all, aes(xintercept=mean_weight_per_shrimp), color = "grey20",lwd=1,lty=2)+
  facet_grid(Date~.)+
  ggtitle("Weight per Shrimp")




##############################################################################################
# Perhaps we need to confirm the mean shrimp weight is good. Here I conduct a bootstrap sample of the heads on shrimp to check if the mean changes much..
#############################################################################################

#separating the dates
shrimp_weights_yes_1<-subset(creel_dat,weight_per_shrimp>0 & Spot_Shrimp_Heads_On=="Yes"& Date=="5/11/2019")
shrimp_weights_yes_2<-subset(creel_dat,weight_per_shrimp>0 & Spot_Shrimp_Heads_On=="Yes"& Date=="5/15/2019")

shrimp_weights_all_1<-subset(creel_dat,weight_per_shrimp>0 & Date=="5/11/2019")
shrimp_weights_all_2<-subset(creel_dat,weight_per_shrimp>0 & Date=="5/15/2019")

#create a data.frame to hold the bootstrap samples
#set the number of bootstrap samples
bs_num=200

boot.data<-matrix(NA,nrow(shrimp_weights_all_boot),bs_num)


for(i in 1:bs_num){

#drawing the bootstrap samples
shrimp_weights_all_1$boot<-sample(shrimp_weights_yes_1$weight_per_shrimp, nrow(shrimp_weights_all_1),replace = T)
shrimp_weights_all_2$boot<-sample(shrimp_weights_yes_2$weight_per_shrimp, nrow(shrimp_weights_all_2),replace = T)

#combining them again
shrimp_weights_all_boot<-rbind(shrimp_weights_all_1,shrimp_weights_all_2)

#populate the matrix
boot.data[,i]<-shrimp_weights_all_boot$boot
}
  

boot.data.full<-data.frame(Date=shrimp_weights_all_boot$Date)
boot.data.full<-cbind(boot.data.full,as.data.frame(boot.data))
boot.data.melt<-melt(boot.data.full)


mean_shrimp_weights_boot<-boot.data.melt%>%group_by(Date,variable) %>% summarise(mean_weight_per_shrimp=mean(value,na.rm = T),sd_weight_per_shrimp=sd(value,na.rm=T), n = length(variable))

mean_shrimp_weights_boot_final<-boot.data.melt%>%group_by(Date) %>% summarise(mean_weight_per_shrimp=mean(value,na.rm = T),sd_weight_per_shrimp=sd(value,na.rm=T))



#making the plot
ggplot(mean_shrimp_weights_boot, aes(x=mean_weight_per_shrimp))+
  geom_histogram(color="black",alpha=0.8, position="identity")+
  theme_bw()+
  guides(fill=guide_legend(title="Heads On"))+
  xlab("Weight per shrimp (lbs)")+
  ylab("Frequency")+
  geom_vline(dat=mean_shrimp_weights_boot_final, aes(xintercept=mean_weight_per_shrimp), color = "grey20",lwd=1,lty=2)+
  facet_grid(Date~.)+
  ggtitle("Weight per Shrimp")



#converted weights
shrimp_weights_heads_off<-shrimp_weights[c(2,5),]
shrimp_weights_heads_off$adj<-shrimp_weights_heads_off$mean_weight_per_shrimp*2.2

#interesting...calculating the mean weight for only shrimp with heads and also propogating the error through calculations

######################################################################################
# Summarize creel data

#get some information about the total number of boats from pots
hist(creel_dat$Total_Pots_Fished)
hist(creel_dat$Spot_Shrimp_Retained)

#look at buoy counts
ggplot(creel_dat, aes(x=Total_Pots_Fished))+
  geom_histogram(color="black",alpha=0.8, position="identity")+
  theme_bw()+
  xlab("Number of Pots per Boat")+
  ylab("Frequency")+
  #geom_vline(dat=shrimp_weights, aes(xintercept=mean_weight_per_shrimp), color = "grey20",lwd=1,lty=2)+
  facet_grid(Date~.)+
  ggtitle("Shrimp Pots per Boat")


#multiply shrimp weights per day by spot retained

#set up col
creel_dat$shrimp_weight_per_boat<-NA
creel_dat$shrimp_measured<-NA


#calc mean total shrimp weight per boat
for (i in 1:length(creel_dat$Date))
  for(j in 1:length(shrimp_weights$mean_weight_per_shrimp)){
  if(creel_dat$Date[i]==shrimp_weights$Date[j])
  {creel_dat$shrimp_weight_per_boat[i]<-creel_dat$Spot_Shrimp_Retained[i]*shrimp_weights$mean_weight_per_shrimp[j]}}

#calc the number of boats with shrimp weighed
for(i in 1:length(creel_dat$Date)){
if(!is.na(creel_dat$Spot_Shrimp_Weight[i])){creel_dat$shrimp_measured[i]=1}}



#by day
creel_summary_day<-creel_dat%>%group_by(Date) %>% summarise(median_pots=median(Total_Pots_Fished,na.rm=T),mean_pots=mean(Total_Pots_Fished,na.rm=T),sd_pots=sd(Total_Pots_Fished,na.rm=T),n_interviews=length(Total_Pots_Fished),mean_shrimp_per_boat=mean(Spot_Shrimp_Retained,na.rm=T),sd_shrimp_per_boat=sd(Spot_Shrimp_Retained,na.rm = T),mean_weight_per_boat=mean(shrimp_weight_per_boat,na.rm = T),sd_weight_per_boat=sd(shrimp_weight_per_boat,na.rm=T), n_boats_weighted=sum(shrimp_measured,na.rm=T))


#look at the data
ggplot(creel_dat, aes(x=shrimp_weight_per_boat))+
  geom_histogram(color="black",alpha=0.8, position="identity",binwidth = 4)+
  theme_bw()+
  xlab("Shrimp Weight per Boat (lbs)")+
  ylab("Frequency")+
  geom_vline(dat=creel_summary_day, aes(xintercept=mean_weight_per_boat), color = "grey20",lwd=1,lty=2)+
  facet_grid(Date~.)+
  ggtitle("Shrimp Weight per Boat (lbs)")


#calc the heads on weight per shrimp
creel_summary_day_weight<-creel_dat[creel_dat$Spot_Shrimp_Heads_On=="Yes",]%>%group_by(Date) %>% summarise(mean_lb_per_shrimp=mean(weight_per_shrimp,na.rm=T), sd_lb_per_shrimp=sd(weight_per_shrimp,na.rm=T),se_lbs_per_shrimp=(sd(weight_per_shrimp, na.rm=TRUE) /sqrt(length(weight_per_shrimp[!is.na(weight_per_shrimp)]))))
creel_summary_day_weight$lower<-creel_summary_day_weight$mean_lb_per_shrimp-(1.96*creel_summary_day_weight$se_lbs_per_shrimp)
creel_summary_day_weight$upper<-creel_summary_day_weight$mean_lb_per_shrimp+(1.96*creel_summary_day_weight$se_lbs_per_shrimp)

#try to merge the data sets here
creel_summary_day$mean_lb_per_shrimp<-creel_summary_day_weight$mean_lb_per_shrimp
creel_summary_day$sd_lb_per_shrimp<-creel_summary_day_weight$sd_lb_per_shrimp
creel_summary_day$mean_lbs_per_boat<-creel_summary_day$mean_weight_per_boat
creel_summary_day$sd_lbs_per_boat<-creel_summary_day$sd_weight_per_boat

#by area and day
creel_summary_area<-creel_dat%>%group_by(Date,Shrimp_Catch_Area_Map) %>% summarise(median_pots=median(Total_Pots_Fished,na.rm=T),mean_pots=mean(Total_Pots_Fished,na.rm=T),n_interviews=length(Total_Pots_Fished),mean_shrimp_per_boat=mean(Spot_Shrimp_Retained))

creel_summary_area_weight<-creel_dat[creel_dat$Spot_Shrimp_Heads_On=="Yes",]%>%group_by(Date,Shrimp_Catch_Area_Map) %>% summarise(mean_lb_per_shrimp=mean(weight_per_shrimp,na.rm=T))

creel_summary_area<-merge(creel_summary_area,creel_summary_area_weight, by=c("Date","Shrimp_Catch_Area_Map"))
creel_summary_area$mean_lbs_per_boat<-(creel_summary_area$mean_lb_per_shrimp*creel_summary_area$mean_shrimp_per_boat)


ggplot(creel_summary_area, aes(Shrimp_Catch_Area_Map,mean_lb_per_shrimp))+
  geom_col(color="black",alpha=0.2)+
  theme_bw()+
  #guides(fill=guide_legend(title="Heads On?"))+
  ylab("Weight per shrimp (lbs)")+
  geom_hline(data = creel_summary_day_weight, aes(yintercept = mean_lb_per_shrimp), color = "red",lwd=1.0,alpha=0.6)+
  geom_hline(data = creel_summary_day_weight, aes(yintercept = mean_lb_per_shrimp+(1.96*se_lbs_per_shrimp)), color = "blue",wd=1.2,lty=2,alpha=0.8)+
  geom_hline(data = creel_summary_day_weight, aes(yintercept = mean_lb_per_shrimp-(1.96*se_lbs_per_shrimp)), color = "blue",wd=1.2,lty=2,alpha=0.8)+
  xlab("Area")+
  facet_grid(Date~.)+
  ggtitle("Mean lbs per Shrimp by Area")

#95% confidence interval in the mean...looks good!


#################################################################################################################
# Linking the effort data to the creel data for determining the expansion factor
# 

################
# Day 1- boats missed for Exp factor
################

#automating calculation of trips in an area that were not caught
boat_dat<-subset(effort_dat,Survey_Method=="Boat" & Date == "5/11/2019")
c_dat<-subset(creel_dat,Date == "5/11/2019")


#create an vector to tally the trips that were missed by the survey
c_dat$missing_boats<-NA


for(j in 1:dim(boat_dat)[1])
  for(i in 1:dim(c_dat)[1]) 
    {
    if((c_dat$Date[i]==boat_dat$Date[j]) && (c_dat$Shrimp_Catch_Area_Map[i]==boat_dat$Shrimp_Creel_Area[j]) && ((c_dat$Time_Last_Pot_Pulled[i]<boat_dat$Observation_Start_Time[j]) | (c_dat$Time_Pots_Started_Soaking[i]>boat_dat$Observation_End_Time[j])))
     {c_dat$missing_boats[i]=1}}

#calculate the Expansion factor for boat survey
exp.factor.boat1<-1-(sum(c_dat$missing_boats,na.rm = T)/length(c_dat$missing_boats))
exp.factor.boat1


####################
#day 2
####################
boat_dat<-subset(effort_dat,Survey_Method=="Boat" & Date == "5/15/2019")
c_dat<-subset(creel_dat,Date == "5/15/2019")


#create an vector to tally the trips that were missed by the survey
c_dat$missing_boats<-NA


for(j in 1:dim(boat_dat)[1])
  for(i in 1:dim(c_dat)[1]) 
  {
    if((c_dat$Date[i]==boat_dat$Date[j]) && (c_dat$Shrimp_Catch_Area_Map[i]==boat_dat$Shrimp_Creel_Area[j]) && ((c_dat$Time_Last_Pot_Pulled[i]<boat_dat$Observation_Start_Time[j]) | (c_dat$Time_Pots_Started_Soaking[i]>boat_dat$Observation_End_Time[j])))
    {c_dat$missing_boats[i]=1}}

#calculate the Expansion factor for boat survey
exp.factor.boat2<-1-(sum(c_dat$missing_boats,na.rm = T)/length(c_dat$missing_boats))
exp.factor.boat2

#############################################################################################################################
# Determining the total recreational harvest in area 2E
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






