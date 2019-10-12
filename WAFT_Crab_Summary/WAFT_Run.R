##########################################################
# Running the WAFT fish ticket checker
# to look at landings 
# Created by: Katelyn Bosley
# Date: 10/8/2019
###########################################################

# This code is to replace the clunky spreadsheet set_up that Don R had for fish ticket reconciliation
# and it also works up the CPUE based on fish tickets

#load libraries
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

###########################################################
#setwd
wd<-"D:\\R_Catch"
setwd(wd)


#read in data
raw<-read.csv("PS Crab_10.11.19.csv")
head(raw)
sapply(raw,class)


#set number of pots per license and other specifications for the data
pots=50
year=2019
days=1:9

#select specific region
# 1 == yes, then enter the regions
# 0 == no
select.reg = 1

#if you area selecting regions, enter which regions
regs=c("1","2E")


#run the code
{

#enter the raw data
raw$Catch_Area_Code<-str_replace(raw$Catch_Area_Code," ", "")

raw<-raw[raw$Batch_year==year,]
raw<-raw[raw$Day%in%days,]


###########################################################
# Enter the regions table
reg<-read.csv("Region_Area.csv")
head(reg)
sapply(reg,class)
names(reg)[1]<-"Catch_Area_Code"

###########################################################
# merge all the data 
full.data<-merge(raw,reg,by='Catch_Area_Code')

#change the 2 to 2E
area2<-which(full.data$Region==2 & full.data$Split_SubUnit_Code=="E")
#area3<-which(full.data$Region==2 & full.data$Split_SubUnit_Code=="W")

#look to see what area these 2 values are
#full.data[area2,]
full.data$Region[area2]="2E"

if(select.reg == 1){full.data<-full.data[full.data$Region%in%regs,]}




full.data$Landing_Date_2=as.Date(full.data$Landing_Date, format = "%M/%d/%Y")
head(full.data)

#summarize by date
daily_sum_save<-full.data%>%group_by(Landing_Date,Region)%>%summarize(Total_Pounds = sum(Round_Lbs_Qty))
daily_FT_area_save<-spread(daily_sum_save,Region,Total_Pounds)
write.csv(daily_FT_area_save,paste0("Daily_FT_",year,".csv"))
        

#look at the number of liscense per area
daily_sum<-full.data%>%group_by(Landing_Date,Region)%>%summarize(Total_Pounds = sum(Round_Lbs_Qty),N=length(License_ID))
daily_sum_FT_save<-full.data%>%group_by(Landing_Date,Region)%>%summarize(Total_Pounds = sum(Round_Lbs_Qty))
daily_FT_area<-spread(daily_sum,Region,Total_Pounds)

#
boats.area<-ggplot(daily_sum, aes(x = as.factor(Landing_Date), y=N)) +
  geom_bar(aes(fill=Region),col="grey30",stat="identity", position = 'dodge')+
  theme_bw()+
  ylab("Licenses/Area/Day")+
  xlab("Date")+
  facet_wrap(~Region)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
boats.area

ggsave(paste0("Licenses_per_Area_",year,".pdf"))


#looking at the number of liscences fishing per area per day

#calculate the CPUE
daily_cpue<-full.data%>%group_by(Landing_Date_2,License_ID, Region)%>%summarize(Total_Pounds = sum(Round_Lbs_Qty))
daily_cpue

#
daily_cpue_FT<-full.data%>%group_by(Landing_Date_2,License_ID,Fish_Ticket_Num,Region)%>%summarize(Total_Pounds = sum(Round_Lbs_Qty),N=length(License_ID))
daily_cpue_FT

#look<-which(daily_cpue_FT$License_ID==56075)
#daily_cpue_FT[look,]

t.cpue<-ggplot(daily_cpue_FT, aes(x = as.factor(Landing_Date_2), y =Total_Pounds)) +
  geom_boxplot(aes(fill = Region),outlier.colour="black", outlier.shape=8,
               outlier.size=4)+
  theme_bw()+
  ylab("Pounds/License/Day")+
  xlab("Date")+
  facet_wrap(~Region)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
#t.cpue


sale.cpue<-ggplot(daily_cpue_FT, aes(x = as.factor(Landing_Date_2), y=Total_Pounds)) +
  geom_boxplot(aes(fill = Region),outlier.colour="black", outlier.shape=8,
             outlier.size=4)+
  stat_smooth(method="lm")+
  theme_bw()+
  ylab("Average Pounds/License/Sale")+
  xlab("Date")+
  facet_wrap(~Region)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
sale.cpue

#t.cpue
ggsave(paste0("Comm_Crab_CPUE_",year,".pdf"))


#another approach
pot.cpue_2<-ggplot(daily_cpue_FT, aes(x = Landing_Date_2, y =Total_Pounds/pots)) +
  geom_point(aes(fill = Region))+
  stat_smooth()+
  theme_bw()+
  ylab("Average Pounds/License/Pot")+
  xlab("Date")+
  facet_wrap(~Region)

pot.cpue_2

ggsave(paste0("Comm_Crab_CPUE_model_",year,".pdf"))


#mean CPUE
CPUE_summary<-daily_cpue_FT%>%group_by(Region)%>%summarize(Mean_CPUE=mean(Total_Pounds))
CPUE_summary$Mean_by_pot<-CPUE_summary$Mean_CPUE/pots
print.data.frame(CPUE_summary)
write.csv(CPUE_summary,paste0("CPUE_Summary_",year,".csv"))


sale.cpue<-sale.cpue+geom_hline(data=CPUE_summary,aes(yintercept=Mean_CPUE), color="red",lty=2)
ggsave(paste0("Comm_Crab_CPUE_",year,".pdf"))


pdf(paste0("all_CPUE_",year,".pdf"),paper="letter")
grid.arrange(sale.cpue, pot.cpue_2,nrow=2)
dev.off()

}

