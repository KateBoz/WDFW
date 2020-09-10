##############################################
# Building a Model to predict when to close 
# crab fishery based on WAFT and QR data
# Created by: Katelyn Bosley
# Date: 9/9/2020
#############################################

library(readxl)
library(plotly)
library(reshape2)
library(lubridate)
library(scales)
library(MASS)
library(fitdistrplus)

#run the code
{  

#load in the landings file
#df.daily<-as.data.frame(read_excel(paste("./Output/", "Daily_Landings_Complete","_", Sys.Date(),".xlsx", sep = "")))
  
df.daily<-as.data.frame(read_excel(paste("./Output/", "Daily_Landings_Complete_2020-05-04.xlsx",sep = ""))) 
  
names(df.daily)<-c("LandDate","Reg1","Reg2E","Reg2W","Reg3_1","Reg3_2","Reg3_3")
df.daily$Source<-NA
df.daily$Source[1:(nrow(df.daily)-ndays)]<-"FT"
df.daily$Source[(nrow(df.daily)-ndays+1):nrow(df.daily)]<-"QR"
df.daily$LandDate<-as.Date(df.daily$LandDate,format = "%Y-%m-%d")


#create a long version of the data frame
df.daily.long<-melt(df.daily,id.vars = c("LandDate","Source"))
names(df.daily.long)[3:4]<-c("Region","Pounds")

vars<-setdiff(names(df.daily),c("LandDate","Source"))
labs<-list(title=c("Pounds","Date"))

#generate the plots
plots <- lapply(vars, function(var) {
  plot_ly(df.daily, x = ~LandDate, y = as.formula(paste0("~", var))) %>%
    add_lines(name = var)
})


subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)

#read in openings and closings _ Version 1
open.closed<-read_excel("./input/Openings2019_2020.xlsx")

#Version1
#open.closed.long<-melt(open.closed,id.vars = "Region")
#names(open.closed.long)<-c("LandDate","Region","Opening")
#open<-subset(open.closed.long,Opening=="Open")
#closed<-subset(open.closed.long,Opening=="Closed")

#Version 2
#head(open.closed)
open.closed$Open<-as.Date(open.closed$Open)
open.closed$Closed<-as.Date(open.closed$Closed)
#open.closed$Closed[is.na(open.closed$Closed)]<-as.Date(Sys.time())

#
qr_data<-subset(df.daily.long,Source=="QR")

#ggplot
daily.catch<-ggplot(df.daily.long, aes(x = LandDate, y = Pounds, colour=Region, fill = Region))+
  geom_vline(dat = open.closed, aes(xintercept = Open), col = "black",lwd = 0.5)+ 
  geom_vline(dat = open.closed, aes(xintercept = Closed), col = "red",lwd = 0.5)+ 
  geom_area(aes(fill=Region),lwd = 0.8, alpha = 0.4,lty = 1)+
  geom_line(aes(x = LandDate, y = Pounds, col = Region), lwd = 0.5)+
  geom_line(data=qr_data,aes(x = LandDate, y = Pounds, col = Region), lwd = 1.5)+
  facet_grid(Region~., scales="free_y")+
  ggtitle("Daily Catch by Area")+
  xlab("Date")+
  theme_bw()


#read in the total available
avail<-read_excel("./Input/Commercial_Avail.xlsx")
       
# calculate the cumulative     
df.daily.cm<-df.daily[,1:ncol(df.daily)]
df.daily.cm[,2:7]<-NA
df.daily.cm[1,]<-df.daily[1,]

for(i in 2:nrow(df.daily.cm)){
  for(j in 2:7)
  {df.daily.cm[i,j]<-df.daily.cm[i-1,j]+df.daily[i,j]}}

#make the cumulative data long
df.daily.cm.long<-melt(df.daily.cm,id.vars = c("LandDate","Source"))
names(df.daily.cm.long)[3:4]<-c("Region","Pounds")
qr_data_cm<-subset(df.daily.cm.long,Source=="QR")


# cumulative catch plot
cm.catch.all<-ggplot(df.daily.cm.long, aes(x = LandDate, y = Pounds, colour=Region, fill = Region))+
  geom_vline(dat = open.closed, aes(xintercept = Open), col = "black",lwd = 0.5)+ 
  geom_vline(dat = open.closed, aes(xintercept = Closed), col = "red",lwd = 0.5)+ 
  geom_hline(dat = avail, aes(yintercept = Target, col = Region),lwd = 1.0)+ 
  geom_area(aes(fill=Region),lwd = 0.8, alpha = 0.4,lty = 1)+
  geom_line(aes(x = LandDate, y = Pounds, col = Region), lwd = 0.5)+
  geom_line(data=qr_data_cm,aes(x = LandDate, y = Pounds, col = Region), lwd = 1.5)+
  facet_grid(Region~., scales="free_y")+
  scale_y_continuous(labels = comma)+
  xlab("Date")+
  ggtitle("Cumulative Harvest by Area")+
  theme_bw()


# calculate the percent remaining
#complete the table
avail$Harvest<-colSums(df.daily[,2:7], na.rm = TRUE)
avail$Remaining<-avail$Target-avail$Harvest
names(avail)<-c("Region","Target","Harvest","Remaining")
avail$Percent<-((avail$Target-avail$Harvest)/avail$Target*100)
#avail

 
#######################################
# Tracker Model
#######################################

# calculate the cumulative     
df.daily.catch<-df.daily[,1:ncol(df.daily)]
df.daily.catch[,2:7]<-NA
df.daily.catch[1,2:7]<-avail$Target-df.daily[1,2:7]

for(i in 2:nrow(df.daily.catch)){
  for(j in 2:7)
  {df.daily.catch[i,j]<-df.daily.catch[i-1,j]-df.daily[i,j]}}

#make the cumulative data long
df.daily.catch.long<-melt(df.daily.catch,id.vars = c("LandDate","Source"))
names(df.daily.catch.long)[3:4]<-c("Region","Pounds")
qr_data_catch<-subset(df.daily.catch.long,Source=="QR")

remaining.pounds<-ggplot(df.daily.catch.long, aes(x = LandDate, y = Pounds, colour=Region, fill = Region))+
  geom_vline(dat = open.closed, aes(xintercept = Open), col = "black",lwd = 0.5)+ 
  geom_vline(dat = open.closed, aes(xintercept = Closed), col = "red",lwd = 0.5)+ 
  geom_area(aes(fill=Region),lwd = 0.8, alpha = 0.4,lty = 1)+
  geom_line(aes(x = LandDate, y = Pounds, col = Region), lwd = 0.5)+
  geom_line(data=qr_data_catch,aes(x = LandDate, y = Pounds, col = Region), lwd = 1.5)+
  facet_grid(Region~., scales="free_y")+
  scale_y_continuous(labels = comma)+
  ggtitle("Remaining Pounds by Area")+
  #xlim(as.Date(c(date.min,date.max)))+
  xlab("Date")+
  theme_bw()


#get the average landings for the last days_average
ave_table<-df.daily[(nrow(df.daily.catch)-days_average):nrow(df.daily.catch),2:7]

average_by_area<-data.frame(Region=avail$Region,Ave=colMeans(ave_table),SD = NA)

#table of statistics
for(i in 1:ncol(ave_table))
{average_by_area$SD[i]=sd(ave_table[,i])}


#set up the df for closure dates
close.dates<-data.frame(Region=rep(NA,6),low5 = rep(NA,6),low25 = rep(NA,6),median =rep(NA,6),high75=rep(NA,6),high90 = rep(NA,6))


################################
# Run the rest tracker model
##################################
#k = 1
for(k in 1:ncol(ave_table))
#create function to run all sims and make plots
{
#reg<-which(colnames(ave_table)==sub)
reg = k
sub<-colnames(ave_table)[k]

#run.sims<-function(reg){
#i = 1
test.mat<-matrix(nrow = nsims,ncol = days_average)
for(i in 1:nsims){
  test.mat[i,]<-sample(ave_table[,reg],days_average,replace=T)
}

test.ave<-rowMeans(test.mat, na.rm = TRUE)

prob<-((test.ave/mean(test.ave)))
prob[is.na(prob)]=0

#populate the matrix
time.series=seq(as.Date(Sys.time())-2,as.Date(date.max),1)

#for testing the code
#time.series=seq(as.Date("2020-04-20")-2,as.Date("2020-07-20"),1)

test2<-matrix(NA,length(time.series),nsims)
test2[1,]<-avail$Remaining[reg]


#generate the sampling error

#uniform
if(pdf.fun==1){
  for(i in 2:nrow(test2)){
    for(j in 1:nsims){
      
      test2[i,j]<-test2[i-1,j]-(test.ave[j]*runif(1))
    }}}

#beta dist
if(pdf.fun==2){
  for(i in 2:nrow(test2)){
    for(j in 1:nsims){
      
      test2[i,j]<-test2[i-1,j]-(test.ave[j]*rbeta(1,sh1,sh2))
    }}}

#lognormal
if(pdf.fun==3){
  for(i in 2:nrow(test2)){
    for(j in 1:nsims){
      
      test2[i,j]<-test2[i-1,j]-(test.ave[j]*rlnorm(1,sh1,sh2))
    }}}

#gamma dist
if(pdf.fun==4){
  #fit distribution of the data
  if(sum(prob)>0){
  fit<-fitdist(prob, distr = "gamma", method = "mme", optim.method="Nelder-Mead")
  
  for(i in 2:nrow(test2)){
    for(j in 1:nsims){
      test2[i,j]<-test2[i-1,j]-(test.ave[j]*rgamma(1,coef(fit)[1],coef(fit)[2]))
    }}}
    
    if(sum(prob)<=0){
      for(i in 2:nrow(test2)){
        for(j in 1:nsims){
          test2[i,j]<-test2[i-1,j]-(test.ave[j])
        }
      
    }}}


#hist(prob)
#hist(rbeta(300,1,2))
#hist(rlnorm(500,0,0.25))
#hist(rgamma(500,coef(fit)[1],coef(fit)[2]))
#hist(runif(300))
#matplot(test2[,1:10])

test3<-data.frame(Date = time.series,test2)
test4<-melt(test3,id.vars = "Date")
test4$value[which(test4$value<=0)]<-0
test4$Region<-names(ave_table)[reg]

#Summarize the quartiles and the mean
sums <- test4%>%
  group_by(Date)%>%
  summarise(median=median(value),
            low5=quantile(value,0.10, na.rm = TRUE),
            low25=quantile(value,0.25, na.rm = TRUE),
            high75=quantile(value,0.75, na.rm = TRUE), 
            high90=quantile(value,.90, na.rm = TRUE))

#which dates
close.dates$Region[reg]=names(ave_table)[reg]
close.dates$low5[reg]=as.character(sums$Date[which(sums$low5==0)[1]])
close.dates$low25[reg]=as.character(sums$Date[which(sums$low25==0)[1]])
close.dates$median[reg]=as.character(sums$Date[which(sums$median==0)[1]])
close.dates$high75[reg]=as.character(sums$Date[which(sums$high75==0)[1]])
close.dates$high90[reg]=as.character(sums$Date[which(sums$high90==0)[1]])

#make plots

if(length(which(sums$median==0)>0)){
sums$median[(which(sums$median==0))[2]:length(sums$median)]<-NA  
}

today<-as.Date(Sys.time())

reg_tracker<-ggplot(sums, aes(x = Date, y = median))+
  geom_ribbon(data=sums,aes(ymin = low5, ymax = high90),fill="grey95",alpha=0.5,col="black")+
  geom_ribbon(data=sums,aes(ymin = low25, ymax = high75),fill="grey75",alpha=0.5,col="black")+ 
  geom_line(data=test4,aes(x=Date,y=value,group=variable),col="grey30", lwd = 0.1,alpha=0.2)+
  geom_line(data=sums,aes(x=Date,y = median), col="blue",lwd=1.4)+
  scale_y_continuous(labels = comma)+
  ggtitle(paste0("Harvest Prediction from Today (",sub,": ",today,")"))+
  ylab("Pounds Remaining")+
   theme_bw()

reg_tracker_full<-ggplot(subset(df.daily.catch.long,Region==names(ave_table)[reg]), aes(x = LandDate, y = Pounds))+
  geom_area(aes(fill=Region),lwd = 0.8, alpha = 0.4,lty = 1)+
  geom_line(aes(x = LandDate, y = Pounds, col = Region), lwd = 0.5)+
  geom_line(data=subset(qr_data_catch,Region==names(ave_table)[reg]),aes(x = LandDate, y = Pounds,col=Region),alpha=0.75,lwd = 1.5)+
  geom_line(data=test4,aes(x = Date, y = value, group=variable,col=Region), lwd = 0.1,alpha=0.25)+
  geom_line(data=sums,aes(x=Date,y = median), col="grey35",lwd=1.2)+
  scale_y_continuous(labels = comma)+
  xlim(as.Date(c(date.min,date.max)))+
  ggtitle("Harvest To Date")+
  ylab("Pounds Remaining")+
  xlab("Date")+
  theme_bw()


#outputs

#Tracker plots
png(filename = paste0("./Output/Plots/Harvest Prediction",sub,".png"))
grid.arrange(reg_tracker,reg_tracker_full)
dev.off()
}

#end of loop

##################
#summary plots
##################

#daily catch
ggsave(daily.catch,filename = paste0("./Output/Plots/daily_catch.png"))

#cumulative catch
ggsave(cm.catch.all,filename = paste0("./Output/Plots/cumulative_catch.png"))

#remaining pounds
ggsave(remaining.pounds,filename = paste0("./Output/Plots/remaining_pounds.png"))


#################
#tables
##################

#closure dates
write.xlsx(close.dates, paste("./Output/", "Closure_Summary","_", Sys.Date(),".xlsx", sep = ""), row.names = FALSE)

#landings summary
write.xlsx(avail, paste("./Output/", "Landings_To_Date_summary","_", Sys.Date(),".xlsx", sep = ""), row.names = FALSE)

#end of code

}


