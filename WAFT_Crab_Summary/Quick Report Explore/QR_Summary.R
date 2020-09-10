# Summarise the QR list

#run the code
{ 
library(openxlsx)
library(tidyverse)
library(lubridate)

## Load the R file in. 
QR <- readRDS("./Input/CommercialCrab_QuickReport_full.rdata")

## Summarise
QR.region.summary <- QR%>%
  mutate(LandDate = as.Date(LandDate))%>%
  group_by(Region, LandDate)%>%
  summarise(DailyLandings = sum(QRLanding))%>%
  ungroup()%>%
  complete(Region, 
           LandDate = seq.Date(min(LandDate, na.rm = TRUE), max(LandDate, na.rm = TRUE), by = "day"), # fill the date frame wil all dates for every region and if NA fill with 0. 
           fill = list(DailyLandings = 0))%>%
  arrange(LandDate, Region)

QR.region.summary <- spread(QR.region.summary, Region, DailyLandings, fill = 0)

QR.Buyer.Dates <- QR%>%
  group_by(Owner, ReportDate, LandDate, `Region-SubArea`, Comments)%>%
  summarise()%>%
  mutate(ReportDelay = as.integer(ReportDate - LandDate, units = "days"))%>%
  arrange(ReportDate)

Late.Reports <- QR.Buyer.Dates%>%
  filter(ReportDelay >= 2)%>%
  arrange(Owner)

write.xlsx(Late.Reports, paste("./Output/", "Commercial Crab_Late Quick Reports","_", Sys.Date(),".xlsx", sep = ""), 
          row.names = FALSE)
write.xlsx(QR.region.summary, paste("./Output/", "Commercial Crab-Daily","QR_Landings by Area","_", Sys.Date(),".xlsx", sep = ""), 
          row.names = FALSE)
}
