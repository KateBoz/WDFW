###########################################################################
#
# Objective: The purpose of this script is to connect the quick report database, 
# merge the separate tables together into one dataframe, do a QA/QC check and 
# create a r object to export for performing the summary report.
# 
# Created by Daniel Sund and Katelyn Bosley
############################################################################
#enter inputs for the code

#Ndays switch - This number sets the number of days that the QR data is used. Default is 12
ndays = 8

#xaxis limit for harvest tracker
date.min = "2019-10-01"
date.max = "2020-05-20"

#number of days for the running average
days_average = 5

#number of simulations to conduct
nsims = 1000

#probability function for harvest predictor - functions allow only values between 0(no harvest) and 1(100% remaining taken). 
pdf.fun = 4
  # 1 == uniform probability. makes no assumptions on likilhood daily of harvest activity but assumes it is equal to or less than day_average
  # 2 == beta probability. Assumes daily harvest activity is will be similar or less than to the day_average amount
  # 3 == lognormal probability. Random probability of harvest greater to or less than day_average
  # 4 == gamma probability. Random probability of harvest greater to or less than day_average uses. PDF fitting to determine shape parameters. Shape parameters are set for each area separately.

#if pdf function 2 or 3 is selected - define the shape parameters
sh1 = 2.5
sh2 = 2


#create a plots directory
#dir.create("./Output/Plots")

##############################
#run the code
##############################
{
library(RODBC)
library(tidyverse)
library(openxlsx)
  
## Bring in the datebase
accDB <- "S://Reg6-PTT/FP/Crustacean/Crab/2019-2020_CRAB_CATCH_ACCOUNTING/Database/Commercial Crab - Quick Reports_backend.accdb"

con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", accDB))

# Identify the tables in the database.
sqlTables(con, tableType = "TABLE")

# Import the tables that hold the data that will be analyzed.
## Bring in the QuickReport Header table which hold the desciptive information for each report.
QR_head <- sqlQuery(con, "SELECT * FROM tbl_QuickReports_Header")
glimpse(QR_head)

## Bring in the QuickReport table which hold the landing and region information for each report.
QR_body <- sqlQuery(con, "SELECT * FROM tbl_QuickReports")
glimpse(QR_body)

## Bring in the Dealer information. 
dealer <- sqlQuery(con, "SELECT * FROM lu_Dealer_Information")
glimpse(dealer)

##Bring in the regions
regions <- sqlQuery(con, "SELECT * FROM lu_Harvest_Areas")
glimpse(regions)

close(con)

# Join the tables together to get one dataframe.
## Join header to individual reports.
QR.int1 <- left_join(QR_head, QR_body, by = "QRHeaderID")

## Add the dealer information.
QR.int2 <- left_join(QR.int1, dealer, by = c("Dealer" = "LicenseID"))

## Join the harvest area to the interim df
QR <- left_join(QR.int2, regions, by = c("Region" = "Region-SubArea"))


## Clean up the field names
colnames(QR)[colnames(QR)=="Region"] <- "Region-SubArea"
colnames(QR)[colnames(QR)=="Region.y"] <- "Region"

#QR <- QR%>%
#  select(Dealer, Owner, Region, `Sub-area`, `Region-SubArea`, ReportDate, ReportMethod, LandDate, QRLanding, Ticket, Comments)%>%
#  arrange(LandDate, Region)

levels(QR$Region)

## Look for duplicate records
QR.dups <- QR%>%
  dplyr::select(-ReportMethod, -QRHeaderID, -QRReportID, -Comments, -Ticket, -ReportDate)

table(duplicated(QR.dups))
QR.dups <- QR.dups[which(duplicated(QR.dups) == TRUE),]

QR.dups <- QR.dups%>%
  left_join(QR)

## Remove duplicate flags for verified non-duplicates.
QR.dups <- QR.dups%>%
  filter(!QRHeaderID %in% c("649"))
  
##Pull out problematic records for correction
### Pull out the unkown records to flag for correction
QR.unk <- QR[QR$Dealer == "-999999",]

### Pull out records where the date the report recieved miraculously occured before the landing.
QR.date.problems <- QR%>%
  filter(ReportDate < LandDate)

### Pull out records without pounds landing
QR.empties <- QR[is.na(QR$QRLanding) | QR$QRLanding == 0,]

### Look for other empty fields
QR.empties1 <- QR%>%
  filter(is.na(ReportDate) | is.na(LandDate) | is.na(`Region-SubArea`) | is.na(QRReportID))

## Write the full file.
write.xlsx(QR, paste("./Output/", "Commercial Crab-Daily-Quick Reports","-", Sys.Date(),".xlsx", sep = ""), 
           row.names = FALSE)

## Export the data as an R object for use for summaries.
saveRDS(QR, paste("./Input/", "CommercialCrab_QuickReport_full.rdata", sep = ""))

#run the next code for complete summary
source("QR_summary.R")
source("WAFT_Run.R")
source("Harvest_tracker.R")
}
