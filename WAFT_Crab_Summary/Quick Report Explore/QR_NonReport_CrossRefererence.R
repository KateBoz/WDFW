library(tidyverse)
library(lubridate)
library(openxlsx)

# Pull in the QR data
QR <- readRDS("./Input/CommercialCrab_QuickReport_full.rdata")%>%
  filter(LandDate < "2020-05-01")

## Summarise by dealer, week, and region
QR.sumbyDate <- QR%>%
  group_by(Dealer, Owner, Region, LandDate)%>%
  summarise(TotalPoundsReported = sum(QRLanding))

QR.sumbyRegion <- QR%>%
  group_by(Dealer, Owner, Region)%>%
  summarise(TotalPoundsReported = sum(QRLanding))

# Pull in the WAFT Data
raw <- read.csv("./Input/PS Crab.csv", skip=9, header = TRUE)%>%
  filter(Fisher_Type_Code == "1")

raw$Landing_Date <- as.POSIXct(raw$Landing_Date, format = "%m-%d-%Y")

raw <- raw%>%
  filter(Landing_Date < "2020-05-01")

#enter the raw data
raw$Catch_Area_Code<-trimws(raw$Catch_Area_Code)
raw$Catch_Area_Code <- str_replace(raw$Catch_Area_Code," ", "")

raw$Split_SubUnit_Code <- str_replace(raw$Split_SubUnit_Code," ", "")
raw$Split_SubUnit_Code <-trimws(raw$Split_SubUnit_Code)

raw$Catch_Area_Code  <- paste0(raw$Catch_Area_Code, raw$Split_SubUnit_Code)

###########################################################
# merge all the data 
waft <- left_join(raw, regions, by = c("Catch_Area_Code"="Sub-area"))

#change the 2 to 2E
area2<-which(waft$Region==2 & waft$Split_SubUnit_Code=="E")
area3<-which(waft$Region==2 & waft$Split_SubUnit_Code=="W")

#look to see what area these 2 values are
#full.data[area2,]
waft$Region[area2]="2E"
waft$Region[area3]="2W"

# Add the dealer information.
waft <- left_join(waft, dealer, by = c("Dealer_License_ID" = "LicenseID"))#%>%
  #dplyr::select(Dealer_License_ID, Owner, Region, Landing_Date, Round_Lbs_Qty)

## Summarise by Dealer_License_ID, Region, Date
waft.sumbyDate <- waft%>%
  group_by(Dealer_License_ID, Owner, Region, Landing_Date)%>%
  summarise(TotalPoundsLanded = sum(Round_Lbs_Qty))

names(waft.sumbyDate) <- c("Dealer", "Owner", "Region", "LandDate", "TotalPoundsLanded")

## Summarise by region and dealer
waft.sumbyRegion <- waft%>%
  group_by(Dealer_License_ID, Owner, Region)%>%
  summarise(TotalPoundsLanded = sum(Round_Lbs_Qty))

names(waft.sumbyRegion) <- c("Dealer", "Owner", "Region", "TotalPoundsLanded")

# Join the two together by Dealer ID. 
NR.reg <- full_join(QR.sumbyRegion, waft.sumbyRegion, by = c("Dealer", "Owner", "Region"))

NR.reg$TotalPoundsReported <- replace_na(NR.reg$TotalPoundsReported, 0)
NR.reg$TotalPoundsLanded <- replace_na(NR.reg$TotalPoundsLanded, 0)

NR.reg <- NR.reg%>%
  mutate(LandingDifference = (TotalPoundsLanded - TotalPoundsReported))%>%
  arrange(Dealer, Region)

NR.reg.flags <- NR.reg%>%
  mutate(Issue = case_when(LandingDifference > 0 ~ "Non-Quick Reported Landings",
                           LandingDifference < 0 ~ "Potentially Unfiled Fish Tickets",
                           LandingDifference == 0 ~ "Perfect Reporting"))%>%
  filter(Issue != "Perfect Reporting")%>%
  arrange(Owner, Region, LandingDifference)

# Export the cross referenced QR - WAFT data as well as fish tickets for the 
# selected buyers and dates and the QR report records for the selected buyers and dates.  
##  pull out the Fish Ticket records from 10/1 through the most recent date.
FT.reference <- waft%>%
  filter(Dealer_License_ID %in% NR.reg.flags$Dealer)%>%
  arrange(Owner, Region, Landing_Date, Fish_Ticket_Num)

## Pull out the QR records for the relevant records
QR.reference <- QR%>%
  filter(Dealer %in% NR.reg.flags$Dealer)%>%
  arrange(Owner, Region, LandDate)

## Write to an excelfile to share with enforcement.
QR.WAFT.Comparison <- list("Report Flags" = NR.reg.flags,
                           "Fish Ticket Reference" = FT.reference,
                           "QR Reference" = QR.reference,
                           "Dealer Information" = dealer)

write.xlsx(QR.WAFT.Comparison, "./Output/QR-WAFT_Comparison_10-01-2019to03-15-2020.xlsx", rowNames = FALSE, colNames = TRUE)

# Look at QR-FT discrepancies by day and region.
NR.day <- full_join(QR.sumbyDate, waft.sumbyDate, by = c("Dealer", "Owner", "LandDate", "Region"))

NR.day$TotalPoundsReported <- replace_na(NR.day$TotalPoundsReported, 0)
NR.day$TotalPoundsLanded <- replace_na(NR.day$TotalPoundsLanded, 0)

NR.day <- NR.day%>%
  mutate(LandingDifference = (TotalPoundsLanded - TotalPoundsReported))%>%
  arrange(Dealer, Region, LandDate)

NR.day.flags <- NR.day%>%
  mutate(Issue = case_when(LandingDifference > 0 ~ "Non-Quick Reported Landings",
                           LandingDifference < 0 ~ "Potentially Unfiled Fish Ticket",
                           LandingDifference == 0 ~ "Perfect Reporting"))%>%
  filter(Issue != "Perfect Reporting")%>%
  arrange(Owner, Region, LandingDifference)

# Export the cross referenced QR - WAFT data as well as fish tickets for the 
# selected buyers and dates and the QR report records for the selected buyers and dates.  
##  pull out the Fish Ticket records from 10/1 through 12/31
FT.reference.day <- waft%>%
  filter(Dealer_License_ID %in% NR.day.flags$Dealer)%>%
  arrange(Owner, Region, Landing_Date, Fish_Ticket_Num)

## Pull out the QR records for the relevant records
QR.reference.day <- QR%>%
  filter(Dealer %in% NR.day.flags$Dealer)%>%
  arrange(Owner, Region, LandDate)

QR.WAFT.Comparison.day <- list("Report Flags" = NR.day.flags,
                           "Fish Ticket Reference" = FT.reference.day,
                           "QR Reference" = QR.reference.day,
                           "Dealer Information" = dealer)

write.xlsx(QR.WAFT.Comparison.day, "./Output/QR-WAFT_Comparison_daily_10-01-2019to03-15-2020.xlsx", rowNames = FALSE, colNames = TRUE)

# Pattern Parsing
## make a list with each dealer's information within its own slot
dealer.list <- split(dealer, seq(nrow(dealer)))
names(dealer.list) <- unique(dealer$LicenseID)

## Make a list of flagged reports with each buyer stored within its own element 
# Preallocate list before filling 
flag.list <- vector('list', length(unique(as.character(NR.flags$Dealer))))

for (i in seq_along(unique(as.character(NR.flags$Dealer)))) {
  flags.by.dealer <- filter(NR.flags, Dealer == unique(as.character(NR.flags$Dealer))[i])
  tmp <- list(flags.by.dealer)
  
  flag.list[i] <- tmp
}

## For each flagged report  for each buyer in the flag.list 
## determine the percent difference between reported landings and ticketed landings that  

test <- filter(NR.flags, Dealer == "128235")
test.1 <- filter(test, Region == "1")

for (j in 2:(nrow(test.1) - 1)) 
  {
  if (test.1$LandingDifference[j] == test.1$LandingDifference[j - 1] |
      test.1$LandingDifference[j] == test.1$LandingDifference[j + 1])
    {test.1$DateIssues[j] == "Mis-dated"}}


#####################################################
NR.sum <- NR.flags%>%
  group_by(Region, Issue, Owner)%>%
  summarise(SumLandingDifference = sum(LandingDifference))
  
unique(NR.flags$Owner)
  
test <- NR.flags%>%
  group_by(Owner, Region)%>%
  summarise(MeanDifference = mean(LandingDifference))

test2 <- NR.flags%>%
  group_by(Region, Issue)%>%
  summarise(SumLandingDifference = sum(LandingDifference))
