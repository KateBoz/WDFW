raw<-read.csv("./Input/PS Crab.csv", skip=9,header = T)

#enter the raw data
raw$Catch_Area_Code<-str_replace(raw$Catch_Area_Code," ", "")
raw$Catch_Area_Code<-paste0(raw$Catch_Area_Code,raw$Split_SubUnit_Code)

###########################################################
# Pull out only state commercial data. 
raw <- raw%>%
  filter(Fisher_Type_Code == "1")

###########################################################
# merge all the data 
full.data <- left_join(raw, regions, by = c("Catch_Area_Code"="Sub-area"))%>%
  filter(Fisher_Type_Code == "1")

full.data$Landing_Date_2 = as.Date(full.data$Landing_Date, format = "%m-%d-%Y")

# bring in the harvester reports
hvr <- read.xlsx("./Input/2E_December_Registrants.xlsx")

# Unnest the permits from a single cell to multiple.
hvr.long <- hvr%>%
  separate_rows(Permit,sep="-")

hvr.long$Permit <- as.character(hvr.long$Permit)

# Pull out the list of tickets for the Dec 2E opening.
FT.2E.Dec <- full.data%>%
  filter(Region == "2E" & Landing_Date_2 > "2019-12-09")

FT.2E.Dec$License_ID <- as.character(FT.2E.Dec$License_ID) 

# Cross reference list of registered harvesters with the list of harvesters that landed.
## Select values that do not appear in the harvester list from those that were landed (permit number).

unregistered <- anti_join(FT.2E.Dec, hvr.long, by = c("License_ID" = "Permit"))
unique(unregistered$License_ID) 

# Cross reference the unregistered permits with a recent permit list. 

