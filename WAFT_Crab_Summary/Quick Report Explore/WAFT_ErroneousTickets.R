raw<-read.csv("./Input/PS Crab.csv", skip=9,header = T)
head(raw)
sapply(raw,class)

#run the code

#enter the raw data
raw$Catch_Area_Code<-str_replace(raw$Catch_Area_Code," ", "")

###########################################################
# Pull out only state commercial data. 
raw <- raw%>%
  filter(Fisher_Type_Code == "1")

###########################################################
# merge all the data 
full.data <- left_join(raw, regions, by = c("Catch_Area_Code" = "Sub-area"))

#change the 2 to 2E
area2 <- which(full.data$Region==2 & full.data$Split_SubUnit_Code=="E")
area3 <- which(full.data$Region==2 & full.data$Split_SubUnit_Code=="W")

#look to see what area these 2 values are
#full.data[area2,]
full.data$Region[area2]="2E"
full.data$Region[area3]="2W"

full.data$Landing_Date_2 = as.Date(full.data$Landing_Date, format = "%m-%d-%Y")

## pull out fish tickets that are likely erroneous. 

problem.tickets1 <- full.data%>%
  filter(Region == "1" & Landing_Date_2 > "2019-11-09")

problem.tickets2 <- full.data%>%
  filter(Region == "2E")%>%
  filter(Landing_Date_2 > "2019-10-10" & Landing_Date_2 < "2019-12-10")

problem.tickets3 <- full.data%>%
  filter(Region == "2E")%>%
  filter(Landing_Date_2 > "2019-12-15" & Landing_Date_2 < "2020-01-13")

problem.tickets4 <- full.data%>%
  filter(Region %in% c("4", "5"))

problem.tickets5 <- full.data%>%
  filter(Region == "3-2")%>%
  filter(Landing_Date_2 > "2019-12-23")

problem.tickets <- rbind(problem.tickets1, problem.tickets2, problem.tickets3, problem.tickets4, problem.tickets5)

write.xlsx(problem.tickets, paste("./Output/", "ProblemTickets","-", Sys.Date(),".xlsx", sep = ""), 
           row.names = FALSE)
