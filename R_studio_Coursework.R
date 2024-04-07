library(DBI)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
# set the working directory that contains the files
setwd("/Users/christopherpang/Desktop/Main/SIM uni year 2/Programming for data science/Coursework_data/dataverse_files")

# ======== create the database ========
if (file.exists("airline2.db")) 
  file.remove("airline2.db")
conn <- dbConnect(RSQLite::SQLite(), "airline2.db")



# ======== write to the database ========
# load in the data from the csv files
airports <- read.csv("airports.csv", header = TRUE)
carriers <- read.csv("carriers.csv", header = TRUE)
planes <- read.csv("plane-data.csv", header = TRUE)
dbWriteTable(conn, "airports", airports)
dbWriteTable(conn, "carriers", carriers)
dbWriteTable(conn, "planes", planes)

for(i in c(1991:2000)) {
  ontime <- read.csv(paste0(i, ".csv"), header = TRUE)
  if(i == 2000) {
    dbWriteTable(conn, "ontime", ontime)
  } else {
    dbWriteTable(conn, "ontime", ontime, append = TRUE)
  }
}

# ======== getting flights that have less than 15 mins delay visa DBI (Q Best time of the day to fly)========
DepTimeEarlyMorn <- dbGetQuery(conn, 
                 "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='600' AND DepTime <= '800' ")
DepTimeMidMorn <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='800' AND DepTime <= '1000' ")
DepTimelateMorn <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='1000' AND DepTime <= '1200' ")
DepTimeEarlyAft <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='1200' AND DepTime <= '1400' ")
DepTimeMidAft <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='1400' AND DepTime <= '1600' ")
DepTimeLateAft <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='1600' AND DepTime <= '1800' ")
DepTimeEarlyEve <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='1800' AND DepTime <= '2000' ")
DepTimeMidEve <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='2000' AND DepTime <= '2200' ")
DepTimeLateEve <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='2200' AND DepTime <= '2359' ")
DepTimeEarlyOver <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='1' AND DepTime <= '200' ")
DepTimeMidOver <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='200' AND DepTime <= '400' ")
DepTimeLateOver <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DepTime >='400' AND DepTime <= '600' ")
print(DepTimeEarlyMorn)

EarlyMornCount <-nrow(DepTimeEarlyMorn )
MidMornCount <- nrow(DepTimeMidMorn)
LateMornCount <- nrow(DepTimelateMorn )
EarlyAftCount <- nrow(DepTimeEarlyAft)
MidAftCount <- nrow(DepTimeMidAft)
LateAftCount <- nrow(DepTimeLateAft)
EarlyEveCount <- nrow(DepTimeEarlyEve )
MidEveCount <- nrow(DepTimeMidEve)
LateEveCount <- nrow(DepTimeLateEve)
EarlyOverCount <- nrow(DepTimeEarlyOver)
MidOverCount <- nrow(DepTimeMidOver)
LateOverCount <- nrow(DepTimeLateOver)

#testing <- data.frame(value=c(EarlyMornCount,MidMornCount))
#print(testing)
NoDelayCountDf <- data.frame(value<- c(EarlyMornCount, MidMornCount, LateMornCount, EarlyAftCount, MidAftCount, LateAftCount, EarlyEveCount, MidEveCount ,LateEveCount, EarlyOverCount, MidOverCount, LateOverCount),
                             categories<- c("Early Morn", "Mid Morn", "Late Morn", "Early Aft", "Mid Aft", "Late Aft", "Early Eve", "Mid Eve", "Late Eve", "Early Over", "Mid Over", "Late Over") )

print(NoDelayCountDf)
#mysc<-
my_sc<-ggplot(NoDelayCountDf, aes(x=categories, y=value, fill=categories)) + 
  geom_bar(stat = "identity")+
  labs(title="Number of non delayed flights over a period of 24 hrs", x="period of the day", y="no. of flights with no delay")+
  theme(axis.text= element_text(size=11),
        plot.title= element_text(size=18),
        axis.title=element_text(size=17))+
  geom_col()+
  geom_text(aes(label = value), color="black", size=4, hjust=-0.2)+
  coord_flip()

my_sc 
#+ theme_wsj()







#======== Q Best day of the week to fly ======== 

# ======== getting flights that have less than 5 mins delay visa DBI (Q Best time of the day to fly)========
Mon <- dbGetQuery(conn, 
                               "SELECT DayOfWeek AS DayOfWeek FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DayOfWeek='1' ")
Tues <- dbGetQuery(conn, 
                  "SELECT DayOfWeek AS DayOfWeek FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DayOfWeek='2' ")
Wed <- dbGetQuery(conn, 
                      "SELECT DayOfWeek AS DayOfWeek FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DayOfWeek='3' ")
Thurs <- dbGetQuery(conn, 
                  "SELECT DayOfWeek AS DayOfWeek FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DayOfWeek='4' ")
Fri<- dbGetQuery(conn, 
                  "SELECT DayOfWeek AS DayOfWeek FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DayOfWeek='5' ")
Sat <- dbGetQuery(conn, 
                  "SELECT DayOfWeek AS DayOfWeek FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DayOfWeek='6' ")
Sun<- dbGetQuery(conn, 
                  "SELECT DayOfWeek AS DayOfWeek FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND DayOfWeek='7' ")

MonCount <-nrow(Mon)
TuesCount <-nrow(Tues)
WedCount <- nrow(Wed)
ThursCount <- nrow(Thurs)
FriCount <- nrow(Fri)
SatCount <- nrow(Sat)
SunCount <- nrow(Sun)
print(MonCount)
#======== Plotting the graph for best day of the week to fly ======== 

NoDelayWeekCountDf <- data.frame(value2<- c(MonCount, TuesCount, WedCount, ThursCount, FriCount, SatCount, SunCount),
                             categories2<- c("Mon" ,"Tues", "Wed", "Thurs", "Fri", "Sat", "Sun") )

print(NoDelayWeekCountDf)

my_sc2<-ggplot(NoDelayWeekCountDf, aes(x=factor(categories2, level=categories2), y=value2, fill=categories2)) + 
  geom_bar(stat = "identity")+
  labs(title="Number of non delayed flights over a period of a week", x="day of the week", y="no. of flights with no delay")+
  theme(axis.text= element_text(size=11),
        plot.title= element_text(size=18),
        axis.title=element_text(size=17))+
  geom_col()+
  geom_text(aes(label = value2), color="black", size=4, hjust=5)+
  coord_flip()

my_sc2 





#======== Q Best time of the year to fly ======== 
Jan <- dbGetQuery(conn, 
                 " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='1' ")
Feb <- dbGetQuery(conn, 
                  " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='2' ")
March <- dbGetQuery(conn, 
                  " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='3' ")
Apr <- dbGetQuery(conn, 
                  " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='4' ")
May <- dbGetQuery(conn, 
                  " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='5' ")
June <- dbGetQuery(conn, 
                  " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='6' ")
July <- dbGetQuery(conn, 
                  " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='7' ")
Aug <- dbGetQuery(conn, 
                  " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='8' ")
Sep <- dbGetQuery(conn, 
                  " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='9' ")
Oct <- dbGetQuery(conn, 
                  " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='10' ")
Nov <- dbGetQuery(conn, 
                  " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='11' ")
Dec <- dbGetQuery(conn, 
                  " SELECT Month AS Month FROM ontime WHERE ArrDelay < '15' AND DepDelay < '15' AND month ='12' ")


CountJan <-nrow(Jan )
CountFeb <- nrow(Feb)
CountMarch <- nrow(March)
CountApr <- nrow(Apr)
CountMay <- nrow(May)
CountJune <- nrow(June)
CountJuly <- nrow(July)
CountAug <- nrow(Aug)
CountSep <- nrow(Sep)
CountOct <- nrow(Oct)
CountNov <- nrow(Nov)
CountDec <- nrow(Dec)


#======== Plotting the graph for best time of the year to travel ======== 
custom_colors <- c("Jan" = "skyblue", "Feb" = "lightgreen", "March" = "orange",
                   "April" = "pink", "May" = "lightblue", "June" = "yellow",
                   "July" = "lightcoral", "Aug" = "lightskyblue", "Sep" = "lightgreen",
                   "Oct" = "lightpink", "Nov" = "lightgray", "Dec" = "lightyellow")

my_sc3 <- ggplot(NoDelayWeekCountDf, aes(x = factor(categories3, level = categories3), y = value3, fill = categories3)) + 
  geom_bar(stat = "identity") +
  labs(title = "Number of Non-Delayed Flights Over a Period of a Week", x = "Day of the Week", y = "Number of Flights with No Delay") +
  theme(axis.text = element_text(size = 11, family = "serif"),
        plot.title = element_text(size = 18, family = "serif"),
        axis.title = element_text(size = 17, family = "serif")) +
  geom_col() +
  geom_text(aes(label = value3), color = "black", size = 4, hjust = 5) +
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  coord_flip()

my_sc3

##======== Plotting the graph for do older planes suffer on year to year basis ======== 
query <- " SELECT CAST(ontime.TailNum AS INTEGER) AS TailNum
          FROM ontime
          WHERE ontime.ArrDelay < 15
"
result <- dbGetQuery(conn, query)
print(result)

print(ontime)
datesOfPlane <- as.data.frame(result)
datesOfPlane$Year <- as.integer(ifelse(is.na(datesOfPlane$Year), NA, 
                                       ifelse(grepl("^\\d+$", datesOfPlane$Year), 
                                              2024 - as.integer(datesOfPlane$Year), 
                                              NA)))



#SELECT CAST(planes.year AS INTEGER) AS year
#FROM planes
#JOIN ontime ON planes.tailnum = ontime.TailNum
#WHERE CAST(ontime.ArrDelay AS INTEGER) < 15
#AND CAST(ontime.DepDelay AS INTEGER) < 15
#AND planes.year IS NOT NULL
# Group by year and count the occurrences
delay_counts <- datesOfPlane %>% 
  filter(!is.na(Year)) %>% 
  group_by(Year) %>%
  summarise(Count = n())

# Plot the graph
ggplot(delay_counts, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = 'Number of No Delays per Year',
       x = 'Age of the Plane',
       y = 'Number of No Delays') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlim(10, 70) +
  theme(axis.title = element_text(size = 13, family = 'serif'),
        axis.text = element_text(size = 10, family = 'serif'))