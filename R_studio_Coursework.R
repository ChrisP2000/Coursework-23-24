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



# ======== getting flights that have less than 5 mins delay visa DBI (Q Best time of the day to fly)========
DepTimeEarlyMorn <- dbGetQuery(conn, 
                 "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='600' AND DepTime <= '800' ")
DepTimeMidMorn <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='800' AND DepTime <= '1000' ")
DepTimelateMorn <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='1000' AND DepTime <= '1200' ")
DepTimeEarlyAft <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='1200' AND DepTime <= '1400' ")
DepTimeMidAft <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='1400' AND DepTime <= '1600' ")
DepTimeLateAft <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='1600' AND DepTime <= '1800' ")
DepTimeEarlyEve <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='1800' AND DepTime <= '2000' ")
DepTimeMidEve <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='2000' AND DepTime <= '2200' ")
DepTimeLateEve <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='2200' AND DepTime <= '2359' ")
DepTimeEarlyOver <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='1' AND DepTime <= '200' ")
DepTimeMidOver <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='200' AND DepTime <= '400' ")
DepTimeLateOver <- dbGetQuery(conn, 
                               "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5' AND DepTime >='400' AND DepTime <= '600' ")
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

testing <- data.frame(value=c(EarlyMornCount,MidMornCount))
print(testing)
NoDelayCountDf <- data.frame(value<- c(EarlyMornCount, MidMornCount, LateMornCount, EarlyAftCount, MidAftCount, LateAftCount, EarlyEveCount, MidEveCount ,LateEveCount, EarlyOverCount, MidOverCount, LateOverCount),
                             categories<- c("Early Morn", "Mid Morn", "Late Morn", "Early Aft", "Mid Aft", "Late Aft", "Early Eve", "Mid Eve", "Late Eve", "Early Over", "Mid Over", "Late Over") )

print(NoDelayCountDf)
#mysc<-
my_sc<-ggplot(NoDelayCountDf, aes(x=categories, y=value, fill=categories)) + 
  geom_bar(stat = "identity")+
  labs(title="Number of non delayed flights over a period of 24 hrs", x="period of the day", y="no. of flights")+
  theme(axis.text= element_text(size=11),
        plot.title= element_text(size=18),
        axis.title=element_text(size=17))+
  geom_col()+
  geom_text(aes(label = value), color="black", size=4, hjust=-0.2)+
  coord_flip()

my_sc 
#+ theme_wsj()







#======== Q Best day of the week to fly ======== 





















