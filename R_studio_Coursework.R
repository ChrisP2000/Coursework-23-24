library(DBI)
library(dplyr)
library(stringr)
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
DepTime <- dbGetQuery(conn, 
                 "SELECT DepTime AS DepTime FROM ontime WHERE ArrDelay < '5' AND DepDelay < '5'")
countEarlyMorn<- 0
print(countEarlyMorn)
#& time <= 800
for (time in DepTime) {
    #print(typeof(time))
    ifelse( time >= 600 & time <= 800,countEarlyMorn<-countEarlyMorn+1, next )
     
}
print(countEarlyMorn)
