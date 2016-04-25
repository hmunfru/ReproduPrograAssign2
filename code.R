library(R.utils)  
library(plyr)
library(ggplot2)
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
storm_data_bz <- "storm_data_bz.csv.bz2"
storm_data_csv <- "storm_data_bz.csv"
directory <- "StormData"
storm_data_path_bz <- file.path(directory, storm_data_bz)
storm_data_path <- file.path(directory, storm_data_csv)

if (!dir.exists(directory)){
    dir.create(directory)
}
if (!file.exists(storm_data_path_bz)) {
    download.file(url, storm_data_path_bz)
}
if (!file.exists(storm_data_path)) {
    bunzip2(storm_data_path_bz, storm_data_path, remove=FALSE)  
}

data = read.csv(storm_data_path) 
str(data)
head(data)
colnames(data)

events_health <- subset(data, select = c("EVTYPE", "FATALITIES", "INJURIES"))


events_health$damage <- events_health$FATALITIES + events_health$INJURIES


events_economic <- subset(data, select = c("EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMGEXP",  "CROPDMGEXP"))


levels(events_economic$PROPDMGEXP) <- c("1", "1", "1", "1", "1", "1", "1", "1", "1", 
                                  "1", "1", "1", "1", "1000000000", "1", "1", "1000", "1000000", "1000000")
levels(events_economic$CROPDMGEXP) <- c("1", "1", "1", "1", "1000000000", "1000", 
                                  "1000", "1000000", "1000000")
events_economic$PROPDMG <- events_economic$PROPDMG * as.integer(events_economic$PROPDMGEXP)
events_economic$CROPDMG <- events_economic$CROPDMG * as.integer(events_economic$CROPDMGEXP)
events_economic$damage <- events_economic$PROPDMG + events_economic$CROPDMG



### ALGUNOS EVENTOS TIENEN ABREVIACIONES
total <- aggregate(damage ~ EVTYPE, events_health, FUN=sum, na.rm=TRUE)   
## sort and filter top only    
total <- arrange(total, desc(damage))[1:12,]
total <- reorder(total$EVTYPE, -(total$damage)) 
ggplot(data=total, aes(x=EVTYPE, y=damage)) +
  geom_bar(stat="identity") +
  labs(x="Event Type", y="Number of Population Affected",  
       title="Most Harmful Events to Population Health")
print(gp) 

total <- aggregate(damage ~ EVTYPE, events_economic, FUN=sum, na.rm=TRUE)   
## sort and filter top only    
total <- arrange(total, desc(damage))[1:12,]
total <- reorder(total$EVTYPE, -(total$damage)) 
ggplot(data=total, aes(x=EVTYPE, y=damage)) +
  geom_bar(stat="identity") +
  labs(x="Event Type", y="Economic",  
       title="Most Harmful Events to Population Health")
print(gp) 





