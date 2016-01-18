# Data Processing
## Load and preprocess the data
### Store the URL for data

fileURL <- c("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FstormData.csv.bz2")
docURL <- c("https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf")


### Store filenames and paths as varriables

fileName <- "~/GitHub/RepData/PA_2/repdata-data-stormData.csv"
zipFileName <- "~/GitHub/RepData/PA_2/repdata-data-stormData.csv.bz2"
docFileName <- "~/GitHub/RepData/PA_2/repdata-peer2_doc-pd01016005curr.pdf"
faqFileName <- "~/GitHub/RepData/PA_2/repdata-peer2_doc-NCDC Storm Events-FAQ Page.pdf"


### Check to see if file exists. Download and extract only if it does not

library(R.utils, quietly = TRUE)
if (!file.exists(fileName)) {
        download.file(fileURL, destfile = zipFileName, method = "curl")
        bunzip2(file = zipFileName, fileName)
        date = Sys.Date()
}


### Create data frame, reduce the complexity of the data by subsetting only the data needed for this report. Rename columns

library(dplyr) 
stormData <- read.csv(fileName, na.strings = " ")
stormData <- subset(stormData, select = c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "CROPDMG"))
stormData[is.na(stormData)] <- 0
colnames(stormData) <- c("Event", "Fatalities", "Injuries", "PropDmg", "CropDmg")
propDmg <- summarize(group_by(stormData, Event), sum(PropDmg))
colnames(propDmg) <- c("Event", "PropDmg")
cropDmg <- summarize(group_by(stormData, Event), sum(CropDmg))
colnames(cropDmg) <- c("Event", "CropDmg")
fatal <- summarize(group_by(stormData, Event), sum(Fatalities))
colnames(fatal) <- c("Event", "Fatalities")
injury <- summarize(group_by(stormData, Event), sum(Injuries)) 
colnames(injury) <- c("Event", "Injuries")


### Order data frames and get summary assesments for reporting and further analysis. 

summaryData <- Reduce(function(...) merge(..., all=TRUE), list(propDmg, cropDmg, fatal, injury))
top5PropDmg <- propDmg[with(propDmg, order(-PropDmg)),][1:5,] # order data frame and subset top 5
top5CropDmg <- cropDmg[with(cropDmg, order(-CropDmg)),][1:5,] # order data frame and subset top 5
top5Fatal <- fatal[with(fatal, order(-Fatalities)),][1:5,] # order data frame and subset top 5
top5Injury <- injury[with(injury, order(-Injuries)),][1:5,] # order data frame and subset top 5
economic <- Reduce(function(...) merge(..., all=TRUE), list(top5PropDmg, top5CropDmg))
economic <- mutate(economic, TotalDmg = (economic$PropDmg + economic$CropDmg))
health <- Reduce(function(...) merge(..., all=TRUE), list(top5Fatal, top5Injury))
health <- mutate(health, TotalHealth = (health$Fatalities + health$Injuries))

## Results
### Across the United States, which types of events are most harmful with respect to population health?

health[with(health, order(-TotalHealth)),][1:5,]

### Table 1: Summary assesments of the top public health impacts

healthData <- melt(topHealth, id="Event")
healthData[is.na(healthData)] <- 0
ggplot(healthData, aes(x=Event, y=factor(value), fill = variable)) +
        geom_bar(stat="identity", position = "identity") + facet_wrap(~variable) + 
        theme(axis.text.x =
                      element_text(size  = 10,
                                   angle = 45,
                                   hjust = 1,
                                   vjust = 1))

### Figure 1: Representation of the public health data showing the events contributing to injuries, fatalities and total (injuries + fatalities) health impact.

### Across the United States, which types of events have the greatest economic consequences?


economic[with(economic, order(-TotalDmg)),][1:5,]

### Table 2: Summary assesments of economic impacts 


library(data.table)
library(ggplot2)
damageData <- melt(topEconomic, id="Event")
damageData[is.na(damageData)] <- 0
ggplot(damageData, aes(x=Event, y=factor(value), fill = variable)) +
        geom_bar(stat="identity", position = "identity") + facet_wrap(~variable) + 
        theme(axis.text.x =
                      element_text(size  = 10,
                                   angle = 45,
                                   hjust = 1,
                                   vjust = 1))

### Figure 2: Representation of the economic data showing the events contributing to property damage, crop damage and total (property damage + crop damage) economic impact.