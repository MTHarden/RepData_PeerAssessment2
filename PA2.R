## Download and unzip the data the data file
library(R.utils)
temp <- tempfile()
fileUrl = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile = temp, mode = "wb")
bunzip2(temp,"repdata-data-StormData.csv", overwrite = TRUE, remove = FALSE)
unlink(temp)

## Manipulate data 
library(dplyr)
StormData <- read.csv("repdata-data-StormData.csv")
tbl_StormData <- tbl_df(StormData)
## Group by Event Type (EVTYPE)
by_EVTYPE_health <- group_by(tbl_StormData, EVTYPE)
##Find summary of cases, and arrange in descending order
PublicHealthSummary <- summarize(by_EVTYPE_health, total_injuries_fatalities = sum(FATALITIES, INJURIES))
PublicHealthSummary <- arrange(PublicHealthSummary, desc(total_injuries_fatalities))
## Take the top 10
TotPubHealthSum <- PublicHealthSummary[1:10, ]

## Multiply the property dand crop amage and copy damage cost by factor to put value in $1B
tbl_StormData$PROPDMGAct <- with(tbl_StormData, 
                             ifelse(PROPDMGEXP == "K", PROPDMG/1e+06,
                                    ifelse(PROPDMGEXP == "M", PROPDMG/1000,
                                           ifelse(PROPDMGEXP == "B", PROPDMG, 
                                                  0*PROPDMG))))
tbl_StormData$CROPDMGAct <- with(tbl_StormData, 
                             ifelse(CROPDMGEXP == "K", CROPDMG/1e+06,
                                    ifelse(CROPDMGEXP == "M", CROPDMG/1000,
                                           ifelse(CROPDMGEXP == "B", CROPDMG, 
                                                  0*CROPDMG))))
## Group by EVTYPE
by_EVTYPE_econ <- group_by(tbl_StormData, EVTYPE)
##Find summary of cases, and arrange in descending order
EconConseqSummary <- summarize(by_EVTYPE_econ, total_economic_damage = sum(PROPDMGAct, CROPDMGAct))
EconConseqSummary <- arrange(EconConseqSummary, desc(total_economic_damage))
## Take the top 10
TotEconConseqSum <- EconConseqSummary[1:10, ]


##-----------------

library(ggplot2)
TotPubHealthSum
q <- qplot(EVTYPE, data = TotPubHealthSum, geom = "bar", weight = total_injuries_fatalities,
           main = "Weather Events that Affect Public Health Most",
           xlab = "Weather Event Type",
           ylab = "Total Injuries and Fatalities")
q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

TotEconConseqSum
q <- qplot(EVTYPE, data = TotEconConseqSum, geom = "bar", weight = total_economic_damage,
           main = "Weather Events which Have the Greatest Economic Consequence",
           xlab = "Weather Event Type",
           ylab = "Total Property and Crop Damage (in $1B)")
q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

## Because RSTudio is buggy
library(knitr)
## WD must be set: setwd("~/RepData_PeerAssessment2")
knit2html("PA2.Rmd")