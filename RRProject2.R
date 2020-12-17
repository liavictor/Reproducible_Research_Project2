### R code for Reproducible Research Project #2
## downloadd, unzip file and read csv  file
## 1. Code for reading  csv.bz2 into dataset and/or processing the data
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destFile <- "repdata_data_StormData.csv.bz2"
if (!file.exists(destFile)){
        download.file(URL, destfile = destFile, mode='wb')
}

StormData <- read.csv("repdata_data_StormData.csv.bz2")

## List of related variables for analyisis on health and economy impact:

# Analysis Health related variables:
        # FATALITIES: death population
        # INJURIES: injury population

# Analysis Economic related variables:
        
        # PROPDMG: Property damage value
        # PROPDMGEXP: Unit of property damage 
        # CROPDMG: Crop damage value
        # CROPDMGEXP: Unit of crop damage 

# Anaysis Key event variable: 
        #EVTYPE: weather event

### loading required libraries 
library(tidyverse)
library(sqldf)
library(ggplot2)

## processing data:
        # create working sub-dataset with only related for analysis.
        # convert property and crop damage values from billions/millions/others into dollar

AnaData <- sqldf('select 
                case when EVTYPE in ("FLASH FLOOD","RIVER FLOOD") then "FLOOD" 
                        else EVTYPE end as EVTYPE, 
                FATALITIES, INJURIES, PROPDMG, PROPDMGEXP,
                case when PROPDMGEXP in ("K","k","3") then 1000
                     when PROPDMGEXP in ("M","m","6") then 1000000 
                     when PROPDMGEXP in ("B","b","9") then 1000000000
                     when PROPDMGEXP in ("H","h","2") then 100
                     when PROPDMGEXP in ("","0") then 1
                     when PROPDMGEXP="4" then 10000 
                     when PROPDMGEXP="5" then 100000
                     when PROPDMGEXP="7" then 10000000
                     when PROPDMGEXP="8" then 100000000
                     when PROPDMGEXP="+" then 1.01
                     when PROPDMGEXP="-" then 0.99 
                     else 0 end as prop_rate,
                CROPDMG, CROPDMGEXP,
               case when CROPDMGEXP in ("K","k","3") then 1000
                     when CROPDMGEXP in ("M","m","6") then 1000000 
                     when CROPDMGEXP in ("B","b","9") then 1000000000
                     when CROPDMGEXP in ("H","h","2") then 100
                     when CROPDMGEXP in ("","0") then 1
                     when CROPDMGEXP="4" then 10000 
                     when CROPDMGEXP="5" then 100000
                     when CROPDMGEXP="7" then 10000000
                     when CROPDMGEXP="8" then 100000000
                     when CROPDMGEXP="+" then 1.01
                     when CROPDMGEXP="-" then 0.99 
                     else 0 end as crop_rate
                from StormData  ')

AnaData <- sqldf('select *, prop_rate*PROPDMG as propdmg_dollar, 
                        CROPDMG*crop_rate as cropdmg_dollar from AnaData ')

### Create new event type by grouping the similar events: logics used in below:

## + if EVTYPE contain "HURRICANE" then newEVTYPE = "HURRICANE"
## + else if EVTYPE contain "TORNADO" then newEVTYPE = "TORNADO" 
## + else if EVTYPE contain "FLOOD" then newEVTYPE = "FLOOD"
## + else if EVTYPE contain "LIGHTNING" then newEVTYPE = "LIGHTNING"
## + else if EVTYPE contain "HEAT" then newEVTYPE = "HEAT"
## + else if EVTYPE contain "HAIL" then newEVTYPE = "HAIL"
## + else if EVTYPE contain "STORM" then newEVTYPE = "STORM"
## + else if EVTYPE contain "SNOW" then newEVTYPE = "SNOW"
## + else if EVTYPE contain "RAIN" then newEVTYPE = "RAIN"
## + else if EVTYPE contain "WIND" then newEVTYPE = "WIND"
## + else if EVTYPE contain "WINTER" then newEVTYPE ="WINTER"
## + else newEVTYPE = EVTYPE

AnaData$newEVTYPE <- "OTHER"
AnaData$newEVTYPE[grep("SURF", AnaData$EVTYPE, ignore.case = TRUE)] <- "SURF"
AnaData$newEVTYPE[grep("TIDE", AnaData$EVTYPE, ignore.case = TRUE)] <- "TIDE"
AnaData$newEVTYPE[grep("DUST", AnaData$EVTYPE, ignore.case = TRUE)] <- "DUST/FOG"
AnaData$newEVTYPE[grep("FOG", AnaData$EVTYPE, ignore.case = TRUE)] <- "DUST/FOG"
AnaData$newEVTYPE[grep("RAIN", AnaData$EVTYPE, ignore.case = TRUE)] <- "RAIN"
AnaData$newEVTYPE[grep("DRY", AnaData$EVTYPE, ignore.case = TRUE)] <- "DRY"
AnaData$newEVTYPE[grep("LOW", AnaData$EVTYPE, ignore.case = TRUE)] <- "WINTER/COLD"
AnaData$newEVTYPE[grep("COOL", AnaData$EVTYPE, ignore.case = TRUE)] <- "WINTER/COLD"
AnaData$newEVTYPE[grep("FROST", AnaData$EVTYPE, ignore.case = TRUE)] <- "WINTER/COLD"
AnaData$newEVTYPE[grep("FREEZE", AnaData$EVTYPE, ignore.case = TRUE)] <- "WINTER/COLD"
AnaData$newEVTYPE[grep("COLD", AnaData$EVTYPE, ignore.case = TRUE)] <- "WINTER/COLD"
AnaData$newEVTYPE[grep("WINTER", AnaData$EVTYPE, ignore.case = TRUE)] <- "WINTER/COLD"
AnaData$newEVTYPE[grep("FIRE", AnaData$EVTYPE, ignore.case = TRUE)] <- "FIRE"
AnaData$newEVTYPE[grep("WIND", AnaData$EVTYPE, ignore.case = TRUE)] <- "WIND"
AnaData$newEVTYPE[grep("SNOW", AnaData$EVTYPE, ignore.case = TRUE)] <- "SNOW"
AnaData$newEVTYPE[grep("RIP CURRENT", AnaData$EVTYPE, ignore.case = TRUE)] <- "RIP CURRENT"
AnaData$newEVTYPE[grep("STORM", AnaData$EVTYPE, ignore.case = TRUE)] <- "STORM"
AnaData$newEVTYPE[grep("HAIL", AnaData$EVTYPE, ignore.case = TRUE)] <- "HAIL"
AnaData$newEVTYPE[grep("HEAT", AnaData$EVTYPE, ignore.case = TRUE)] <- "HEAT"
AnaData$newEVTYPE[grep("RECORD HIGH", AnaData$EVTYPE, ignore.case = TRUE)] <- "HEAT"
AnaData$newEVTYPE[grep("DROUGHT", AnaData$EVTYPE, ignore.case = TRUE)] <- "DROUGHT"
AnaData$newEVTYPE[grep("BLIZZARD", AnaData$EVTYPE, ignore.case = TRUE)] <- "BLIZZARD"
AnaData$newEVTYPE[grep("AVALANC", AnaData$EVTYPE, ignore.case = TRUE)] <- "AVALANCHE"
AnaData$newEVTYPE[grep("LIGHTING", AnaData$EVTYPE, ignore.case = TRUE)] <- "LIGHTNING"
AnaData$newEVTYPE[grep("LIGHTNING", AnaData$EVTYPE, ignore.case = TRUE)] <- "LIGHTNING"
AnaData$newEVTYPE[grep("FLOOD", AnaData$EVTYPE, ignore.case = TRUE)] <- "FLOOD"
AnaData$newEVTYPE[grep("TORNADO", AnaData$EVTYPE, ignore.case = TRUE)] <- "TORNADO"
AnaData$newEVTYPE[grep("HURRICANE", AnaData$EVTYPE, ignore.case = TRUE)] <- "HURRICANE"


## Frist objective from asssignment: Across the United States, which types of events 
## (as indicated in the \color{red}{\verb|EVTYPE|}EVTYPE variable) 
## are most harmful with respect to population health?

## The harmful with respect to population health can be valued by the number of death and injuries 
## during the harmful events. Following analysis steps to find out the most harmful events. 
## step 1: get top 8 harmful events in death
## step 2: get top 8 harmful events in injury
## step 3: get top 8 harmful events with both death and injury
## step 4: binding data from step 1-3 and make plot to show the most harmful events.

### processing data for the frist objective:
q1adf <-  sqldf('select newEVTYPE, "- Fatalities" as PopulationType, sum(FATALITIES) as "value" 
              from AnaData group by newEVTYPE, PopulationType') %>% top_n(8, value)
q1adi <-  sqldf('select newEVTYPE, "- Injuries" as PopulationType, sum(INJURIES) as "value" 
              from AnaData group by newEVTYPE, PopulationType') %>% top_n(8, value)
q1adt <-  sqldf('select newEVTYPE, "- Total=Fatalities+Injuries" as PopulationType, 
                sum(FATALITIES+INJURIES) as "value" 
              from AnaData group by newEVTYPE, PopulationType') %>% top_n(8, value)

q1AnaData <- rbind(q1adf, q1adi, q1adt)

### Plot the Result for the frist objective:
ggplot(q1AnaData,aes(x = newEVTYPE, y = value)) + 
        geom_bar(aes(fill = PopulationType),stat = "identity",position = "dodge") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.5)) +
        labs(x = "Harmful Events")+
        labs(y = " # of Fatalities / Injuries from year 1950 to 2011 ")+
        labs(title = "Fatalities and Injuries by population type and harmful event")

### Question 2:  Across the United States, 
## which types of events have the greatest economic consequences?

q2adp <-  sqldf('select newEVTYPE, "- Property" as DamageType, sum(propdmg_dollar) as "value" 
              from AnaData group by newEVTYPE, DamageType') %>% top_n(8, value)
q2adc <-  sqldf('select newEVTYPE, "- Crop" as DamageType, sum(cropdmg_dollar) as "value" 
              from AnaData group by newEVTYPE, DamageType') %>% top_n(8, value)
q2adt <-  sqldf('select newEVTYPE, "- Total=Crop+Property" as DamageType, 
                sum(propdmg_dollar+cropdmg_dollar) as "value" 
              from AnaData group by newEVTYPE, DamageType') %>% top_n(8, value)

q2AnaData <- rbind(q2adp, q2adc, q2adt)


### Plot the Result for the second objective:
ggplot(q2AnaData,aes(x = newEVTYPE, y = value/1000000000)) + 
        geom_bar(aes(fill = DamageType),stat = "identity",position = "dodge") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.6)) +
        labs(x = "Harmful Events")+
        labs(y = "Total damage in billions from 1950 to 2011")+
        labs(title = "Economic damage by damage type and harmful event")







