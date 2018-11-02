StormData <- read.csv("StormData")
head(StormData)
library(dplyr)
library(ggplot2)
library(knitr)

StormData_df <- tbl_df(StormData) #transform to df for dplyr ops


#Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
SDby_EV <- group_by(StormData_df, EVTYPE)    #group all same EV rows
SDby_EV_sum <- mutate(SDby_EV, total_aff = FATALITIES + INJURIES) #add a variable for total health_loss
SDby_EV_mod <- select(SDby_EV_sum,EVTYPE, FATALITIES, INJURIES, total_aff) #Pull out the columns we need,
#like EVTYPE, FATALITIES, INJURIES
SDby_EV_sorted <- summarize(SDby_EV_mod, health_loss = sum(total_aff)) #gives just total healthloss for each EVTYPE

##A method to get top 1% disastor events by health_loss, happens to yield top 10 disastors
quantile(SDby_EV_sorted$health_loss, probs = 0.99) #Finds top 1% amount for total health loss
Top_Ten1 <- filter(SDby_EV_sorted, health_loss > 1400) #Yields top 1% events with most health losses, in this case greater than 1400
Top_Ten1 <- arrange(Top_Ten, desc(health_loss)) #Sort in desc order
ggplot(Top_Ten, aes(x=EVTYPE, y=health_loss)) + geom_bar(stat="identity")

####The Largest cause of population health damage was by Tornado.


#Across the United States, which types of events have the greatest economic consequences?

SD_ECL <- select(StormData_df, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
SD_ECL$PROPDMGEXP <- as.character(SD_ECL$PROPDMGEXP)
SD_ECL$CROPDMGEXP <- as.character(SD_ECL$CROPDMGEXP)
class(SD_ECL$PROPDMGEXP)

# Convert the units of "PROPDMGEXP" and "CROPDMGEXP" into numerical form
SD_ECL$PROPDMGEXP[(SD_ECL$PROPDMGEXP == "")] <- 0
SD_ECL$PROPDMGEXP[(SD_ECL$PROPDMGEXP == "+") | (SD_ECL$PROPDMGEXP == "-") | (SD_ECL$PROPDMGEXP == "?")] <- 1
SD_ECL$PROPDMGEXP[(SD_ECL$PROPDMGEXP == "h") | (SD_ECL$PROPDMGEXP == "H")] <- 2
SD_ECL$PROPDMGEXP[(SD_ECL$PROPDMGEXP == "k") | (SD_ECL$PROPDMGEXP == "K")] <- 3
SD_ECL$PROPDMGEXP[(SD_ECL$PROPDMGEXP == "m") | (SD_ECL$PROPDMGEXP == "M")] <- 6
SD_ECL$PROPDMGEXP[(SD_ECL$PROPDMGEXP == "b") | (SD_ECL$PROPDMGEXP == "B")] <- 9

SD_ECL$CROPDMGEXP[(SD_ECL$CROPDMGEXP == "")] <- 0
SD_ECL$CROPDMGEXP[(SD_ECL$CROPDMGEXP == "+") | (SD_ECL$CROPDMGEXP == "-") | (SD_ECL$CROPDMGEXP == "?")] <- 1
SD_ECL$CROPDMGEXP[(SD_ECL$CROPDMGEXP == "h") | (SD_ECL$CROPDMGEXP == "H")] <- 2
SD_ECL$CROPDMGEXP[(SD_ECL$CROPDMGEXP == "k") | (SD_ECL$CROPDMGEXP == "K")] <- 3
SD_ECL$CROPDMGEXP[(SD_ECL$CROPDMGEXP == "m") | (SD_ECL$CROPDMGEXP ==  "M")] <- 6
SD_ECL$CROPDMGEXP[(SD_ECL$PROPDMGEXP == "b") | (SD_ECL$CROPDMGEXP == "B")] <- 9

# convert to "PROPDMGEXP" and "CROPDMGEXP" values as integer.
SD_ECL$PROPDMGEXP <- as.integer(SD_ECL$PROPDMGEXP)
SD_ECL$CROPDMGEXP <- as.integer(SD_ECL$CROPDMGEXP)

SD_ECL$PROPDMG <- SD_ECL$PROPDMG * 10^SD_ECL$PROPDMGEXP
SD_ECL$CROPDMG <- SD_ECL$CROPDMG * 10^SD_ECL$CROPDMGEXP


SD_ECL <- group_by(SD_ECL, EVTYPE)
SD_ECL_sum <- mutate(SD_ECL, SUM = PROPDMG + CROPDMG)
SD_ECL_sum <- summarize(SD_ECL_sum, PropLoss = sum(SUM))
SD_ECL_sum <- arrange(SD_ECL_sum, desc(PropLoss))
Top_TenPL <- head(SD_ECL_sum,10)

ggplot(Top_TenPL, aes(x=EVTYPE, y=PropLoss)) + geom_bar(stat="identity")

####The largest Economic Losses are suffered froom Floods. 


























    













