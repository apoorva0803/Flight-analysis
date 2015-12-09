
# install.packages("ggplot2")
# install.packages("reshape2")
library(ggplot2)
head(diamonds)
# getwd()
# setwd("C:\\Users\\Rimple\\Desktop\\Sem-1\\SSDI\\Project")
flightData<-read.csv("final1.csv",TRUE,",")
head(flightData)

flightData$new_delay <- ifelse(flightData$CARRIER_DELAY>0 & flightData$CARRIER_DELAY>flightData$LATE_AIRCRAFT_DELAY & flightData$CARRIER_DELAY>flightData$NAS_DELAY & flightData$CARRIER_DELAY>flightData$SECURITY_DELAY & flightData$CARRIER_DELAY>flightData$WEATHER_DELAY,"Career Delay",
                        ifelse(flightData$WEATHER_DELAY >0 & flightData$WEATHER_DELAY>flightData$CARRIER_DELAY & flightData$WEATHER_DELAY>flightData$CARRIER_DELAY & flightData$CARRIER_DELAY>flightData$LATE_AIRCRAFT_DELAY & flightData$CARRIER_DELAY>flightData$NAS_DELAY & flightData$CARRIER_DELAY>flightData$SECURITY_DELAY,"Weather Delay",
                        ifelse(flightData$NAS_DELAY>0 & flightData$NAS_DELAY>flightData$CARRIER_DELAY & flightData$NAS_DELAY>flightData$LATE_AIRCRAFT_DELAY & flightData$NAS_DELAY>flightData$SECURITY_DELAY & flightData$NAS_DELAY>flightData$WEATHER_DELAY,"NAS Delay",
                        ifelse(flightData$SECURITY_DELAY>0 & flightData$SECURITY_DELAY>flightData$CARRIER_DELAY & flightData$SECURITY_DELAY>flightData$LATE_AIRCRAFT_DELAY & flightData$SECURITY_DELAY>flightData$NAS_DELAY & flightData$SECURITY_DELAY>flightData$WEATHER_DELAY,"Security Delay",
                        ifelse(flightData$LATE_AIRCRAFT_DELAY>0 & flightData$LATE_AIRCRAFT_DELAY>flightData$CARRIER_DELAY & flightData$LATE_AIRCRAFT_DELAY>flightData$NAS_DELAY & flightData$LATE_AIRCRAFT_DELAY>flightData$SECURITY_DELAY & flightData$LATE_AIRCRAFT_DELAY>flightData$WEATHER_DELAY,"Late Aircraft Delay",
                        "No Delay")))))

flightData$new_delay[is.na(flightData$new_delay)] <- "No Delay"


flightDataDelay<- flightData[flightData$new_delay != "No Delay",]

head(flightDataDelay)



p<- qplot(factor(flightDataDelay$new_delay), main = "Types of Delay in Flights", data=flightDataDelay, geom="bar", fill=factor(flightDataDelay$new_delay)
,xlab="Delay"
,ylab="Number of Flights")
p<-p+labs(fill="Types of Delay")
p
