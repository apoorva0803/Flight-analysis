
# getwd()
# setwd("C:/Users/divya239/Desktop/SSDI Proj")
mydata1 <- read.csv("Final1.csv")
summary(mydata1)

library(plyr)
count(mydata1,c("mydata1$ARR_DELAY_GROUP","mydata1$DAY_OF_WEEK"))
C <- count(mydata1,c("mydata1$ARR_DELAY_GROUP","mydata1$DAY_OF_WEEK"))
plot(C$mydata1.ARR_DELAY_GROUP~C$mydata1.DAY_OF_WEEK,pch = 15, main = "Frequency of Delays in a week")
par(mar = c(2,2,2,2))
text(C$mydata1.DAY_OF_WEEK,C$mydata1.ARR_DELAY_GROUP, C$freq,pos =4, cex = 0.8)
