#--------------------------------------------------------------------
#Final Code for graph about freqency comparison between different airlines


mydata<- read.csv("Final1.csv",header = TRUE)
counts <- table(mydata$UNIQUE_CARRIER,mydata$MONTH)

data2<-c("January","February","March")
options("scipen"=20)
barplot(counts,main="Airline Frequency by month",xlab = "Months",names.arg = data2,
        col=rainbow(14),beside=TRUE,ylab="Frequency",ylim = c(4000,100000))
legend('topleft',horiz = FALSE,title("Airlines id"),
       border = "black",legend = rownames(counts),fill = rainbow(14), ncol = 3,cex = 0.50)
#---------------------------------------------------------------------
#-------------------------------------------
#to display most frequent origin city names

library(plotrix)
mytable <- (rev(sort(table(mydata$ORIGIN_CITY_NAME)))[1:5])
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie3D(mytable, labels = lbls, explode = 0.1, radius = 0.9,labelcex = 0.9,
      main="3D-Pie Chart of Most Visited Cities",col=1:5)
#------------------------------------------------------