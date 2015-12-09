data<- read.csv("Final1.csv",header = TRUE)
counts <-table(data$MONTH,data$DAY_OF_WEEK)
data2<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
barplot(counts,main="Airline Frequency by Day of Week",xlab = "Day",names.arg = data2,
        col = terrain.colors(3),beside=TRUE,ylab="Frequency",ylim = c(3000,80000))
legend('topleft',horiz = FALSE,title="Month",
       border = "black",legend = rownames(counts),fill = terrain.colors(3), ncol = 3,cex = 0.60)

