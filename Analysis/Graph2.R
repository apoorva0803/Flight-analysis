library(plyr)
A <- count(mydata1,c("mydata1$UNIQUE_CARRIER", "mydata1$ORIGIN_CITY_NAME", "mydata1$DAY_OF_WEEK"))
print(A)
C <- subset(A , A$mydata1.UNIQUE_CARRIER == "US" & A$freq >2000)
print(C)
plot(C$mydata1.DAY_OF_WEEK, C$freq, main= "High Frequency Airports for US Airways", xlab = "Frequency", ylab= " Day of the Week")
text(C$mydata1.DAY_OF_WEEK,C$freq, C$mydata1.ORIGIN_CITY_NAME,pos =3, cex = 0.8)

