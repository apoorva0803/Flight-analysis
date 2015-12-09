library(e1071)


rmse <- function(error)
{
  sqrt(mean(error^2))
}


library(plyr)

A <- count(mydata1,c("mydata1$ARR_DELAY_GROUP","mydata1$DAY_OF_WEEK"))
C <- subset(A , A$mydata1.ARR_DELAY_GROUP == 0)
print(C)
plot(C$mydata1.DAY_OF_WEEK, C$freq, main =" Flight Traffic in a Week")


# linear model ==============================================



model=lm( C$freq ~ C$mydata1.DAY_OF_WEEK, C)

summary(model)
predictedY <- predict(model) 
print(predictedY)
points(C$mydata1.DAY_OF_WEEK, predictedY , col = "blue", pch=4)   


error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   
print(error)
# end of linear model =======================================
