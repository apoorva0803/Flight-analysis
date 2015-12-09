library(e1071)


rmse <- function(error)
{
  sqrt(mean(error^2))
}


library(plyr)

A <- count(mydata1,c("mydata1$ARR_DELAY_GROUP","mydata1$DAY_OF_WEEK"))
C <- subset(A , A$mydata1.ARR_DELAY_GROUP == 0)

plot(C$mydata1.DAY_OF_WEEK, C$freq, main =" Flight Traffic in a Week")


# svr model ==============================================
if(require(e1071)){
  
  
  model <- svm( C$freq ~ C$mydata1.DAY_OF_WEEK, C)
  
  predictedY <- predict(model)
  
  points(C$mydata1.DAY_OF_WEEK, predictedY, col = "red", pch=17)
  
  
  error <- C$freq - predictedY  # /!\ this time  svrModel$residuals  is not the same as data$Y - predictedY
  svrPredictionRMSE <- rmse(error)  # 3.157061 
  Y <- C$freq
  X<- C$mydata1.DAY_OF_WEEK
  data <- C
  
  
  summary(model)
  print(predictedY)
}

tuneResult <- tune(svm, Y ~ X,  data = data , tunecontrol = tune.control(cross = 5))



print(tuneResult) # best performance: MSE = 8.371412, RMSE = 2.89  epsilon  1e-04   cost 4

# Draw the first tuning graph 
plot(tuneResult) 

# On the first tuning graph, we can see that the graph is darker on the leftside when epsilon is small,
# so we adjust the tuning to go in this direction 

# Draw the second tuning graph
tuneResult <- tune(svm, Y ~ X,  data = data, 
                   ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9)), tunecontrol = tune.control(cross = 5)  )
print(tuneResult) 

plot(C$mydata1.DAY_OF_WEEK, C$freq, pch = 16, main = "SVM Vs Tuned SVM")
tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, data) 

points(C$mydata1.DAY_OF_WEEK, predictedY, col = "red", pch=4)
lines(C$mydata1.DAY_OF_WEEK, predictedY, col = "red", pch=4)

points(C$mydata1.DAY_OF_WEEK, tunedModelY, col = "blue", pch=4)
lines(C$mydata1.DAY_OF_WEEK, tunedModelY, col = "blue", pch=4)

error <- C$freq - tunedModelY  

# this value can  be different because the best model is determined by cross-validation over randomly shuffled data 
tunedModelRMSE <- rmse(error)  

print(paste0("Root Squared Mean Error for Tuned Model = ",tunedModelRMSE))
print(paste0("Root Squared Mean Error for SVM Prediction Model = ", svrPredictionRMSE))
print(paste0("Root Squared Mean Error for Linear Regression Model = ", predictionRMSE))
} 


# end of svr model =======================================




