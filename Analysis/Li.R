
mydata_subset3 <- subset(mydata1, mydata1$ARR_DELAY_GROUP == -1 | mydata1$ARR_DELAY_GROUP== 0)
#save the dataset if memory if full
#save(mydata_subset3, file = "C:\\Users\\divya239\\Desktop\\temp.RData")

#load("C:\\Users\\divya239\\Desktop\\temp.RData")

rm(obj)

mydata_subset3.lm=lm(mydata_subset3$ARR_DELAY_GROUP ~ mydata_subset3$ORIGIN_CITY_NAME)
summary(mydata_subset3.lm)
#library(car)
outlierTest(mydata_subset3.lm)


rm(mydata_subset3.lm)

mydata_subset3.lm=lm(mydata_subset3$ARR_DELAY_GROUP ~ mydata_subset3$DEST_CITY_NAME)
summary(mydata_subset3.lm)

rm(mydata_subset3.lm)

mydata_subset3.lm=lm(mydata_subset3$ARR_DELAY_GROUP ~ mydata_subset3$UNIQUE_CARRIER)
summary(mydata_subset3.lm)

rm(mydata_subset3.lm)

mydata_subset3.lm=lm(mydata_subset3$ARR_DELAY_GROUP ~ mydata_subset3$DAY_OF_WEEK)
model <- summary(mydata_subset3.lm)

rm(mydata_subset3.lm)


