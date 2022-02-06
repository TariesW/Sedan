library(rsample)
library(randomForest)
library(ranger)
library(caret)
library(h2o)

#split data into training(80%) and test set(20%)
training.samples <- df$price %>% createDataPartition(p=0.8, list = FALSE)
train.data <- df[training.samples,]
test.data <- df[-training.samples,]



#default Random Forest model
m1 <- randomForest(formula = price ~., data = df)
m1
plot(m1)
#plotting the model m1 illustrate the error rate drops significantly around 30 tress and stabalizes around 100 trees 

#we can find which number of tress providing the lowest error rate and the result is 97 trees providing an average car sales price error of $2013.53
which.min(m1$mse)
sqrt(m1$mse[which.min(m1$mse)])


#use another method to fit the model
model_randomforest <- train(price~., data = df, method = "rf", trControl = trainControl("cv", number = 13), importance = TRUE)
model_randomforest$bestTune


#ranking variable importance
importance_m1 <- m1$importance
importance_m1

#obtain variable importance 
varImp(m1)

#rank the variable importance by MeanDecreaseGini 
varImpPlot(m1, type = 2)









