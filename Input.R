library(readxl)
Car <- read_excel("Documents/准雀/4-Lasso／Ridge Regression／Random Decision Forest-1850/Car.xlsx")
View(Car)

price <- Car$price
head(price)
highwaympg <- Car$highwayMpg
head(highwaympg)
citympg <- Car$cityMpg
head(citympg)
peakrpm <- Car$peakRpm
head(peakrpm)
horsepower <- Car$horsepower
head(horsepower)
compressionratio <- Car$compressionRatio
head(compressionratio)
stroke <- Car$stroke
head(stroke)
bore <- Car$bore
head(bore)
enginesize <- Car$engineSize
head(enginesize)
curbweight <- Car$curbWeight
head(curbweight)
height <- Car$height
head(height)
width <- Car$width
head(width)
length <- Car$length
head(length)
wheelbase <- Car$wheelBase
head(wheelbase)



#We first study the regression model between the price and the 13 explanatory variables by a linear regression:
model_1 <- lm(price ~ highwaympg + citympg + peakrpm + horsepower + compressionratio + stroke + bore + enginesize + curbweight + height + width + length + wheelbase)
summary(model_1)

#build a model with only statistically significant variables
model2 <- lm(price ~ stroke + enginesize, data = df)
summary(model2)

#build a model with only engine size explanatory variable
model3 <- lm(price ~ enginesize, data = df)
summary(model3)
#plot this model3
ggplot(model3, aes(enginesize, price)) +
  geom_point()+
  stat_smooth(method = lm, formula = y ~ x )


  
#the bootstrap method is used to quantify the uncertainty associated with given statistical estimator or with a predictive model
#the bootstrap procedure consists of randomly selecting a sample of n observations from the original data set. The subset, called bootstrap data set is then sued to evaluate the model.
#the bootstrap process, repeated large number of times and the standard error of the bootstrap estimate is then calculated. The results provide an indiation of the variance of the model's performance

library(boot)
library(tidyverse)
library(caret)

#bootstrap procedure for model_1 multiple linear regression, we use a bootstrap with 500 samples to test this model
boot.control <- trainControl(method = "boot", number = 500)
model_1_boot <- train(price ~., data = df, method = "lm", trControl = boot.control)
print(model_1_boot)


#bootstrap procedure for model_2 multiple linear regression with 2 variables, we use a bootstrap with 500 samples to test this model
boot.control <- trainControl(method = "boot", number = 500)
model_2_boot <- train(price ~ enginesize + stroke , data = df, method = "lm", trControl = boot.control)
print(model_2_boot)


#bootstrap procedure for model_3 simple linear regression with only 1 variables, used 500 samples to test this model.
boot.control <- trainControl(method = "boot", number = 500)
model_3_boot <- train(price ~ enginesize, data = df, method = "lm", trControl = boot.control)
print(model_3_boot)


# we are going to build polynomial regression between responsive variable "price" and explanatory variable"engine size"
model4 <- lm(price ~ poly(enginesize, 2, raw = TRUE), data = df)
summary(model4)


# plot model 4
ggplot(model4, aes(enginesize, price)) +
  geom_point()+
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))


# build model 5 based on log(enginesize)
model5 <- lm(price ~ log(enginesize), data = df)
summary(model5)


#plot model 5
ggplot(model5, aes(enginesize, price))+
  geom_point()+
  stat_smooth(method = lm, formula = y ~ log(x))
  
  
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

#The standard linear model (or the ordinary least squares method) performs poorly in a situation, where you have a large multivariate data set containing a number of variables superior to the number of samples.


#A better alternative is the penalized regression allowing to create a linear regression model that is penalized, for having too many variables in the model, by adding a constraint in the equation (James et al. 2014,P. Bruce and Bruce (2017)). This is also known as shrinkage or regularization methods.
#The most commonly used penalized regression methods, including ridge regression, lasso regression and elastic net regression. 


#Ridge regression shrinks the regression coefficients, so that variables, with minor contribution to the outcome, have their coefficients close to zero.
#Lasso stands for Least Absolute Shrinkage and Selection Operator. It shrinks the regression coefficients toward zero by penalizing the regression model with a penalty term called L1-norm, which is the sum of the absolute coefficients.


#Lasso regression works better if some of the explanatory variables affect responsive variables significantly while other variables have very small coefficient.
#Ridge regression works better if responsive variables are affected by explanatory variables of roughly equal coefficients.


library(tidyverse)
library(dplyr)
library(caret)
library(glmnet)

df <- data.frame(price, highwaympg, citympg, peakrpm , horsepower , compressionratio , stroke , bore , enginesize , curbweight , height , width , length , wheelbase)

y <- price
head(y)
x <- model.matrix(price ~ highwaympg + citympg + peakrpm + horsepower + compressionratio + stroke + bore + enginesize + curbweight + height + width + length + wheelbase)

#computing ridge regression
#find the best lambda using cross-validation, lambda = a number value defining the amount of shrinkage
set.seed(123)
cv <- cv.glmnet(x, y, alpha = 0)
#display the best lambda value
cv$lambda.min
#fit the final model 
model_ridge <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
#display regression coefficients
coef(model_ridge)


#computing lasso regression
#find the best lambda using cross-validation
set.seed(123)
cv <- cv.glmnet(x, y, alpha = 1)
#display the best lambda value
cv$lambda.min
#fit the final model
model_lasso <- glmnet(x, y, alpha = 1,lambda = cv$lambda.min)
#display regression coefficients
coef(model_lasso)


varmtx <- model.matrix(price~. -1, data = df)
response <- df$price
ridge2 <- glmnet(scale(varmtx), response, alpha=0)
cv.ridge2 <- cv.glmnet(varmtx, response, alpha = 0)
lasso2 <- glmnet(scale(varmtx), response, alpha=1)
cv.lasso2 <- cv.glmnet(varmtx, response, alpha = 1)
par(mfrow = c(1,2))
par(mar=c(4,2,6,2))



plot(ridge2, xvar = "lambda", label = T)
abline(v=cv.ridge2$lambda.lse, col = "blue", lty =2)
abline(v=cv.ridge2$lambda.min, col = "red", lty =2)
title("Ridge(with co-linearity)", line = 2.5)



plot(lasso2, xvar = "lambda", label = T)
abline(v=cv.ridge2$lambda.min, col = "red", lty =2)
abline(v=cv.ridge2$lambda.lse, col = "blue", lty =2)
title("Lasso(with co-linearity)", line = 2.5)




