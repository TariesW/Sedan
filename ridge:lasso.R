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














