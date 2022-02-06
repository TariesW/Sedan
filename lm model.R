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




