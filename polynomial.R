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
