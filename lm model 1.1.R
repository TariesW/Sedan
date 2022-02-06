library(readxl)
Car <- read_excel("Documents/Accunique/4-Lasso／Ridge Regression／Random Decision Forest-Car Price-1850/Car.xlsx")
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


make <- Car$make
fueltype <- Car$fuelType
aspiration <- Car$aspiration
numofdoors <- Car$numOfDoors
bodystyle <- Car$bodyStyle
drivewheel <- Car$driveWheels
enginelocation <- Car$engineLocation
enginetype <- Car$engineType
numbofcylinders <- Car$numOfCylinders
fuelSystem <-  Car$fuelSystem

res_make <- model.matrix(~make)
res_fueltype <- model.matrix(~fueltype)
res_aspiration <- model.matrix(~aspiration)
numofdoors <- model.matrix(~numofdoors)
res_aspiration <- model.matrix(~aspiration)
res_numofdoors <- model.matrix(~numofdoors)
res_bodystyle <- model.matrix(~bodystyle)
res_drivewheel <- model.matrix(~drivewheel)
res_enginelocation <- model.matrix(~enginelocation)
res_enginetype <- model.matrix(~enginetype)
res_numofcylinders <- model.matrix(~numbofcylinders)
res_fuelsystem <- model.matrix(~fuelSystem)

summary(model1)
