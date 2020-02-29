# Project Title:  Hotel Price Analysis
# NAME: Vineet Viswakumar
# EMAIL: vineetmilan@gmail.com
# COLLEGE / COMPANY: SRM University, Chennai

View(Cities.df)
attach(Cities.df)

library(psych)
describe(Cities.df)

#The tree variables x1,x2 and x3 are:
#1) Star rating of hotel
#2) Is the city a metro
#3) Is it a tourist attraction
hoteltable <- table(HotelName)
hoteltable

citytable <- table(CityName, IsTouristDestination)
citytable

startable <- table(StarRating)
startable

istouristtable <- table(IsTouristDestination)
istouristtable

isweekendtable <- table(IsWeekend)
isweekendtable

library(car)
scatterplot(StarRating, RoomRent)

library(lattice)
bwplot(RoomRent)
xyplot(RoomRent~ Population)

#T tests for the dependent variable(Room rent) and the indpendent variables
t.test(RoomRent, StarRating)
t.test(RoomRent~IsTouristDestination)
t.test(RoomRent~IsMetroCity)
t.test(RoomRent~IsWeekend)
t.test(RoomRent~FreeBreakfast)
t.test(RoomRent, Airport)
t.test(RoomRent~FreeWifi)
t.test(RoomRent, CityRank)

#corrogram
library(corrgram)
hotel <- c("RoomRent","StarRating","IsTouristDestination","IsMetroCity")
corrgram(Cities.df[,hotel], order=TRUE,lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

#Covariance matrix
library(Hmisc)
hotel <- c("RoomRent","StarRating","IsTouristDestination","IsMetroCity")
hotelMatrix2 <- rcorr(as.matrix(Cities.df[,hotel]))
hotelMatrix2

test_model<-log(RoomRent)~IsMetroCity+Date+StarRating+IsTouristDestination+IsWeekend+IsNewYearEve+HasSwimmingPool+FreeBreakfast+FreeWifi+HotelName
fit_1<-lm(test_model, data=Cities.df)
summary(fit_1)

