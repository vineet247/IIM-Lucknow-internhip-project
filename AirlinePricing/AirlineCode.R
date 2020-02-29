# Analysis of Airline Ticket Pricing
# NAME: Vineet Viswakumar
# EMAIL: vineetmilan@gmail.com
# COLLEGE / COMPANY: SRM University, Chennai


#Attach the data frame and display summary
attach(SixAirlinesDataV2)
summary(SixAirlinesDataV2)

#mean, median and standard deviation of cost of premium economy tickets 
aggregate(PricePremium, by = list(int = IsInternational), mean)
aggregate(PricePremium, by = list(int = IsInternational), median)
summary(PricePremium)


#mean cost of premium economy tickets by month
aggregate(PricePremium, by = list(month = TravelMonth,air=Airline, int= IsInternational), mean)

#mean travel time
mean(FlightDuration)

#Plot flight distance and cost of premium economy tickets
plot(FlightDuration, PricePremium, col='blue')

#Plot to see cost of premium tickets based on  number of premiun economy seats
plot(SeatsPremium, PricePremium)

#Plot to see cost of premium economy tickets based on pitch in economy
plot(PitchPremium, PricePremium)

#Plot pitch width in premium  and cost of premium economy tickets
plot(WidthPremium, PricePremium)


#boxplots
boxplot(SeatsPremium, xlab= 'Seats Premium')
boxplot(SeatsEconomy, xlab= ' Seats Economy')
boxplot(PricePremium, xlab= 'Price Premium')
boxplot(PriceEconomy, xlab= ' Price Economy')

#Regession
model=lm(PricePremium~FlightDuration+PriceEconomy+PriceRelative+PitchDifference+PercentPremiumSeats+Airline)
summary(model)
abline(model)

#Corrogram
corrgram(SixAirlinesDataV2, order=TRUE, upper.panel=panel.pie, text.panel=panel.txt)


