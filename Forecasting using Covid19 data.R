#Forecasting COVID19 Cases with R, using facebook's prophet library
install.packages("covid19.analytics")
install.packages("ape")
library(covid19.analytics)
library(dplyr)
library(prophet)
library(lubridate)
library(ggplot2)

setwd("/Users/riteshtripathi/Desktop/")
tsc <- read.csv("ts-confirmed.csv", header = TRUE)

#we got a coln called Country, lets select US from it
tsc <- tsc %>% filter(Country.Region == 'US')
#we see the cols from are in rows, lets transponse into colns
tsc <- data.frame(t(tsc))
#dates are: 22-01-2020 and ownwards

#currently date is in row names, we need to make them in coln names
tsc <- cbind(rownames(tsc), data.frame(tsc, row.names = NULL))

#Change coln names
colnames(tsc) = c('Date', 'Confirmed')

#now lets remove first four rows
tsc <- tsc[-c(1:4), ]

#lets change dates
tsc$Date <- ymd(tsc$Date)#didnt work

tsc$Date <- ymd(tsc$Date)
str(tsc)
#it says confirmed in char, lets convert that
tsc$Confirmed <- as.numeric(tsc$Confirmed)

# Plot
qplot(Date, Confirmed, data = tsc,
      main = "Covid19 confirmed cases in UK")
ds <- tsc$Date
y <- tsc$Confirmed
df <- data.frame(ds, y)

# Forecasting
m <- prophet(df)

# Prediction
future <- make_future_dataframe(m, periods = 28)#we want to make predcitions for next 28 days
#last date now is June 18 2020
forecast <- predict(m, future)

# Plot forecast
plot(m, forecast)
dyplot.prophet(m, forecast)#making above 
prophet_plot_components(m, forecast)

#Forecast Components
prophet_plot_components(m, forecast)
#this shows two plots
#the bottom one is number of confrimed cases on an average at a lower side, Frid and sat numbers are bigger
#data is not realisty, it only tries to approximate reality
#due to lag or reporting, 

# Model performance
pred <- forecast$yhat[1:121]#this forecast has a coln called yhat
actual <- m$history$y
plot(actual, pred)

#lets add line
abline(lm(pred~actual), col = 'red')#we dont see underestimation or overestimation

#Summary
summary(lm(pred~actual))
