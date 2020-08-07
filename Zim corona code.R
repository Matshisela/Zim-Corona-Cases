#Loading the data
who_data <- read.csv(file.choose())

#Viewing the data
View(who_data)

#Loading packages
library(tidyverse)
library(fpp)
library(git2r)
library(tidymodels)
library(timetk)
library(modeltime)

#Date Manipulation
who_data$Date_reported <- as.Date(who_data$Date_reported)

#Subsetting Zimbabwean data
zim_data <- who_data %>%
  filter(Country=='Zimbabwe')

#Selec n creating a Timeseries dataset
zim_data1 <- zim_data %>%
  select(Date_reported, New_cases, New_deaths) 

#Plotting graph
zim_data1 %>%
  plot_time_series(Date_reported, New_cases, .interactive = interactive)

start_date <- as.Date('2020-03-20')
finish_date <- as.Date('2020-07-07')
data_points <- finish_date - start_date #109 days

#Time series
time_ser <- ts(zim_data1$New_cases, start = start_date, end = finish_date)

#Plotting
ggplot(data = zim_data1, aes(x = Date_reported)) + 
  geom_line(aes(y = cumsum(New_cases)), color = 'blue') +
  ggtitle('Total Corona Cases in Zimbabwe: 20 March - 07 July 2020') +
  ylab('Total Number')+
  xlab('Date') +
  ggsave('Total Corona.png')

ggplot(data = zim_data1, aes(x = Date_reported, y = New_cases)) + 
  geom_line(color = 'red') +
  ggtitle('Daily Corona Cases in Zimbabwe: 20 March - 07 July 2020') +
  ylab('Total Number')+
  xlab('Date') +
  ggsave('Daily Corona.png')

ggplot(data = zim_data1, aes(x = New_cases)) +
  geom_boxplot()


#ACF and PACF
acf(zim_data1$New_cases)
pacf(zim_data1$New_cases)

#Hyndman Khandakar Algorithm
fit1 <- auto.arima(zim_data1$New_cases)
res <- residuals(fit1)
Box.test(res, lag = 10)
plot(res)
accuracy(fit1)

#Forecast
fcast <- forecast(fit1, h=20)
fcast
plot(fcast)
plot(forecast(fit1, fan=TRUE))
