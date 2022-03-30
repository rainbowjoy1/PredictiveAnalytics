#try write by Annaya
###Part 1
#1. Plot the data and the autocorrelation function. Decompose your time series into trend, cycle,
#and seasonal component with a methodology of your choice (classical decomposition, SEATS, etc). 
#Discuss the properties of the series and the limitation of the methodology you choose (5 lines).

library(ggplot2)
library(ggfortify)
library(forecast)
library(dplyr)
library(fpp3)
library(tsibble)
library(gridExtra)

#read the csv file from github repository
emp <- read.csv("emp.csv", header=TRUE)

#convert the date column to date format
emp[["DATE"]] <- as.Date(emp[["DATE"]])

#examine the data
dim(emp)
summary(emp)

#convert the data to a time series to allow the SCF to run an dplace the data in monthly format
emp.ts <- ts(emp[,2], start = c(1948,1), end = c(2007,12), frequency = 12)


#generate plots of the data
auto <- autoplot(emp.ts)+ ylab("")+ggtitle("Plot of Time Series Data")
acfplot <- ggAcf(emp.ts, )+ ggtitle("ACF of Data")

#arrange the plots into one image
grid.arrange(auto, acfplot)

autoplot(decompose(emp.ts, type = c("multiplicative")))+
  labs(title = "Multiplicative Decomposition on the Dataset")



#2. Transform your data by taking natural logarithm or with Box and Cox methodology. Repeat
#the analysis of point 1.1: how does it change? Decide whether to use original or transformed
#data in the rest of the assignment. Motivate your choice.


#3. Based on the results in point 1.1 and the choice in point 1.2, discuss what is the most accurate
#functional form for the KPSS and ADF tests. Then, perform both tests to check stationarity
#and discuss the results.


#4. If the series is not stationary, follow the approach presented in class to make it stationary.
#When the series is stationary, look at the autocorrelation and partial autocorrelation function,
#choose an ARIMA model for the series and write down the equation of the model in the form
#ARIMA(p,d,q)(P,D,Q)[m]. (Hint: if there is a deterministic trend, you need to detrend the time series)


###Part 2
#In this part use the data based on the decision in point 1.2 (i.e. either the original data or the log/box-cox transformed data).

#1. Split your sample into a train set and a test set. Estimate one ARIMA model by using AIC: is the estimated model 
#coherent with the information acquired in point 1.3? Discuss the properties of the residuals. Can the model be improved? 
# If it is the case, modify it discuss the new model performance.


#2. Perform a forecast with the model selected in point 2.1 and in point 1.4 
#(note: if you had detrended or differentiated you need to change something in the formula). Plot the forecasts
#of each model with the original data in two separate graphs and discuss the measures of
#accuracy: what is the best model?