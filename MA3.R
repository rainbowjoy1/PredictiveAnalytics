library(AER)
library(ggplot2)
library(tsibble)
library(fpp3)

data("USMacroG")

GDP_i<-USMacroG[,c("gdp", "cpi")]

GDP_i

#####Part 1

#1. Plot GDP logs and Inflation. Investigate graphically the presence of a trend
#and seasonality without using Classical Decomposition, STL or other statistical methodologies.
log.GDP_i <- box_cox(GDP_i, 0)
ggplot(log.GCP_i)

#2. Stationarize GDP logs and Inflation.


#3. Once that data are stationary, select a dynamic model with ARIMA where (stationary) GDP
#logs explains (stationary) Inflation. Verify the presence of structural breaks in this regression
#model with a QLR test and the SIS by setting the p-value to 5%.


#####Part 2

#1. Shorten the time series if structural breaks are present. Then, split the sample in a test test
#and a training set.


#2. Estimate a VAR and the dynamic regression (as before, GDP enters only contemporaneously 
#as explanatory variable in the regression model) on the training set. Evaluate the fitting of the
#models. Note that dynamic regressions require stationarity, VARs do not.


#3. Forecast the test set with the two models (hint: use an ARIMA to forecast GDP 
#in the dynamic regression). Compare the accuracy of the two forecast for CPI.


#4. Verify that GDP growth granger-causes inflation growth.