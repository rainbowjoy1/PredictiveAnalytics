#Assignment 1
install.packages("ggfortify") 

#Currently not all packages are being used. Uploaded for ease later
library(ggplot2)
library(ggfortify)
library(fpp3)
library(tidyr)
library(fable)
library(AER)

###Question 1
#For this question, we will study real disposable income (disposable income from now on) and
#real consumption expenditure (consumption expenditure from now on) in US from 1950 to 2000.
#The dataset USMacroG contains 12 time series: Real gross domestic product (in billion USD), Real
#consumption expenditures, Real investment by private sector, Real government expenditures, Real
#disposable personal income, Consumer price index, Nominal money stock, Quarterly average of
#month end 90 day treasury bill rate, Unemployment rate, Population (in million), Inflation rate, Ex
#post real interest rate (computed as treasury bill rate - inflation). Data have quarterly frequency.

data("USMacroG", package = "AER")
summary(USMacroG)
USMacroG

####1. Create two plots, one for the disposable income series and one for its autocorrelation. What
####are the relevant features of the data? Can you confirm them from the autocorrelation function?
####Would it make sense to transform the data? Why?

autoplot(USMacroG[, "dpi"]) + labs(y= "DPI $USD", title= "Disposible US Income over Time")

#The plot currently looks like it could be exponential. It is hard to tell if 
#there is seasonality but if there is it is not strong. The graph is not smooth and has a 
#clear upward trend.

ggAcf(USMacroG[,"dpi"]) + labs(y= "Autocorrelation", title= "Autocorrelation of DPI in the US")


####2. Transform the series to create a series for the growth rate of disposable income in US quarter
####on quarter. Plot the growth series and its autocorrelation function and comment it. Are the
####data still trending? Do you see any sign of seasonality or cycle? Perform a statistical test to
####verify whether data are autocorrelated or not.


####3. Split the sample into a training and a test set. Fit the level of disposable income with 2 models
####from the ETS class (hint: use one model ???guessed??? and one model automatically selected).
####Discuss the models and their residuals (include a test on residuals autocorrelation).


####4. Now forecast the test set, plot the two forecasts with the original data into two separate
####graphs (one for each model), and evaluate the accuracy of the two models.


####5. Pick the real consumption expenditures and transform the series to create a series for the
####growth rate quarter on quarter. Plot the growth rate of real consumption expenditures against 
####the growth rate of disposable income. Do you think there is any relation?

