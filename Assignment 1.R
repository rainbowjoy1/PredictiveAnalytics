#Assignment 1

#library(ggfortify)
#library(forecast)
#library(fpp3)
#library(tidyr)
#library(fable)
#library(MASS)
#library(caret)


library(tsibble)
library(ggplot2)
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
dpi <- USMacroG[,"dpi"]

dpi_l <- as_tsibble(USMacroG[,"dpi"])

####1. Create two plots, one for the disposable income series and one for its autocorrelation. What
####are the relevant features of the data? Can you confirm them from the autocorrelation function?
####Would it make sense to transform the data? Why?

autoplot(dpi) + labs(y= "DPI $USD", title= "Disposible US Income over Time")

#The plot currently looks like it could be exponential. It is hard to tell if 
#there is seasonality but if there is it is not strong. The graph is not smooth and has a 
#clear upward trend.The data is not random and we think has a strong correlation

ggAcf(dpi, lag.max = 300) + labs(y= "Autocorrelation", title= "Autocorrelation of DPI in the US")

#The correlation proves that there is strong correlation between dpi over time. It shows that the 
#relationship is correlated, positive, and trending upward over time. It does not highlight 
#seasonality and has a very smooth shape.

#The data seems to be exponential based on the plot and and the autocorrelation so we
#should remove it via finding the logs or via box-cox. This should be done so that we can run 
#further exploration on linear data and get appropriate results.


####2. Transform the series to create a series for the growth rate of disposable income in US quarter
####on quarter. Plot the growth series and its autocorrelation function and comment it. Is the
####data still trending? Do you see any sign of seasonality or cycles? Perform a statistical test to
####verify whether data is autocorrelated or not.

dpi_lambda <- BoxCox.lambda(USMacroG[,"dpi"])

#because our lambda value is greater than 0.1 (.295) we will use the Box-Cox method to 
#transform the data to a linear dataset


BoxCox.lambda(dpi)
BC_dpi = (BoxCox (dpi, dpi_lambda))
autoplot(BoxCox(dpi, lambda=dpi_lambda))

growth_dpi <- diff(BC_dpi)
autoplot(growth_dpi) + labs(title="Box Cox Diff Data over time")
ggAcf(growth_dpi, lag.max = 300)

#The transformed data is no longer trending. Generally the data seems to be cyclical in
#having upward trending periods followed by downward trending periods, but it shows no seasonality

Box.test(growth_dpi, type = c("Ljung-Box"), lag = 10)

gdpil<- as_tsibble(growth_dpi)

#we ran the Ljung-Box test to determine if the autocerrelations are significantly different
#than white noise. We found that the p-value is very small so the residuals are
#distinguisable from white noise and autocorrelation exsists.

####3. Split the sample into a training and a test set. Fit the level of disposable income with 2 models
####from the ETS class (hint: use one model ???guessed??? and one model automatically selected).
####Discuss the models and their residuals (include a test on residuals autocorrelation).


#This is the "right" way to train/test code BUT it is leading to major issues with the 
#models because they are not in proper time order....

#Below the line of hash I will rewrite everything to be done with "window"

smp_size <- floor(0.8 * nrow(gdpil))

set.seed(123)
train_ind <- sample(seq_len(nrow(gdpil)), size = smp_size)

train <- gdpil[train_ind, ]
test <- gdpil[-train_ind, ]


fit <- gdpil %>%
  model(ETS(value))
report(fit)

#Report says the A,N,N model is ideal

train %>%
  stretch_tsibble(.init = 10) %>%
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
  ) %>%
  forecast(h = 20) %>%
  accuracy(train)


###########################################################################################

growth.USIncTrain <- window(gdpil, end = "1990 Q1")
growth.USIncTest <- window(gdpil, start = 1950 Q1)


####4. Now forecast the test set, plot the two forecasts with the original data into two separate
####graphs (one for each model), and evaluate the accuracy of the two models.


####5. Pick the real consumption expenditures and transform the series to create a series for the
####growth rate quarter on quarter. Plot the growth rate of real consumption expenditures against 
####the growth rate of disposable income. Do you think there is any relation?


cpi <- USMacroG[,"cpi"]

cpi_1 <- as_tsibble(USMacroG[,"cpi"])

autoplot(cpi) + labs(y= "CPI $USD", title= "Consumption Expenditures over Time")
ggAcf(cpi, lag.max = 300) + labs(y= "Autocorrelation", title= "Autocorrelation of CPI in the US")
cpi_lambda <- BoxCox.lambda(USMacroG[,"cpi"])

BoxCox.lambda(cpi)
BC_cpi = (BoxCox (cpi, cpi_lambda))
autoplot(BoxCox(cpi, lambda=cpi_lambda))

growth_cpi <- diff(BC_cpi)
autoplot(growth_cpi) + labs(title="CPI Box Cox Diff Data over time")
ggAcf(growth_cpi, lag.max = 300)


gcpil<- as_tsibble(growth_cpi)

cpi_dpi<- cbind(growth_cpi, growth_dpi)
autoplot(cpi_dpi)

#trying to make a single line that is the difference of the diffs to see if there is any relation

cpi_dpi$V3 <- cpi_dpi - cpi_dpi$V2

