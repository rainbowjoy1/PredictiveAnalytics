#Assignment 1

#Currently not all packages are being used. Uploaded for ease later
library(ggplot2)
library(ggfortify)
library(forecast)
library(fpp3)
library(tidyr)
library(fable)
library(AER)
library(MASS)
library(caret)


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

#we ran the Ljung-Box test to determine if the autocerrelations are significantly different
#than white noise. We found that the p-value is very small so the residuals are
#distinguisable from white noise and autocorrelation exsists.

####3. Split the sample into a training and a test set. Fit the level of disposable income with 2 models
####from the ETS class (hint: use one model ???guessed??? and one model automatically selected).
####Discuss the models and their residuals (include a test on residuals autocorrelation).

dpi_train <- window(growth_dpi, end = 1990)
dpi_test <- window(growth_dpi, start = 1991)


#following code needs adjustments but it does work

fit <- China_GDP %>%
  model(ETS(GDP))
report(fit)

dpi_train %>%
  stretch_tsibble(.init = 10) %>%
  model(
    SES = ETS(dpi ~ error("A") + trend("N") + season("N")),
    Holt = ETS(dpi ~ error("A") + trend("A") + season("N")),
    Damped = ETS(dpi ~ error("A") + trend("Ad") +
                   season("N"))
  ) %>%
  forecast(h = 20) %>%
  accuracy(dpi_train)


#Broken ass code

#gathered <- gather(BC_dpi)

#long_DF <- BC_dpi %>% gather(key=Quarter, value=value -dpi)

#df_pivoted <- pivot_longer(!dpi, names_to = "Quarter")


####4. Now forecast the test set, plot the two forecasts with the original data into two separate
####graphs (one for each model), and evaluate the accuracy of the two models.


####5. Pick the real consumption expenditures and transform the series to create a series for the
####growth rate quarter on quarter. Plot the growth rate of real consumption expenditures against 
####the growth rate of disposable income. Do you think there is any relation?

