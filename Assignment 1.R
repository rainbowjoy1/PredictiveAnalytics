#Assignment 1

#library(ggfortify)

#library(fpp3)
#library(tidyr) 
library(MASS) #for mutate()
#library(caret)
library(forecast) #for BoxCox()
library(fable)
library(tsibble)
library(ggplot2)
library(AER)
library(feasts) #for gg_tsresiduals()



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
dpi_l <- as_tsibble(dpi)

####1. Create two plots, one for the disposable income series and one for its autocorrelation. What
####are the relevant features of the data? Can you confirm them from the autocorrelation function?
####Would it make sense to transform the data? Why?

autoplot(dpi) + labs(y= "DPI $USD", title= "Disposible US Income over Time")

#The plot currently looks like it could be exponential. It is hard to tell if 
#there is seasonality but if there is it is not strong. The graph is not smooth and has a 
#clear upward trend.The data is not random and we think has a strong correlation

ggAcf(dpi, lag.max = 24) + labs(y= "Autocorrelation", title= "Autocorrelation of DPI in the US")

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

dpi.ts <- as_tsibble(BC_dpi)
dpi.ts$observation <- 1:nrow(dpi.ts) 
dpi.expand = dpi.ts %>%
  mutate(Diff_year =  observation - lag(observation),# Difference in time (just in case there are gaps)
         Diff_growth = value - lag(value), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/value * 100)

real_growth_rate_dpi = subset(dpi.expand, select = -c(observation, Diff_year, Diff_growth, value))

autoplot(real_growth_rate_dpi) + labs(title="Box Cox Diff Data over time")
ggAcf(real_growth_rate_dpi, lag.max = 24)

#The transformed data is no longer trending. Generally the data seems to be cyclical in
#having upward trending periods followed by downward trending periods, but it shows no seasonality

Box.test(growth_dpi, type = c("Ljung-Box"), lag = 10)

gdpil<- as_tsibble(growth_dpi)

#we ran the Ljung-Box test to determine if the autcoerrelations are significantly different
#than white noise. We found that the p-value is very small so the residuals are
#distinguishable from white noise and autocorrelation exists.

####3. Split the sample into a training and a test set. Fit the level of disposable income with 2 models
####from the ETS class (hint: use one model ???guessed??? and one model automatically selected).
####Discuss the models and their residuals (include a test on residuals autocorrelation).

Train <- dpi_l %>% slice(1:163)
Test <- dpi_l %>% slice(163:203)

MAN <- Train %>% model(MAN = ETS(value))

report(MAN)

#'Report says the M,A,N model is ideal because the ETS model is trained on the original data
#'Which is exponential so the ETS suggests a multiplicative model. 


Holt <- Train %>%
  model(Holt = ETS(value ~ error("A") + trend("A") + season("N"))) 


####4. Now forecast the test set, plot the two forecasts with the original data into two separate
####graphs (one for each model), and evaluate the accuracy of the two models.

MAN %>% forecast(h=40) %>%
  autoplot(Test,level=NULL)+ ggtitle("MAN Forecast")

Holt %>% forecast(h=40) %>%
  autoplot(Test,level=NULL) + ggtitle("Holt Forecast")

<<<<<<< HEAD
=======
best <- MAN %>% gg_tsdisplay(.resid, lag_max = 24, plot_type = "histogram") # error'.resid' not found

>>>>>>> 9f46adac603dba3bb314cfb06983e812e91db7ca
MAN %>%
  gg_tsresiduals()+ ggtitle("MAN Residuals")

Holt %>%
  gg_tsresiduals() + ggtitle("Holt Residuals")

####5. Pick the real consumption expenditures and transform the series to create a series for the
####growth rate quarter on quarter. Plot the growth rate of real consumption expenditures against 
####the growth rate of disposable income. Do you think there is any relation?


cpi <- USMacroG[,"cpi"]

cpi_1 <- as_tsibble(cpi)



autoplot(cpi) + labs(y= "CPI $USD", title= "Consumption Expenditures over Time")
ggAcf(cpi, lag.max = 24) + labs(y= "Autocorrelation", title= "Autocorrelation of CPI in the US")
cpi_lambda <- BoxCox.lambda(USMacroG[,"cpi"])

BoxCox.lambda(cpi)
BC_cpi = (BoxCox (cpi, cpi_lambda))
autoplot(BoxCox(cpi, lambda=cpi_lambda))

cpi.ts <- as_tsibble(BC_cpi)
cpi.ts$observation <- 1:nrow(cpi.ts) 
cpi.expand = cpi.ts %>%
  mutate(Diff_year =  observation - lag(observation),
         Diff_growth = value - lag(value),
         Rate_percent = (Diff_growth / Diff_year)/value * 100)

print(ynew, n=300)

real_growth_rate_cpi = subset(cpi.expand, select = -c(observation, Diff_year, Diff_growth, value))
real_growth_rate_cpi

autoplot(real_growth_rate_cpi) + labs(title="Box Cox Diff Data over time")
ggAcf(real_growth_rate_cpi, lag.max = 24)


autoplot(real_growth_rate_cpi) + labs(title="CPI Box Cox Diff Data over time")
ggAcf(real_growth_rate_cpi, lag.max = 24)


gcpil<- as_tsibble(real_growth_rate_cpi)
gdpil<- as_tsibble(real_growth_rate_dpi)

cpi_dpi <- inner_join(gcpil, gdpil, by = "index")

ggplot(cpi_dpi, aes(index)) + 
  geom_line(aes(y = Rate_percent.x, colour = "Rate_Percent.x")) + 
  geom_line(aes(y = Rate_percent.y, colour = "Rate_percent.y"))
