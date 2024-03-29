
###Part 1
#1. Plot the data and the autocorrelation function. Decompose your time series into trend, cycle,
#and seasonal component with a methodology of your choice (classical decomposition, SEATS, etc). 
#Discuss the properties of the series and the limitation of the methodology you chose (5 lines).


library(ggplot2)
library(ggfortify)
library(forecast)
library(dplyr)
library(fpp3)
library(tsibble)
library(gridExtra)
library(latex2exp)
library(tseries)
library(urca)
library(tidyverse)


#read the csv file from github repository
emp <- read.csv("C://Study//Semester2//Predictive Analytics//Github_R//emp.csv", header=TRUE)
#emp <- read.csv("emp.csv", header=TRUE)
emp
#convert the date column to date format
emp[["DATE"]] <- as.Date(emp[["DATE"]])

#examine the data
dim(emp)
summary(emp)

#convert the data to a time series to allow the SCF to run and place the data in monthly format
emp.ts <- ts(emp[,2], start = c(1948,1), end = c(2007,12), frequency = 12)

#generate plots of the data
auto <- autoplot(emp.ts)+ ylab("")+ggtitle("Plot of Time Series Data")
acfplot <- ggAcf(emp.ts, )+ ggtitle("ACF of Data")

#arrange the plots into one image
grid.arrange(auto, acfplot)

autoplot(decompose(emp.ts, type = c("multiplicative")))+
  labs(title = "Multiplicative Decomposition on the Dataset")

#'*Our time series, at first glance, shows no strong seasonality or cycle but shows a strong*
#'*trend upwards. The data could contain some cycles as seen in the small repeated slumps and* 
#'*inclines of the line. The data could be multiplicative due to the small curve near the bottom of the line.*
#'*The auto correlations show a very strong significant correlation.* 
#'
#'*Our decomposition shows that there is a strong trend in the data. It also shows that there is* 
#'*strong seasonality and some cycles. It is important to note that we used classical multiplicative decomposition*
#'*Which assumes that there is a seasonal component and that it is constant from year to year so it is possible*
#'*that the seasonal element is overstated*
#'
#'*A more accurate, but more time consuming, method would be to take the data and use STL*
#'*but we would have to run the Guerrero feature to determine the lambda so we can run a Box-Cox transformation*
#'*because our data is somewhere between additive and multiplicative*

#2. Transform your data by taking natural logarithm or with Box and Cox methodology. Repeat
#the analysis of point 1.1: how does it change? Decide whether to use original or transformed
#data in the rest of the assignment. Motivate your choice.

####Note to do run guerro and then a box cox based on returned lambda

lambda <- guerrero(emp.ts)

bx.emp <- box_cox(emp.ts, lambda)

plot.ts(bx.emp, ylab= "", main = latex2exp::TeX(paste0("Transformed EMP data with $\\lambda$ = ", round(lambda,2))))

#'*Our data has changed primarily by being less multiplicative. The data is far more linear and seasonality/trends* 
#'*seem to be more obvious. Because we ran a Guerrero test and it did not return a 1 lambda value the data*
#'*does need to be transformed because it is nonlinear*

#3. Based on the results in point 1.1 and the choice in point 1.2, discuss what is the most accurate
#functional form for the KPSS and ADF tests. Then, perform both tests to check stationarity
#and discuss the results.


#'*Based on the data shown in part 1 the data is best represented by a random walk with drift.*
#'*This means that we should use tau for a KPSS formula and trend with AIC for DF test*

summary(ur.kpss(bx.emp, type = c("tau")))


#'*In this test, our null hypothesis is that the data is stationary. So small p-values*
#'*(smaller than .05) tell us that we need to use differencing. Since our returned p-value*
#'*is .01 we can reject the null hypothesis and determine that the data is not stationary.*

adf.test(bx.emp)

#'*The adf test has a null hypothesis that there is a unit root. The alternate hypothesis*
#'*says that the time series is stationary. The ADF test returned a p-value of .3226 which is*
#'*is not less than .05 we fail to reject the null hypothesis and conclude that our data is non-stationary*

#4. If the series is not stationary, follow the approach presented in class to make it stationary.
#When the series is stationary, look at the autocorrelation and partial autocorrelation function,
#choose an ARIMA model for the series and write down the equation of the model in the form
#ARIMA(p,d,q)(P,D,Q)[m]. (Hint: if there is a deterministic trend, you need to detrend the time series)

ndiffs(
  bx.emp,
  alpha = 0.05,
  test = c("kpss"),
  type = c("trend"))

#'*To determine how many diffs we should take we ran ndiffs via kpss. It determined we should*
#'*run one diff.*

emp.dif <- diff(bx.emp)
  
kpss.test(emp.dif, null="Trend")
adf.test(emp.dif)

# The adf test returned the p-value of 0.01 which is smaller than 0.05, so we can reject the null hypothesis and
# conclude that the data is stationary.

dif.ACF <- ggAcf(emp.dif)+ ggtitle("ACF of Stationary B-C Data")

dif.PACF <- ggPacf(emp.dif)+ ggtitle("PACF of Stationary B-C Data")

grid.arrange(dif.ACF, dif.PACF)

autoplot(emp.dif)

####################Now we need to choose a model(Choose the best p,d,q that perform best on our data

#We try out auto.arima() as an alternative model
auto = auto.arima(emp.dif)
auto

#'*We have a lot of significant spikes in our ACF and PACF. We have implemented second differencing to the data*
#'*but it turned out that we have over-differencing. So, we chose to apply differencing 1 time.*
#'*As,well as, The unitroot_ndiffs() suggests 1 time difference for d and D. However, we have differenced our data 1 time, so we choose d as 0.*
#'*We examine ACF and PACF of the stationary data. we can see that there is no exponential decay nor sinusoidal behavior.*
#'*We know that p and q are not both zero because our ACF and PACF do not show white noise.* 
#'*We have the most significant lag at lag 2 in ACF. So, we determine that p is 2.* 
#'*We identify significant lag at lag 12 and 24. We determine that we need a seasonal component of ARIMA model as well.*
#'*The ACF plot shows a sharp cut off and the lag 1 of ACF is negative. So, we determine that q is 1.*
 

###Part 2
#In this part use the data based on the decision in point 1.2 (i.e. either the original data or the log/box-cox transformed data).

bx.emp

#1. Split your sample into a train set and a test set. Estimate one ARIMA model by using AIC: is the estimated model 
#coherent with the information acquired in point 1.3? Discuss the properties of the residuals. Can the model be improved? 
# If it is the case, modify it discuss the new model performance.

#'*Split on 20% of the data*  
#windowl <- 144L
#train <- head(bx.emp, round(length(bx.emp) - windowl))
#test <- tail(bx.emp, windowl)
#test.ts <- as_tsibble(test)
#train.ts <- as_tsibble(train)

windowl <- 144L
train_emp <- head(emp.ts, round(length(emp.ts) - windowl))
test_emp <- tail(emp.ts, windowl)
test_emp.ts <- as_tsibble(test_emp)
train_emp.ts <- as_tsibble(train_emp)

auto.fit<- train_emp.ts %>% model(ARIMA(value,ic = "aic", stepwise = FALSE, approx = FALSE))
auto.fit
gg_tsresiduals(auto.fit) + ggtitle("Automatic ARIMA residuals plot")

#'*We believe that the auto ARIMA using AIC is improperly fitting a model because the d of non-*
#'*seasonal component is returning 2 but when we ran the ndiffs and KPSS we found that 1 dif was the most*
#'*appropriate for our data set.We allso ran an additional diff and found that our data was overdifferentiated*
#'*Our residual plot shows that the data does not have a zero mean. The plot has spikes*
#'*up to 4,000. The acf has 2 significant spikes, and the distribution is not normal.*
#'*The tails are uneven and the shape is too tall. According to the residuals the model can be improved.*

fit<- train_emp.ts %>% model(auto123101 = ARIMA(value, ic = "aic", stepwise = FALSE, approx = FALSE), 

                         arima211001 = ARIMA(value ~ 0 + pdq(2,1,1) + PDQ(0,0,1)),
                         
                         arima212012 = ARIMA(value ~ 0 + pdq(2,1,2) + PDQ(0,1,2)),

                         arima211202 = ARIMA(value ~ 0 + pdq(2,1,1) + PDQ(2,0,2)))

fit %>% pivot_longer(everything(), names_to = "Model Name", values_to = "Orders")

glance(fit) %>% arrange(AICc) %>% select(.model:BIC)


#2. Perform a forecast with the model selected in point 2.1 and in point 1.4 
#(note: if you had detrended or differentiated you need to change something in the formula). Plot the forecasts
#of each model with the original data in two separate graphs and discuss the measures of
#accuracy: what is the best model?

auto_fc <- forecast(fit, h=144) %>% filter(.model=='auto123101')
auto_fc_plot <- auto_fc %>% autoplot(train_emp.ts) +ggtitle("Plot of the Automatic ARIMA's forecast")

manual_fc <- forecast(fit, h=144) %>% filter(.model=='arima212012')
manual_fc_plot <- manual_fc %>% autoplot(train_emp.ts) +ggtitle("Plot of the selected model's forecast")

grid.arrange(manual_fc_plot, auto_fc_plot)

accuracy(auto_fc, test_emp.ts)
accuracy(manual_fc, test_emp.ts)

