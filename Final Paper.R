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
library(readr)

Goose <- read_delim("Goose.csv", delim = ";", 
                    escape_double = FALSE, col_types = cols(Year = col_date(format = "%d/%m/%Y")), 
                    trim_ws = TRUE)

#read the csv file from github repository
Goose

#Time series data
goose.ts <- ts(Goose$Duck, start = c(2015,1), end = c(2022,5), frequency = 12)

#tsibble transformation
df <-as_tsibble(goose.ts)

#data information
dim(goose.ts)
summary(goose.ts)

#plot of data
auto <- autoplot(df)+
  labs(y = "Number of Observations",
       title = "Bird IAS in Denmark")

auto

#acf of the data
acfplot <- ggAcf(df, 24)+ ggtitle("ACF of Data")
pacfplot <- ggPacf(df, 24)+ ggtitle("PACF of Data")
acfplot
pacfplot

#seasonal plot of data
df %>%
  gg_season(value, labels = "both") +
  labs(y = "Observed Birds",
       title = "Seasonal plot: IAS Bird Observations")

#ACF and Data plot
grid.arrange(auto, acfplot)

#STL Decomposition
bx_df %>%
  model(
    STL(box_cox ~ trend(window = 13) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

#Guerro test for lambda
lambda <- df %>%
  features(value, features = guerrero) %>%
  pull(lambda_guerrero)

#Box-Cox data
df %>%
  autoplot(box_cox(value, lambda))+
  labs(y = "",
       title = "Box-Cox Transformation with Lambda= -0.1760071")

bx_df <- df %>% mutate(box_cox = box_cox(value, lambda))

lambda

#Train/Test Split
#Will split out 2022
windowl <- 12L
train_df <- head(df, round(length(df) - windowl))
test_df <- tail(df, windowl)
test_df.ts <- as_tsibble(test_df)
train_df.ts <- as_tsibble(train_df)

#Benchmarking Methods

# Fit the models
 fit_df<- train_df.ts %>%
  model(
    Mean = MEAN(value),
    `Naïve` = NAIVE(value),
    Drift = NAIVE(value ~ drift()),
    `Seasonal Naive` = SNAIVE(value ~ lag("year")))
 
 # Produce forecast
df_fc <- fit_df %>% forecast(h=12)

df_fc %>%
  autoplot(df, level = NULL) +
  labs(
    y = "IAS Observations",
    title = "Baseline Forecasts for IAS Bird Observations"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

#ETS

windowl <- 12L
train_df <- head(bx_df, round(length(bx_df) - windowl))
test_df <- tail(bx_df, windowl)
test_df.ts <- as_tsibble(test_df)
train_df.ts <- as_tsibble(train_df)

goose.train <- bx_df %>%
  filter_index("2015 Jan" ~ "2021 May")

goose.test <- bx_df %>%
  filter_index("2021 Jun" ~ .)

auto_selection <- goose.train %>% model(what = ETS(box_cox))

report(auto_selection)


dcmp <- goose.train %>%
  model(STL(box_cox)) %>%
  components()

dcmp %>%
  as_tsibble() %>%
  autoplot(season_adjust)


stlAAA <- decomposition_model(
  STL(box_cox),
  ETS(season_adjust ~ error("A") + trend("Ad") + season("A")))

stlAAM <- decomposition_model(
  STL(box_cox),
  ETS(season_adjust ~ error("A") + trend("Ad") + season("M")))

AAA <- ETS(box_cox ~ error("A") + trend("Ad") + season("A"))
AAM <- ETS(box_cox ~ error("A") + trend("Ad") + season("M"))
MNN <- ETS(box_cox ~ error("M") + trend("N") + season("N"))

fit <- goose.train %>%
  model(
    stlAAA = stlAAA,
    stlAAM = stlAAM,
    AAA = AAA,
    AAM = AAM,
    MNN = MNN
  )

fit %>% forecast(h=12) %>% accuracy(bx_df)

#selected the decomposed AAM model

final_fit <- goose.train %>% model(stlAAM = stlAAM)

whole_fit <- df %>% model(stlAAM = stlAAM)

forecast <- final_fit %>% forecast(h = 12)

inv <- InvBoxCox(forecast$.mean, lambda)
prediction.t <- ts(InvBoxCox(forecast$.mean,lambda), start = c(2021,6), end = c(2022,5), frequency = 12)

df_1 <- as.ts(df)

ts.plot(df_1, prediction, gpars = list(col = c("blue", "orange")))

best <- final_fit
augment(best) %>% gg_tsdisplay(.resid, lag_max = 24, plot_type = "histogram")

augment(best) %>%
  features(.innov, ljung_box, lag = 24, dof = 4)

#determining differencing for arima
new <- bx_df$box_cox
seasonal_diffed <- diff(new,12)
seasonal_diffed.ts <- as_tsibble(ts(seasonal_diffed, start = c(2016,1), end = c(2022,5), frequency = 12))
double_diffed <- diff(seasonal_diffed,1)
double_diffed.ts <- as_tsibble(ts(double_diffed, start = c(2016,1), end = c(2022,5), frequency = 12))

seasonal_diffed.ts


bx <- autoplot(bx_df, .vars=box_cox, axis.text.x = FALSE)+ labs(title = "Box-Coxed")
sd <- autoplot(seasonal_diffed.ts)+
  labs(title = "Seasonal Differenced")
dd <- autoplot(double_diffed.ts)+
  labs(title = "Double Differenced")
grid.arrange(auto, bx, sd, dd, ncol=1, nrow =4)

#seasonal diffed data tests
summary(ur.kpss(seasonal_diffed.ts %>% as.ts(), type = "mu"))
summary(ur.df(seasonal_diffed.ts %>% as.ts(), type="none", selectlags = "AIC", lags = 12))


#Double DIffed Data tests
summary(ur.kpss(double_diffed.ts %>% as.ts(), type = "mu"))
summary(ur.df(double_diffed.ts %>% as.ts(), type="none", selectlags = "AIC", lags = 12))


auto.fit<- train_df.ts %>% model(ARIMA(value, ic = "aic", stepwise = FALSE, approx = FALSE))
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