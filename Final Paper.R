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
acfplot <- ggAcf(bx_df$box_cox, 36)+ ggtitle("ACF of Data")
pacfplot <- ggPacf(bx_df$box_cox, 36)+ ggtitle("PACF of Data")
acfplot
pacfplot

df %>% features(value, ljung_box, lag = 24, dof = 0)

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
lambda

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
train_df <- head(df, 77)
test_df <- tail(df, windowl)
test_df.ts <- as_tsibble(test_df)
train_df.ts <- as_tsibble(train_df)

#Benchmarking Methods
test_df
print(train_df, n=781)

# Fit the models
 fit_df<- train_df.ts %>%
  model(
    Mean = MEAN(value),
    `Naive` = NAIVE(value),
    Drift = NAIVE(value ~ drift()),
    `Seasonal Naive` = SNAIVE(value ~ lag("year")))
 
 # Produce forecast
df_fc <- fit_df %>% forecast(h=12)

df_filter <- df %>% filter_index( "2019 Jan" ~. )

df_fc %>%
  autoplot(df_filter, level = NULL) +
  labs(
    y = "IAS Observations",
    title = "Baseline Forecasts for IAS Bird Observations"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(df_fc, df, D = 1)

#ETS


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


stlAnA <- decomposition_model(
  STL(box_cox),
  ETS(season_adjust ~ error("A") + trend("N") + season("A")))

stlANM <- decomposition_model(
  STL(box_cox),
  ETS(season_adjust ~ error("A") + trend("N") + season("M")))

ANA <- ETS(box_cox ~ error("A") + trend("N") + season("A"))
ANM <- ETS(box_cox ~ error("A") + trend("N") + season("M"))
MNN <- ETS(box_cox ~ error("M") + trend("N") + season("N"))

fit <- goose.train %>%
  model(
    stlANA = stlANA,
    stlANM = stlANM,
    ANA = ANA,
    ANM = ANM,
    MNN = MNN
  )

fit %>% forecast(h=12) %>% accuracy(bx_df, D=1)

#selected the decomposed AAM model

final_fit <- goose.train %>% model(stlAAM = stlAAM)

whole_fit <- df %>% model(stlAAM = stlAAM)

forecast <- final_fit %>% forecast(h = 12)

inv <- InvBoxCox(forecast$.mean, lambda)
prediction.t <- ts(InvBoxCox(forecast$.mean,lambda), start = c(2021,6), end = c(2022,5), frequency = 12)

df_1 <- as.ts(df)

ts.plot(df_1, prediction.t, gpars = list(col = c("blue", "orange")))

best <- final_fit
augment(best) %>% gg_tsdisplay(.resid, lag_max = 24, plot_type = "histogram")

augment(best) %>%
  features(.innov, ljung_box, lag = 24, dof = 4)

#determining differencing for arima
new <- bx_df$box_cox

unitroot_ndiffs(bx_df$value, alpha = 0.05, unitroot_fn = ~unitroot_kpss(.)["kpss_pvalue"], differences = 0:2, .period = 12)

seasonal_diffed <- diff(new,12)
seasonal_diffed.ts <- as_tsibble(ts(seasonal_diffed, start = c(2016,1), end = c(2022,5), frequency = 12))

bx <- autoplot(bx_df, .vars=box_cox)+ labs(title = "Box-Coxed")
sd <- autoplot(seasonal_diffed.ts)+
  labs(title = "Seasonal Differenced")
dd <- autoplot(double_diffed.ts)+
  labs(title = "Double Differenced")
grid.arrange(auto, bx, sd, ncol=1, nrow =3)

autoplot(seasonal_diffed.ts)+ labs(title = "Seasonally DIfferenced Data")
autoplot(double_diffed.ts)

#seasonal diffed data tests
summary(ur.kpss(seasonal_diffed.ts %>% as.ts(), type = "mu", use.lag = 4))
summary(ur.df(seasonal_diffed.ts %>% as.ts(), type="none", selectlags = "AIC"))


acfplot <- ggAcf(seasonal_diffed.ts, 24)+ ggtitle("ACF of Seasonally Differenced Data")
pacfplot <- ggPacf(seasonal_diffed.ts, 24)+ ggtitle("PACF of Seasonally Differenced Data")
acfplot
pacfplot

auto.fit<- train_df.ts %>% model(ARIMA(value, ic = "aic", stepwise = FALSE, approx = FALSE))
auto.fit
gg_tsresiduals(auto.fit) + ggtitle("Automatic ARIMA residuals plot")


fit<- train_df.ts %>% model(auto = ARIMA(value, ic = "aic", stepwise = FALSE, approx = FALSE), 
                             
                             arima1 = ARIMA(value ~ 0 + pdq(1,0,1) + PDQ(0,1,1)),
                             
                             arima2 = ARIMA(value ~ 0 + pdq(1,0,1) + PDQ(0,1,0)),
                             
                             arima3 = ARIMA(value ~ 0 + pdq(1,0,1) + PDQ(1,1,0)),
                            
                            arima6 = ARIMA(value ~ 0 + pdq(1,0,1) + PDQ(1,1,1)))

fit %>% pivot_longer(everything(), names_to = "Model Name", values_to = "Orders")

glance(fit) %>% arrange(AICc) %>% select(.model:BIC)
fit %>% select(arima1) %>% gg_tsresiduals(lag=24)


auto_fc <- forecast(fit, h=12) %>% filter(.model=='arima1')
auto_fc_plot <- auto_fc %>% autoplot(df) +ggtitle("Plot of the (1,0,1)(0,1,1) ARIMA's forecast")
auto_fc_plot
manual_fc <- forecast(fit, h=144) %>% filter(.model=='arima212012')
manual_fc_plot <- manual_fc %>% autoplot(train_emp.ts) +ggtitle("Plot of the selected model's forecast")

grid.arrange(manual_fc_plot, auto_fc_plot)
auto_fc
invarima <- InvBoxCox(auto_fc$.mean, lambda)
accuracy(auto_fc, bx_df)
accuracy(manual_fc, test_emp.ts)