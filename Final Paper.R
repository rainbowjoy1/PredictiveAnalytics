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
acfplot <- ggAcf(df, 12)+ ggtitle("ACF of Data")
pacfplot <- ggPacf(df)+ ggtitle("PACF of Data")
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
df %>%
  model(
    STL(value ~ trend(window = 13) +
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
       title = "Box-Cox Transformation with Lambda= -0.06884258")

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
    `Seasonal Naive` = SNAIVE(value ~ lag("year"))
  )
 # Produce forecasts for the trading days in January 2016
df_fc <- fit_df %>% forecast(h=12)

df_fc %>%
  autoplot(df, level = NULL) +
  labs(
    y = "IAS Observations",
    title = "Baseline Forecasts for IAS Bird Observations"
  ) +
  guides(colour = guide_legend(title = "Forecast"))



#Determingin differencing for ARIMA
difft <- bx_df %>%
  transmute(
    `Observations` = value,
    `Box-Cox Observations` = box_cox,
    `Seasonally Diffed Box-Cox` = difference(box_cox, 12)
  ) %>%
  pivot_longer(-index, names_to="Type", values_to="Sales") %>%
  mutate(
    Type = factor(Type, levels = c(
      "Observations",
      "Box-Cox Observations",
      "Seasonally Diffed Box-Cox"))
  )

difft %>%
  ggplot(aes(x = index, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Stationarity in IAS Data", y = NULL)

#KPSS and ADF for testing diffs on the BCed data

Seasonal_diff <- na.omit(difft) %>% filter(Type == "Seasonally Diffed Box-Cox")

summary(ur.kpss(Seasonal_diff %>% select(Sales) %>% as.ts(), type = "mu"))
summary(ur.df(Seasonal_diff %>% select(Sales) %>% as.ts(), type="none", selectlags = "AIC", lags = 12))


