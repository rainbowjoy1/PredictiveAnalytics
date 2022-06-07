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
Goose

#Time series data
goose.ts <- ts(Goose$Duck, start = c(2018,1), end = c(2022,5), frequency = 12)

#tsibble transformation
df <-as_tsibble(goose.ts)

#data information
dim(goose.ts)
summary(goose.ts)

#plot of data
auto <- autoplot(df)+
  labs(y = "Number of Observations",
       title = "Bird IAS in Denmark")

#acf of the data
acfplot <- ggAcf(df)+ ggtitle("ACF of Data")

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
       title = "Box-Cox Transformation with Lambda= 0.7029993")

bx_df <- df %>% mutate(box_cox = box_cox(value, lambda))

#KPSS and ADF for testing diffs on the BCed data
summary(ur.kpss(bx_df %>% select(box_cox) %>% as.ts(), type = "tau"))
adf.test()


#Train/Test Split
#Will split out 2022


#Benchmarking Methods

# Fit the models
google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )
# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit %>%
  forecast(new_data = google_jan_2016)
# Plot the forecasts
google_fc %>%
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))

