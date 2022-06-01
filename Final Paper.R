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

ts_goose <- Goose %>%
  mutate(month = yearmonth(Year)) %>%
  as_tsibble(key = c(Duck),
             index = month)

goose.ts <- ts(Goose$Duck, start = c(2018,1), end = c(2022,5), frequency = 12)

#examine the data
goose.ts

auto <- autoplot(goose.ts)
acfplot <- ggAcf(goose.ts)+ ggtitle("ACF of Data")

grid.arrange(auto, acfplot)

autoplot(decompose(goose.ts, type = c("multiplicative")))+
  labs(title = "Multiplicative Decomposition on the Dataset")
