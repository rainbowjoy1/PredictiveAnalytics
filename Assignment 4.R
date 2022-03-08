#line 1
library(ggplot2)
library(ggfortify)
library(forecast)
library(fpp3)
library(tidyr)
library(fable)
library(tsibble)
library(AER)
library(MASS)
library(caret)

vic_elec
head(vic_elec)
vic_elec %>% 