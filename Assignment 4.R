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

jan_vic_elec<- vic_elec %>%
  filter(yearmonth(Time)== yearmonth("2014 Jan")) %>%
  index_by(Date= as_date(Time)) %>%
  summarise(Demand= sum(Demand), Temperature= max(Temperature))
jan_vic_elec

autoplot(jan_vic_elec)
