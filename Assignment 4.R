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
<<<<<<< HEAD
  summarise(Demand= sum(Demand), Temperature = max(Temperature))

# a. Plot the data and find the regression model for Demand with temperature as an explanatory variable. Why
# is there a positive relationship?
jan_vic_elec %>% autoplot()


















#fit a regression model
model <- lm(Demand~Temperature, data=jan_vic_elec)

#b.Produce a residual plot. Is the model adequate? Are there any outliers or influential observations?

#get list of residuals 
res <- resid(model)
plot(fitted(model), res)
qqnorm(res)
qqline(res)
plot(density(res))
#ddaacdadsada
=======
  summarise(Demand= sum(Demand), Temperature= max(Temperature))
jan_vic_elec

autoplot(jan_vic_elec)
>>>>>>> f81e7ad991ead6cf98ec77df3dce5b4fb122b4a3
