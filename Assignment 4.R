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
  summarise(Demand= sum(Demand), Temperature = max(Temperature))

# a. Plot the data and find the regression model for Demand with temperature as an explanatory variable. Why
# is there a positive relationship?
ggplot(jan_vic_elec, aes(Date ,Temperature, col=Demand)) + 
  geom_point() + 
  stat_smooth() 


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

summarise(Demand= sum(Demand), Temperature= max(Temperature))
jan_vic_elec

autoplot(jan_vic_elec)

# Create a dataframe of new diamond data
demand.new <- data.frame(weight = c(12, 6, 5),
                           clarity = c(1.3, 1, 1.5),
                           color = c(5, 2, 3))

# Predict the value of the new diamonds using
#  the diamonds.lm regression model
predict(object = diamonds.lm,     # The regression model
        newdata = diamonds.new)   # dataframe of new data



