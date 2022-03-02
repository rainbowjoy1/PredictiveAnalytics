library(fpp3)
library(tidyr)
library(fable)

#Question 1
global_economy %>% 
  filter(Country=="United States")%>% autoplot(GDP/Population)+ labs(title= "GDP per Capita", y= "US $")
#why linearize it? Is it so that we can run algorithms on it
#try other l

autoplot(box_cox(GDP, lambda?))

#we changed this to GDP per capita to normalize the GDP values in comparison to other countries

aus_livestock %>%
  filter(Animal=="Bulls, bullocks and steers", State=="Victoria") %>% autoplot()
###Is there a way to find info about built in dataframes
#we can normalize the time series by day since months are not equal

aus_livestock
vic_elec %>% autoplot()

aus_production %>% select("Gas")%>% autoplot()

#Question 2

#basic Model
China_GDP <- global_economy %>% 
  filter(Country=="China")%>% select("Year","GDP")

fit <- China_GDP %>%
  model(ETS(GDP))
report(fit)


fit %>%
  forecast(h = 20) %>%
  autoplot(China_GDP)

#damped Model

China_GDP <- as_tsibble(China_GDP)
China_GDP %>% autoplot(GDP) +
  labs(x="Years", y="Money",
       title = "GDP in China")

China_GDP %>%
  stretch_tsibble(.init = 10) %>%
  model(
    SES = ETS(GDP ~ error("A") + trend("N") + season("N")),
    Holt = ETS(GDP ~ error("A") + trend("A") + season("N")),
    Damped = ETS(GDP ~ error("A") + trend("Ad") +
                   season("N"))
  ) %>%
  forecast(h = 20) %>%
  accuracy(China_GDP)

#Question 3

##8.10 a
table(tourism["State"])
table(tourism["Purpose"])
tourism

Aus_Trips<- tourism %>% summarise(Trips = sum(Trips))
Aus_Trips %>% autoplot(Trips)
# fairly stable data trends with some 

##8.10 b
dcmp <- Aus_Trips %>%
  modol(stl = STL(Trips))
components(dcmp) %>% autoplot()

Aus_Trips_Adj <- components(dcmp) %>% select("season_adjust")

Aus_Trips_Adj
##8.10 c and d

Aus_Trips_Adj %>%
  model(
    `Holt's method` = ETS(season_adjust ~ error("A") +
                            trend("A") + season("N")),
    `Damped Holt's method` = ETS(season_adjust ~ error("A") +
                                   trend("Ad", phi = 0.9) + season("N"),
    )
  ) %>%
  forecast(h = 8) %>%
  autoplot(Aus_Trips_Adj, level = NULL) +
  labs(title = "Australian Trips",
       y = "Times") +
  guides(colour = guide_legend(title = "Forecast"))

#Question 8.10 e

fit <- Aus_Trips %>%
  model(ETS(Trips))
report(fit)

#AAA = Aus_Trips%>% model(ETS(Trips ~ error("A") + trend("A") + season("A")))
#AAN = Aus_Trips_Adj %>% model(ETS(season_adjust ~ error("A") + trend("A") + season("N")))
#AADN = Aus_Trips_Adj %>% model(ETS(season_adjust ~ error("A") + trend("Ad", phi = 0.9) + season("N")))

#Question 8.10 f
Aus_Trips %>%
  stretch_tsibble(.init = 10) %>%
  model(
    SES = ETS(Trips ~ error("A") + trend("A") + season("A")),
  ) %>%
  forecast(h = 8) %>%
  accuracy(Aus_Trips)

Aus_Trips_Adj %>%
  stretch_tsibble(.init = 10) %>%
  model(
    Holt = ETS(season_adjust ~ error("A") + trend("A") + season("N")),
    Damped = ETS(season_adjust ~ error("A") + trend("Ad") +
                   season("N"))
  ) %>%
  forecast(h = 8) %>%
  accuracy(Aus_Trips_Adj)
#Holt Damped model gives the better in sample fit due to a lower RMSE value

#Question 8.10g


#Question 8.10h

AADN = Aus_Trips_Adj %>% model(ETS(season_adjust ~ error("A") + trend("Ad", phi = 0.9) + season("N")))
AADN %>% gg_tsresiduals()

#Question 8.15

Aus_Trips_Adj %>%
  model(
    `Holt's multiplicative method` = ETS(season_adjust ~ error("A") +
                            trend("A") + season("N")),
    `MAM Method` = ETS(season_adjust ~ error("M") +
                                   trend("A") + season("M"),
    )
  ) %>%
  forecast(h = 8) %>%
  autoplot(Aus_Trips_Adj, level = NULL) +
  labs(title = "Australian Trips",
       y = "Times") +
  guides(colour = guide_legend(title = "Forecast"))
