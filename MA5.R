###*Question 1*
###Code needed from a & b:

library(fpp3)

jan_vic_elec <- vic_elec %>%
  filter(yearmonth(Time) == yearmonth("2014 Jan")) %>%
  index_by(Date = as_date(Time)) %>%
  summarise(Demand = sum(Demand), Temperature = max(Temperature))

jan_vic_elec

fit <- jan_vic_elec %>%
  model(TSLM(Demand ~ Temperature))

#forecast 15

fit %>% forecast(h=2) %>% autoplot()
