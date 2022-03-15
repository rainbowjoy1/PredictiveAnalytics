###*Question 1*
###Code needed from a & b:

jan_vic_elec <- vic_elec %>%
  filter(yearmonth(Time) == yearmonth("2014 Jan")) %>%
  index_by(Date = as_date(Time)) %>%
  summarise(Demand = sum(Demand), Temperature = max(Temperature))
fit <- jan_vic_elec %>%
  model(TSLM(Demand ~ Temperature))

