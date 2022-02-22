library(fpp3)
library(tidyr)

#Question 1
us_employment %>%
  filter(Title=="Total Private")%>% autoplot()

us_employment %>%
  filter(Title=="Total Private")%>% gg_season()

us_employment %>%
  filter(Title=="Total Private")%>% gg_subseries()

us_employment %>%
  filter(Title=="Total Private")%>% gg_lag()

us_employment %>%
  filter(Title=="Total Private")%>% ACF(Employed)%>% autoplot()

aus_production

aus_production %>% autoplot(Bricks)



#Question 2
global_economy %>% 
  filter(Country=="United States")%>% autoplot(GDP/Population)+ labs(title= "GDP per Capita", y= "US $")

aus_livestock %>%
  filter(Animal=="Bulls, bullocks and steers", State=="Victoria") %>% autoplot()

table(aus_livestock["State"])

#Question 3
#a. in figure 3.19 you can see that the civilian labor force has been trending upwards consistently
# the graphs show that there is a strong seasonality to the growth with high points in September and December in more recent history 
#and high points in March in the past. Seasonality has fluctuated over time but it is strong

#b. you can see the recession most clearly in the remainder graph in the decomposition. This is because the seasonality dropped dramatically.


#Question 4
#a.

#Retail_new=aus_retail %>% separate("Month", c("Year", "Month"))
#Retail_Training= Retail_new %>% filter(Year<2014)
#Retail_Test= Retail_new %>% filter(Year>=2014)


Turnover<- aus_retail %>% filter(Industry=="Takeaway food services") %>% summarise(Turnover = sum(Turnover))



train <- Turnover %>% filter_index(.~"2013 Dec") 

# Fit the models
food_fit <- train %>% model(Mean = MEAN(Turnover),
                            naive= NAIVE(Turnover),
                            drift = RW(Turnover ~ drift()),
                            `Seasonal naive`= SNAIVE(Turnover))

food_fc <- food_fit %>% forecast(h = 48)


food_fc %>%
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(Turnover, "2014 Jan" ~ .),
    colour = "pink") +
  labs(
    y = "Turnover",
    title = "Forecasts for monthly Takout Services") +
  guides(colour = guide_legend(title = "Forecast"))


food_fc %>%
  accuracy(Turnover) %>% arrange(MASE)

food_fit %>%
  select(naive) %>% gg_tsresiduals()
