# tidytuesday | July 5, 2022 
# San Francisco Rentals
# @ntran119

rm(list = ls()) # clear environment

library(tidyverse)

rent <-read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

glimpse(rent) #explore the data

range(rent$year) # 2008-2018
n_distinct(rent$year) # 11 years
length(unique(rent$county)) # 11 unique counties
colSums(is.na(rent)) # counts NAs in each column

## QUESTIONS TO EXPLORE ##
# 1. How do rent prices change over years
# 2. Difference in rent prices from different counties
# 3. Predict rent price based on sqft (price per square feet in each county)
## QUESTIONS TO EXPLORE ##


# 1. How do rent prices change over years

q1df <- rent %>%
  select(year, county, price) %>%
  drop_na() 

# dropped 1394 NAs in county column, still have 199402 obs

q1df %>%
  group_by(year) %>%
  summarize(mean_price = mean(price)) %>%
  ggplot(aes(year, mean_price)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

q1df %>% ggplot(aes(year, price)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se= FALSE)

price_fit <- lm(price ~ year, data = q1df)

price_fit
summary(price_fit)

q1df_mean <- q1df %>%
  group_by(year) %>%
  summarize(mean_price = mean(price))

price_mean_fit <- lm(mean_price ~ year, data = q1df_mean)

summary(price_mean_fit)

q1df_mean %>%
  ggplot(aes(year, mean_price)) + geom_col() + geom_smooth(method = 'lm', se= FALSE)

# the mean price of rent increases by 86.53-100 per year on average from 2000-2018 in sanfrancisco

# 2. Difference in rent prices from different counties

q2df <- rent %>%
  select(year, county, price) %>%
  drop_na() 

q2df2 <- q2df %>%
  group_by(county) %>%
  summarize(mean = mean(price), median = median(price)) %>%
  pivot_longer(cols = c(mean,median), names_to = 'feature', values_to = 'value')

  ggplot(q2df2, aes(x=reorder(county, value), y=value, fill = feature)) +
    geom_col(position = position_dodge(), alpha = 0.75) +
    geom_text(aes(label = round(value)), vjust = -0.2, position = position_dodge(.9))

# most expensive rent prices occur in marin/sanfrancisco county, cheapest rents prices occur in solano county

ggplot(q2df, aes(county, price)) + geom_boxplot()

q2df %>%
  filter(price < 10000) %>%
  ggplot(aes(price)) + geom_histogram(binwidth = 500)
