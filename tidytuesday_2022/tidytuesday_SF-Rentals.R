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
# 3. Predict rent price based on sqft
## QUESTIONS TO EXPLORE ##

#### 1. How do rent prices change over years ####
q1df <- rent %>%
  select(year, county, price) %>%
  drop_na()

#quick exploratory data analysis
q1df %>% ggplot(aes(group = year, y = price)) + geom_boxplot()
q1df %>% ggplot(aes(price)) + geom_histogram()

#there are some prices that are probably yearly rather than monthly, some prices are 10000<

mean(q1df$price) #2135
median(q1df$price)#1800

# we need to filter out large prices, but how?
# assign every price a zscore and filter out a zscore < 3
# 3 standard deviations = 99.7% of values

var_price <- var(q1df$price)
sd_price <- sd(q1df$price)

q1df_z <- q1df %>%
  mutate(zscore = abs((price - mean(q1df$price))/sd_price)) %>%
  filter(zscore < 3)

q1df_z %>% ggplot(aes(price)) + geom_histogram()

#the new histogram looks better, it is skewed a bit but that is okay, we have removed extreme rent prices

q1df_z %>%
  group_by(year) %>%
  summarize(mean_price = mean(price)) %>%
  ggplot(aes(year, mean_price)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

q1lm <- lm(price~year, data = q1df_z)

q1lm

# the price of rent increases by 86.92 per year
#### 2. Difference in rent prices from different counties ####

q2df <- q1df_z %>%
  group_by(county) %>%
  summarize(mean = mean(price), median = median(price)) %>%
  pivot_longer(cols = c('mean', 'median'), names_to = 'feature', values_to = 'value')

q2df %>%
  ggplot(aes(reorder(county,value), value, fill = feature)) +
  geom_col( position = "dodge") +
  geom_text(aes(label = round(value)), position = position_dodge(width = .9)) +
  coord_flip()

# generally, SF and marin have the highest overall rent

