

tuesdata <- tidytuesdayR::tt_load('2022-11-29')
wcmatches <- tuesdata$wcmatches
wccup <- tuesdata$worldcups

library(dplyr)
library(ggplot2)

wccup %>% ggplot(aes(x=year, y = attendance, fill = attendance)) + 
  geom_col() +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA) + 
  labs(title = 'Attendance at WC (1930-2018)',
       x = 'Year',
       y = 'Attendence',
       fill = NULL) +
  scale_y_continuous(labels = scales::comma) +
  #rotate labels 90 degrees to fit bar
  geom_text(aes(label = paste(round(attendance/1000000,2), 'M', sep=""), angle = 90, hjust = +1)) +
  theme(legend.position = "none") +
  theme(panel.grid.minor = element_blank())

            