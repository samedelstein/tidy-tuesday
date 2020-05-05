#Load libraries
library(tidyverse)
library(lubridate)
library(gghighlight)
#Read in data
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')



#How have ratings changed over time?
user_reviews %>%
  mutate(week = week(date)) %>%
  group_by(week) %>%
  mutate(avg_grade = median(grade),
         count = n()) %>%
  ggplot(aes(week, avg_grade)) +
  geom_jitter(aes(week, grade),color = "steelblue", alpha=.2) +
  geom_line(color = "black", size = 2) +
  labs(title = "Median Animal Crossing Rating by Week",
       subtitle = "More ratings initially, sudden ratings jump later",
       x = "Week of 2020", 
       y = "Average Grade") +
  ggthemes::theme_fivethirtyeight()

#When are female and male characters born?
villagers %>%
  mutate(date = as.Date(birthday, "%m-%d"),
         month = month(date)) %>%
  group_by(gender, month) %>%
  tally() %>%
  ggplot(aes(month, n, fill = gender)) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_continuous(breaks=seq(1, 12, by = 1)) +
  labs(title = "Month of Birthday by Gender",
       subtitle = "Female Characters have more birthdays from Jan-April",
       x = "Month",
       y = "Number of Characters with Birthday") +
  ggthemes::theme_fivethirtyeight()

#Do personalities change by astrological sign?
villagers %>%
  mutate(date = as.Date(birthday, "%m-%d"),
         astrological_sign = case_when(date >= '2020-03-21' & date <= '2020-04-19' ~ "Aries",
                                       date >= '2020-04-20' & date <= '2020-05-20' ~"Taurus",
                                       date >= '2020-05-21' & date <= '2020-06-20' ~"Gemini",
                                       date >= '2020-06-21' & date <= '2020-07-22' ~"Cancer",
                                       date >= '2020-07-23' & date <= '2020-08-22' ~"Leo",
                                       date >= '2020-08-23' & date <= '2020-09-22' ~"Virgo",
                                       date >= '2020-09-23' & date <= '2020-10-22' ~"Libra",
                                       date >= '2020-10-24' & date <= '2020-11-21' ~"Scorpio",
                                       date >= '2020-11-22' & date <= '2020-12-21' ~"Sagittarius",
                                       (date >= '2020-12-22' & date <= '2020-12-31') | (date >='2020-01-01' & date <= '2020-01-19') ~"Capricorn",
                                       date >= '2020-01-20' & date <= '2020-02-18' ~"Aquarius",
                                       TRUE ~ 'Pisces')) %>%
  group_by(astrological_sign, personality) %>%
  tally() %>%
  group_by(astrological_sign) %>%
  mutate(count = sum(n),
         percent = round((n/count)*100)) %>%
  ungroup(astrological_sign) %>%
  mutate(astrological_sign = fct_relevel(astrological_sign, "Aquarius", 'Pisces', 'Aries', 'Taurus', 'Gemini', 'Cancer', 'Leo', 'Virgo', 'Libra', 'Scorpio', 'Sagittarius', 'Capricorn')) %>%
  ggplot(aes(astrological_sign, n, fill = personality, label = percent)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(x = astrological_sign, y = n, label = paste0(percent, "%")),
            position = position_stack(vjust = .5)) +
  labs(title = "Personality by Astrological Sign",
       subtitle = "Pisces are peppy",
       x = "",
       y = "Count") +
  ggthemes::theme_fivethirtyeight()


