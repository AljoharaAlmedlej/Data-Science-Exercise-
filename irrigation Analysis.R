# irrigation analysis
# Aljohara Almedlej
# 01.10.2020
# a small case study
library(tidyverse)
irrigation <- read.csv("data/irrigation_wide.csv")


# examine the data
glimpse(irrigation)
summary(irrigation)
#
#


irrigation %>%
  filter(year == 2007) %>%
  select(ends_with("erica")) %>%
  sum()
irrigation_t <- irrigation %>%
  pivot_longer(-c(year))


# total by year
irrigation_t %>%
  group_by(year) %>%
  summarise(total=sum(value))


# top 2 counties in diff
irrigation_t %>%
  group_by(name) %>%
  summarise( diff = value[year==2007] - value[year==1980]) %>%
  arrange(-diff) %>% # it will ascending but with - it will be deciding
  slice(2) # the colum number i want to show

# shorter way for the past question
irrigation_t %>%
  group_by(name) %>%
  summarise( diff = value[year==2007] - value[year==1980]) %>%
  slice_max(diff,n=2)


#rate of change in each region yearly ?
irrigation_t <-  irrigation_t %>%
  arrange(name) %>%
  group_by(name) %>%
  mutate(rate= c(0,diff(value)/value[-length(value)]))


#the highest per region
irrigation_t  %>%
  slice_max(rate)


#the highest
irrigation_t  %>%
  ungroup() %>%
  slice_max(rate)

# the lowest
irrigation_t  %>%
  ungroup() %>%
  slice_min(rate)
