# Diamond Analysis
# Aljohara Almedlej
# 29.09.2020
# A small case study for EDA and stats

#load packages
library(tidyverse)

#read in the data (csv format):
#newer methods from tidyr package
#underscore is more modern than dot
jems <- read_csv("data/diamonds.csv") # put quotes followed by tap to have the directory
#another convient way to read your data file
library(rio) # R i/o
jems1 <- import("data/diamonds.csv") # put quotes followed by tap to have the directory


#get fimiliar with our data
summary(jems)
names(jems)
glimpse(jems)


#more details
attributes(jems)
typeof(jems1)

#basic filtering:
#are there any diamonds with VVS2 (clarity) & Good (cut)
jems %>%
  filter(clarity == "VVS2" & cut == "Good")

# How many diamonds with a clarity of category “IF” are present in the data-set? 
Fil <- jems %>% filter(clarity == "IF") %>% count()
Fil
# OR
sum(jems$clarity == "IF")
#%>% shift + control + m


# What fraction of the total do they represent?
fractionOfIf <- Fil$n/nrow(jems)
fractionOfIf


# What proportion of the whole is made up of each category of clarity?
#propCalrity <- 
jems %>% 
  group_by(clarity) %>% 
  count()
table(jems$clarity)/nrow(jems)
#OR
jems %>%
  group_by(clarity) %>%
  count() %>%
  mutate(prop = n/nrow(jems))



# Reminder what group_by is
jems %>% 
  group_by(cut, color) %>% 
  group_split()


# What are the cheapest diamond price overall? 
jems %>%
  filter(price == min(price))
# What is the cheapest diamond price overall? 
min(jems$price)


# What is the range of diamond prices? 
range(jems$price)

# What is the average diamond price in each category of cut and color?
jems %>% 
  group_by(cut, color) %>% 
  summarise( avg = mean(price))



# Make a scatter plot that shows the price of a diamond as described by another continuous variable,like the carat.
ggplot(jems, aes(x = carat, y = price)) + 
  geom_point()

# what can you say about the relationship between these two variables?
# Do you think that you can use the carat weight of a diamond to predict its price?


# Using the functions we discuss earlier, and in class, apply a log10 transformation to both the price and carat.
# You can save these as new columns in the data set called price_log10 and carat_log10.

logPrice <- log10(jems$price) 
logCarat <- log10(jems$carat)
jems$carat_log10 <- logCarat
jems$price_log10 <- logPrice
jems


# Make a scatter plot that shows the price of a diamond as described by another continuous variable, like the carat.
ggplot(jems, aes(x = logCarat, y = logPrice)) + 
  geom_point()



#At the beginning of the course we used the PlantGrowth data set to produce a model. Can you use the same function we used earlier, lm() to recreate a model that describes the relatioship shown in the plot? 
#We’ll get into the details of exactly what that model is doing later on. For now, we’ll just take a look at it in action.
jems_lm <- lm(logPrice ~ logCarat, data = jems)
jems_lm
jems_lm$coefficients


# Now that we’ve described the diamond price given a single variable, can you display that on the plot? Try to use the geom_smooth() function to add this new layer.
ggplot(jems, aes(x = logCarat, y = logPrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")



