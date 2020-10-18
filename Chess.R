# Chess 
# Aljohara Almedlej
# 09.10.2020

# Load the library
library(tidyverse)

# Read the data
chess <- read_csv("data/games.csv")
chess

# show dataset as tibble
chess <- as_tibble(chess)
chess

# view the data
View(chess) # or just click it in the GE

# Inspecting the raw data
head(chess) # shows first 6 columns
tail(chess) # shows last 6 columns
class(chess) # the class of the dataset
length(chess) # how many columns do I have
nrow(chess) # how many rows do I have

chessColNames <- names(chess) # register the column names in the global environment for more exploraion later on
chessColNames

# Exploring the Chess data

# How many games were won? 
wins <- length(which(chess$victory_status == "mate"))
wins

# How many resigns?
resigns <- length(which(chess$victory_status == "resign"))
resigns

# How many outoftime?
outOfTime <- length(which(chess$victory_status == "outoftime"))
outOfTime

# How many draws?
draws <- length(which(chess$victory_status == "draw"))
draws

# comparing to know what is the heighest result
max(chess$victory_status) # we got resign

# What is the heighest number of turns?
max(chess$turns)


# Summary of the Chess dataset
summary(chess)


# Inferential Statistics 
chess_lm <- lm(turns ~ winner , data = chess)
chess_lm

# ANOVA
anova(chess_lm)


chess_lm$residuals
chess_lm$effects

chess_model <- chess_lm$model


# plots
# catigorical 
ggplot(chess, aes(winner)) + 
  geom_bar(c)

# Another polt
ggplot(chess, aes( turns, victory_status)) +
  geom_jitter(width = 1) +
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult = 0.5), 
               col = "blue") 

# Another plot
ggplot(chess, aes( turns, winner)) +
  geom_boxplot()



# Descriptive Statistics Analysis (even though I used ANOVA I'm gonna do each one manually)
# The mean number of turns for a black winner
winner_black <- chess %>% 
  filter(winner == "black") %>% 
  summarise(Average = sum(turns)/length(turns))
winner_black

# The mean number of turns for a white winner
winner_white <- chess %>% 
  filter(winner == "white") %>% 
  summarise(Average = sum(turns)/length(turns))
winner_white

# Compare the two by a logical expression
winner_black < winner_white


# Variance for black winner
Black_sd <- sqrt(winner_black)
Black_sd

# Variance for white winner
white_sd <- sqrt(winner_white)
white_sd




