# Chess 
# Aljohara Almedlej
# 09.10.2020

# Data from: https://www.kaggle.com/datasnaek/chess

# Load the library
library(tidyverse)
library(lubridate)

# Read the data
chess <- read_csv("data/games.csv")
chess

# What is the research question that we can answer with this data

convTime <- function(x) {
  lubridate::as_datetime(as.numeric(substr(x,1,nchar(x)-3)))
}


chess <- chess %>% 
  mutate(created_at = convTime(created_at),
         last_move_at = convTime(last_move_at),
         time_diff = last_move_at - created_at) %>% 
  arrange(-time_diff)
glimpse(chess)

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
  geom_bar()
# Conclusion: White win more


# Another polt
# Is the number of turns associated with the victory status? 
ggplot(chess, aes( turns, victory_status)) +
  geom_col()
# Conclusion: people who resign have the highest number of turns


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

# what is the data type of last move column?
typeof(chess$last_move_at) # double, but the data is still not clear whether it is the position of the last move of the game or the exact time



# What is the most common white_ID?
whiteID_common <- names(table(chess$white_id))[as.vector(table(chess$white_id))==max(table(chess$white_id))]  
whiteID_common


# What is the most common black_ID?
blackID_common <- names(table(chess$black_id))[as.vector(table(chess$black_id))==max(table(chess$black_id))]  
blackID_common


# the user taranga plays a lot....

# Is the first move associated with winning?
typeof(chess$opening_eco)
common_openingEco <- names(table(chess$opening_eco))[as.vector(table(chess$opening_eco))==max(table(chess$opening_eco))] 
common_openingEco == (chess$victory_status == "mate") # the most common first move is not associated with the win




