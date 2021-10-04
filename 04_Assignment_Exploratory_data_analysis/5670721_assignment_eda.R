# Load useful libraries
library(ISLR)
library(tidyverse)
library(haven)
library(readxl)
library(SmartEDA)
library(network)

# Load data
transfers <- read.csv('data/transfers.csv', sep=';')
dict_leagues <- read.csv('data/dict_leagues.csv', sep=';')
dict_clubs <- read.csv('data/dict_clubs.csv', sep=';')
clubs_in_leagues <- read.csv('data/clubs_in_leagues.csv', sep=';')


# What is this data set?

# These data sets pertain to all recorded instances of football/soccer related data on players. Therefore, this data is comprised
# of, but not limited to, clubs in leagues, players in league, player transfers, player stats, positional stats of players, stats
# for players in national teams, and dictionaries of clubs, players, leagues, teams, and stadiums.
# 
# Since the total data set is very large, I had to pick a topic of what story I wanted my data to tell.
# Therefore, I decided to explore the data set with the topic of transfers between clubs in mind. 
# 
# So, the data that I used from the whole data set are:
#   * transfers.csv
#   * dict_leagues.csv
#   * dict_clubs.csv
#   * clubs_in_leagues.csv

# What did I do with the data?

# Since I did not collect this data myself, I needed to get a feel for the data and see how the data was structured. First, I 
# selected the appropriate data (see above) and then I went on to perform global EDA for each data set by viewing, summarizing, 
# and applying functions from the SmartEDA library. When I got a feel for the data, I needed to transform it into a shape I could
# get valuable insights from it. This was the case because there was not data set with just the transfers between clubs.
# 
# From what I gathered of the EDA, there were not many transfers that could be analyzed outside of the English competition. 
# Initially, I wanted to analyze the Dutch competition out of sentiment, but there was not enough data on this. So, I constructed
# the English transfer data set by transforming and aggregating data sets. When this was done, once again I performed EDA on this
# new data set to explore it well before making any changes or getting down to the analysis part of this assignment. As an avid
# football/soccer fan, I formulated questions that I wanted to ask the data set I created. "What are the most expensive footballers?",
# "Which clubs sell or buy the most?", "How many transfers are there each year?", "Can you see the difference between clubs that sell and buy?",
# et cetera. With these questions in mind, I approached the data.
# 
# However, to make this data relevant for the reader, I decided to only focus on Premier League clubs that are currently playing.
# Furthermore, data was clogged up by teams that have since been performing badly, so I did not want annoying data or noise.

# Perform global checks on the data sets
View(transfers)
summary(transfers)
dim(transfers)
head(transfers)
anyNA(transfers) # TRUE
ExpData(transfers, type=1) # Check overview of data
ExpData(transfers, type=2) # Check structure of data

View(dict_leagues)
summary(dict_leagues)
dim(dict_leagues)
head(dict_leagues)
anyNA(dict_leagues) # TRUE
ExpData(dict_leagues, type=1) # Check overview of data
ExpData(dict_leagues, type=2) # Check structure of data

View(dict_clubs)
summary(dict_clubs)
dim(dict_clubs)
head(dict_clubs)
anyNA(dict_clubs) # TRUE
ExpData(dict_clubs, type=1) # Check overview of data
ExpData(dict_clubs, type=2) # Check structure of data

View(clubs_in_leagues)
summary(clubs_in_leagues)
dim(clubs_in_leagues)
head(clubs_in_leagues)
anyNA(clubs_in_leagues) # TRUE
ExpData(clubs_in_leagues, type=1) # Check overview of data
ExpData(clubs_in_leagues, type=2) # Check structure of data


# When looking at the data, EDA has revealed that NA values don't mean that much. Sometimes coaches and types of
# transfers were recorded in the case of famous clubs/players. Sometimes fees are not reported, which can be an issue.

# In particular, I am interested in transfers (players going from one club to another for a fee)
# In the Premier League, the league in England as it is regarded as one of the best.
# I will now create a data frame which contains these objects as it does not yet exist.

epl <- dplyr::filter(clubs_in_leagues, league_id %in% 1)
epl_club_ids <- epl$club_id


# This contains all the clubs that were active in the Premier League (since ~2005), but I am primarily
# interested in the current playing clubs. So let's subset those.
epl_current_club_ids <- c(265, 269, 307, 301, 268, 294, 276, 288, 266, 271, 267, 286, 264, 274, 298, 278, 273, 291, 272, 293)
epl_current_clubs <- c("Arsenal", "Aston Villa", "Brighton", "Burnley", "Chelsea", "Crystal Palace", "Everton", "Fulham", "Leeds", "Leicester", "Liverpool", "Man City", "Man Utd", "Newcastle", "Sheffield Utd.", "Southampton", "Spurs", "West Bromwich Albion", "West Ham", "Wolves")


epl_in <- dplyr::filter(transfers, to_club_id %in% epl_current_club_ids)
epl_out <- dplyr::filter(transfers, from_club_id %in% epl_current_club_ids)
epl_transfers <- rbind(epl_in, epl_out)

epl_transfers$date <- as.Date(epl_transfers$date)
epl_in$date <- as.Date(epl_in$date)
epl_out$date <- as.Date(epl_out$date)

epl_transfers$date <- format(epl_transfers$date,"%Y")
epl_in$date <- format(epl_in$date,"%Y")
epl_out$date <- format(epl_out$date,"%Y")

epl_transfers[is.na(x = epl_transfers)] <- 0
epl_in[is.na(x = epl_in)] <- 0
epl_out[is.na(x = epl_out)] <- 0

epl <- epl %>%
  group_by(club_name, season) %>%
  filter(season >= 2005, season != 2019, club_id %in% epl_current_club_ids)

epl_transfers <- epl_transfers %>%
  group_by(to_club_name, date) %>%
  filter(fee > 0, date >= 2005, date != 2019, to_club_id %in% epl_current_club_ids | from_club_id %in% epl_current_club_ids)

epl_in <- epl_in %>%
  group_by(to_club_name, date) %>%
  filter(fee > 0, date >= 2005, date != 2019, to_club_id %in% epl_current_club_ids)

epl_out <- epl_out %>%
  group_by(to_club_name, date) %>%
  filter(fee > 0, date >= 2005, date != 2019, from_club_id %in% epl_current_club_ids)

# Let's perform some additional, global analysis as this is a new dataset.
View(epl_transfers)
summary(epl_transfers)
dim(epl_transfers)
head(epl_transfers)
anyNA(epl_transfers) # FALSE
ExpData(epl_transfers, type=1) # Check overview of data
ExpData(epl_transfers, type=2) # Check structure of data


# Now, let's look at visualizations and interesting parts of the data

epl_in_summary <- epl_in %>%
  group_by(to_club_name, date) %>%
  filter(fee > 0, date >= 2005, date != 2019) %>%
  summarise(
    mean = mean(fee),
    variance = var(fee),
    min = min(fee),
    max = max(fee)
  )

epl_out_summary <- epl_out %>%
  group_by(from_club_name, date) %>%
  filter(fee > 0, date >= 2005, date != 2019) %>%
  summarise(
    mean = mean(fee),
    variance = var(fee),
    min = min(fee),
    max = max(fee)
  )


# Transfers (incl. Loans) recorded per year by current playing clubs in 2020/21 season

# Transfers IN
p1 <- ggplot(epl_in, aes(x = as.factor(date))) +
  geom_bar(aes(fill=to_club_name)) +
  xlab("Year") +
  ylab("Amount of transfers in by clubs") +
  ggtitle("Amount of transfers in by Premier League clubs currently playing in the 2020/21 season")

p1 + theme_light()

# Transfers OUT
p2 <- ggplot(epl_out, aes(x = as.factor(date))) +
  geom_bar(aes(fill=from_club_name)) +
  xlab("Year") +
  ylab("Amount of transfers out by clubs") +
  ggtitle("Amount of transfers out by Premier League clubs currently playing in the 2020/21 season")

p2 + theme_light()

# All Transfers
p3 <- ggplot(epl_transfers, aes(x = as.factor(date))) +
  geom_bar() +
  xlab("Year") +
  ylab("Total amount of transfers by clubs per year") +
  ggtitle("Total amount of transfers by Premier League clubs currently playing in the 2020/21 season")

p3 + theme_light()


# Most expensive player brought into a Premier League Club
max_fee <- max(epl_in$fee)
index_fee_max <- which(epl_in$fee == max_fee)
most_expensive_player_in <- epl_in[index_fee_max,]
most_expensive_player_in

# Most expensive player sold from Premier League Club
max_fee_out <- max(epl_out$fee)
index_fee_max_out <- which(epl_out$fee == max_fee_out)
most_expensive_player_out <- epl_out[index_fee_max_out,]
most_expensive_player_out

# Players who have been sold/loaned the most times
out_counts <- as.data.frame(table(epl_transfers$player_name))
max_10_counts <- out_counts[with(out_counts, order(-Freq)),]
max_10_counts[1:10,]


# Biggest spenders since 2005

biggest_spenders <- epl_in %>%
  group_by(to_club_name) %>%
  filter(fee > 0, date >= 2005, date != 2019) %>%
  summarise(
    sum = sum(fee)
  )

p4 <- ggplot(biggest_spenders, aes(x = to_club_name, y = sum)) +
  geom_col() +
  xlab("Club") +
  ylab("Total amount of money spent on transfers") +
  ggtitle("Biggest transfer spenders by Premier League clubs current playing in the 2020/21 season")

p4 + theme_light() + coord_flip()


# Biggest sellers since 2005

biggest_sellers <- epl_out %>%
  group_by(from_club_name) %>%
  filter(fee > 0, date >= 2005, date != 2019) %>%
  summarise(
    sum = sum(fee)
  )

p5 <- ggplot(biggest_sellers, aes(x = from_club_name, y = sum)) +
  geom_col() +
  xlab("Club") +
  ylab("Total amount of money gained on transfers") +
  ggtitle("Biggest player selling Premier League clubs current playing in the 2020/21 season")

p5 + theme_light() + coord_flip()

