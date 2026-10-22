# Import datasets
matches <- read.csv("ashford_matches.csv")
players <- read.csv("league_players.csv")

# Display the first 6 rows
head(matches)
head(players)

# Explore structure of data frame
str(matches)
str(players)

# Count missing values
sum(is.na(matches))
sum(is.na(players))

# Count duplicates
sum(duplicated(matches))
sum(duplicated(players))

# Summary statistics for each column
summary(matches)
summary(players)

#  Standard deviation for numeric columns
sapply(matches[sapply(matches, is.numeric)], sd, na.rm = TRUE)
sapply(players[sapply(players, is.numeric)], sd, na.rm = TRUE)



# Classification of variables by type: binary, discrete, continuous, nominal, ordinal


# Binary: home_away, injured
# Ensure there are only 2 distinct values

unique(matches$home_away)
unique(players$injured)

# Discrete: goals_scored, goals_conceded, prev_injuries
# Check how R stores the data: these are all integers which represent count data

str(matches$goals_scored)
str(matches$goals_conceded)
str(players$prev_injuries)

# Continuous: age, minutes_played, bmi
# Check how R stores the data: we naturally treat age, time as continuous 

str(players$age)
str(players$minutes_played)
str(players$bmi)

# Nominal: team, position
# Check how R stores the data: these are stored as text hence categorical

str(players$team)
str(players$position)

# Ordinal: opponent_rank, training_load
# Check how R stores the data: these are stored as numbers which means they are ranked

str(matches$opponent_rank)
str(players$training_load)




# Plot histograms to explore the distributions of age, minutes played and BMI
hist(players$age,
     main = "Distribution of age",
     xlab = "Age",
     ylab = "Frequency")

hist(players$minutes_played, 
     main = "Distribution of minutes played",
     xlab = "Minutes played",
     ylab = "Frequency")

hist(players$bmi,
     main = "Distribution of BMI",
     xlab = "BMI",
     ylab = "Frequency")


# Plot bar plots to display the frequency of goals scored, injuries, position and training load
barplot(table(matches$goals_scored),
        main = "Distribution of goals scored",
        xlab = "Goals scored",
        ylab = "Number of matches")

barplot(table(players$injured),
        main = "Distribution of injuries",
        xlab = "Injury status",
        ylab = "Number of players")

barplot(table(players$position),
        main = "Distribution of player positions",
        xlab = "Position",
        ylab = "Number of players")

barplot(table(players$training_load),
        main = "Distribution of training load",
        xlab = "Traning load",
        ylab = "Number of players")


# Plot box plots for comparison of variables
boxplot(minutes_played ~ position, data = players,
        main = "Minutes played by player position",
        xlab = "Position",
        ylab = "Minutes played")

boxplot(minutes_played ~ injured, data = players,
        main = "Minutes played by injury status",
        xlab = "Injury status",
        yalb = "Minutes played")

boxplot(minutes_played ~ training_load, data = players,
        main = "Minutes played by training load",
        xlab = "Training load", 
        ylab = "Minutes played")

boxplot(goals_scored ~ home_away, data = matches,
        main = "Goals scored by match location",
        xlab = "Match location",
        ylab = "Goals scored")

# Plot scatter plots to explore relationships between variables
plot(minutes_played ~ age, data = players, 
     main = "Minutes played vs age",
     xlab = "Age",
     ylab = "Minutes played")

plot(minutes_played ~ bmi, data = players,
     main = "Minutes played vs BMI",
     xlab = "BMI",
     ylab = "Minutes played")

plot(goals_scored ~ opponent_rank, data = matches,
     main = "Goals scored vs opponent rank", 
     xlab = "Opponent rank",
     ylab = "Goals scored")
