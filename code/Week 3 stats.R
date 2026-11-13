# ===== DECISIONS RECORD: injury model =====
# Response: injured (1 = injured, 0 = not). It can only be yes/no.
# Model: logistic regression (binomial), because injured can only be 0 or 1
#   and a straight-line model could predict values outside 0 to 1.
# Why injury and not goals: the group left the choice open. Injury has
#   620 players (goals has only 46 matches) and several explanatory variables.
# Variables: age, position, training_load, prev_injuries, bmi, minutes_played.
#   I expect age and previous injuries to matter, e.g. players with more
#   previous injuries may be more likely to be injured again (to be tested).
# minutes_played: its coefficient was about 0 (p = 0.96), so it adds nothing
#   here. Injured players miss games, so it may be a result of injury rather
#   than a cause. I kept it in this first model and will test whether it is
#   needed before the final model.
# Diagnostics: done by my partner.
# ============================================

# ---- Read data ----
# This line opens the file league_players and stores it as players
players <- read.csv("league_players.csv")

# ---- Set categorical variables as factors ----
# position is a label (not a number), so factor() makes R treat it as a
# category and compare each position to the baseline (DEF)
players$position <- factor(players$position)

# ---- Check the data loaded correctly ----
# dim() gives the number of rows then columns (620 players, 8 columns)
dim(players)
# str() shows every column with its type
str(players)

# ---- Injury model ----
# I am predicting injured from age, position, training load, previous
# injuries, BMI and minutes played
# family = binomial because injured can only be yes/no (1 or 0)
# The default link is logit, so coefficients are changes in log-odds
injury_model <- glm(
  injured ~ age + position + training_load +
    prev_injuries + bmi + minutes_played,
  data = players,
  family = binomial
)

# Coefficient table: estimates, standard errors, z values, p-values
summary(injury_model)