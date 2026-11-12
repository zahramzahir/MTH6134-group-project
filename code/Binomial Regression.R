
# WEEK 4 - BINOMIAL REGRESSION

# Examine the players dataset
head(league_players)

# Check the types of variables
str(league_players)

# Summary statistics
summary(league_players)


# Count injured and non-injured players
table(league_players$injured)

# Calculate the proportions
prop.table(table(league_players$injured))

# Create a bar chart
barplot(table(league_players$injured),
        main = "Number of Injured and Non-Injured Players",
        xlab = "Injury Status (0 = No, 1 = Yes)",
        ylab = "Number of Players",
        col = c("lightblue", "salmon"))


# Convert playing position to a factor
league_players$position <- factor(league_players$position)

# Set defenders as the reference category
league_players$position <- relevel(
  league_players$position,
  ref = "DEF"
)

# Check the categories
table(league_players$position)


# Fit a binomial logistic regression model
model1 <- glm(injured ~ age + training_load +
                prev_injuries + position,
              data = league_players,
              family = binomial

# Display the regression results
summary(model1)


# Calculate odds ratios
exp(coef(model1))

# Calculate 95% confidence intervals
exp(confint.default(model1))



# Fit a simpler model without playing position
model2 <- glm(injured ~ age + training_load +
                prev_injuries,
              data = league_players,
              family = binomial(link = "logit"))

# Compare models using AIC
AIC(model1, model2)

# Likelihood ratio test
anova(model2, model1, test = "Chisq")



# Create hypothetical players
new_players <- data.frame(
  age = c(25, 25, 25, 25, 25),
  training_load = c(1, 2, 3, 4, 5),
  prev_injuries = c(1, 1, 1, 1, 1)
)

# Predict injury probabilities using model2
new_players$probability <- predict(
  model2,
  newdata = new_players,
  type = "response"
)

# View predictions
new_players

# Plot predicted probabilities
plot(new_players$training_load,
     new_players$probability,
     type = "b",
     pch = 19,
     col = "blue",
     ylim = c(0, 1),
     xlab = "Training Load",
     ylab = "Predicted Probability of Injury",
     main = "Training Load and Injury Probability")
