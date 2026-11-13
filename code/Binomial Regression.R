
# WEEK 4 - BINOMIAL REGRESSION

# Import the players dataset
league_players <- read.csv("league_players.csv")

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
              family = binomial)

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



# MODEL DIAGNOSTICS FOR MODEL 2
# (model2 is the one we kept: lower AIC and position wasn't significant)

# Number of observations and parameters
n <- nobs(model2)
p <- length(coef(model2))

# Standardised (deviance) residuals
std_res <- rstandard(model2)

# Pearson residuals
pearson_res <- residuals(model2, type = "pearson")

# Leverage values
lev <- hatvalues(model2)

# Cook's distance
cd <- cooks.distance(model2)

# Quick summaries
summary(std_res)
summary(pearson_res)
summary(lev)
summary(cd)


# Put the four plots on one page
par(mfrow = c(2, 2))

# Standardised residuals against fitted values
plot(fitted(model2), std_res,
     main = "Standardised residuals vs fitted values",
     xlab = "Fitted probability of injury",
     ylab = "Standardised residuals")
abline(h = 0, lwd = 2)
abline(h = c(-2, 2), lty = 2, col = "grey")

# Pearson residuals against fitted values
plot(fitted(model2), pearson_res,
     main = "Pearson residuals vs fitted values",
     xlab = "Fitted probability of injury",
     ylab = "Pearson residuals")
abline(h = 0, lwd = 2)
abline(h = c(-2, 2), lty = 2, col = "grey")

# Leverage values, with the 2p/n rule of thumb
plot(lev, type = "h",
     main = "Leverage values",
     xlab = "Observation number",
     ylab = "Leverage")
abline(h = 2*p/n, lwd = 2, col = "red")

# Cook's distance, same threshold as the linear regression file
plot(cd, type = "h",
     main = "Cook's distance",
     xlab = "Observation number",
     ylab = "Cook's distance")
abline(h = 8/(n - 2*p), lwd = 2, col = "red")

par(mfrow = c(1, 1))

# Residuals come in two bands because the response is 0/1,
# so the patterns are expected and not a sign of a bad fit

# Players with large standardised residuals
which(abs(std_res) > 2)

# Players with high leverage
which(lev > 2*p/n)

# Influential observations
which(cd > 8/(n - 2*p))
league_players[which(cd > 8/(n - 2*p)), ]
# Largest Cook's distance is about 0.025, so no single player
# is changing the model much
