# Import datasets 
matches <- read.csv("ashford_matches.csv")
players <- read.csv("league_players.csv")

# Convert binary and ordinal data into factors 
players$injured <- factor(players$injured, ordered = TRUE)

# Fit the model for a continuous variable 
model_fit <- lm(minutes_played ~ bmi + age + training_load + position + injured,
                data = players)
summary(model_fit)

# Diagnostic plots to assess linearity and constant variance 
plot(model_fit$fitted.values, rstudent(model_fit),
     main = "Plot of studentised residuals against fitted values",
     xlab = "Fitted values",
     ylab = "Studentised residuals")

abline(h = 0, lwd = 2)
# Assumptions of linearity and homoscedasticity hold 

# Q-Q plot to assess normality
qqnorm(rstudent(model_fit))
qqline(rstudent(model_fit), lwd = 2, col = "grey")
# Assumption of normality holds

# Cook's distance plot
plot(model_fit, 4)

# Cook's distance threshold
n <- nobs(model_fit)
p <- length(coef(model_fit))

abline(h = 8/(n - 2*p), lwd = 2)

# Influential observations 
cd <- cooks.distance(model_fit)
which(cd > 8/(n - 2*p))