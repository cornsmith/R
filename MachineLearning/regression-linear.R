require(MASS)

# Dataset -----------------------------------------------------------------
Boston
str(Boston)
plot(Boston)


# Model -------------------------------------------------------------------
# fit the model
lm_fit <- lm(medv ~ ., data=Boston)

# model summary
summary(lm_fit)

# access components of model
names(lm_fit)
coef(lm_fit)

# access components of model summary
names(summary(lm_fit))

# model plots
plot(lm_fit)


# Stepwise selection ------------------------------------------------------
# do stepwise
step <- MASS::stepAIC(lm_fit, direction="both")

# display results
step$anova

# fit final
lm_fit <- eval(step$call)
summary(lm_fit)


# Interactions and transformations ----------------------------------------
# lstat + age + lstat:age
lm(medv ~ lstat + age + lstat:age, data=Boston)
lm(medv ~ lstat * age, data=Boston) # equivalent

# lstat + lstat^2
lm(medv ~ lstat + I(lstat ^ 2), data=Boston)

# log(rm)
lm(medv ~ log(rm), data=Boston)