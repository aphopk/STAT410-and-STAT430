# STAT410
# Assessment 2
# Adam Hopkins
# student number: 220087256

setwd("~/~ Uni/~Master of Data Science/STAT410/Assignments/A2")

library("ggplot2")
library("GGally")
library("car")
library("broom")
library("tidyverse")

cereal.df <- read.csv("cereal.csv")

# initial visual exploration of data
fix(cereal.df)

# QUESTION 1
ggpairs(cereal.df)

# QUESTION 2
# fit the multiple linear regression model
cereal.lm <- lm(Energy~., data = cereal.df)

# explore the overall utility of the model, contribution
#  of individual predictors to the model, and the relationships
#  suggested between each predictor and the resopnse variable
summary(cereal.lm)
confint(cereal.lm)


# checking the model assumptions with exploratory plots
ggpairs(cereal.df)
vif(cereal.lm)
par(mfrow=c(2,2))
plot(cereal.lm)

# using the tidyverse and broom libraries to get extra information about the model
cereal.lm.data <- augment(cereal.lm) %>% mutate(index = 1:n())
# selecting observations with a standardised residual > 3
cereal.lm.data %>% filter(abs(.std.resid)>3)
# finding the maximum Cook's distance held by an observation in the model
max(cooks.distance(cereal.lm))


# refitting the model with the significant terms
cereal.lm2 <- lm(Energy~Fat+Carbs, data = cereal.df)
summary(cereal.lm2)
#finding the 95% confidence intervals for the regression coefficients in the final model
confint(cereal.lm2)

#diagnostic plots
par(mfrow=c(1,1))
# pairwise correlations between variables, and visual representation
#  of relationship between predictors and response variable
ggpairs(cereal.lm2)
par(mfrow=c(2,2))
# residuals
plot(cereal.lm2)
plot(cereal.lm2, which = 4:5)

# using the tidyverse and broom libraries to get extra information about the model
cereal.lm2.data <- augment(cereal.lm2) %>% mutate(index = 1:n())
# selecting observations with a standardised residual > 3
cereal.lm2.data %>% filter(abs(.std.resid)>3)
# finding the maximum Cook's distance held by an observation in the model
max(cooks.distance(cereal.lm))

vif(cereal.lm)

#making predictions
newobs1 <- c(0.5, 15)
newobs2 <- c(1, 18)
new.df <- data.frame(newobs1, newobs2)
colnames(new.df) <- c('Fat', 'Carbs')
predict(cereal.lm2, newdata = new.df, interval='prediction')
predict(cereal.lm2, newdata = new.df, interval='confidence')

