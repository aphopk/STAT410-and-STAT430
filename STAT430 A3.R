# STAT430
# Assessment 2
#
# A Hopkins
# 23/04/2022

library(GGally)
library(ggplot2)
library(tidyverse)
library(broom)

library(ISLR)

# Question 2
# a)

# initial look at data
fix(Auto)
plot(mpg~horsepower, data = Auto)

attach(Auto)

# look at the significance of higher order polynomials (up to 10th)
mpg.lm10 <- lm(mpg~horsepower+poly(horsepower, 2)+poly(horsepower, 3)+poly(horsepower, 4)+poly(horsepower, 5)+poly(horsepower, 6)+poly(horsepower, 7)+poly(horsepower, 8)+poly(horsepower, 9)+poly(horsepower, 10), data = Auto)
anova(mpg.lm10)

# look at goodness of fit for second, fifth, and sixth order polynomial models
mpg.lm2 <- lm(mpg~horsepower + poly(horsepower, 2), data = Auto)
mpg.lm5 <- lm(mpg~horsepower+poly(horsepower, 2)+poly(horsepower, 3)+poly(horsepower, 4)+poly(horsepower, 5), data = Auto)
mpg.lm6 <- lm(mpg~horsepower+poly(horsepower, 2)+poly(horsepower, 3)+poly(horsepower, 4)+poly(horsepower, 5)+poly(horsepower, 6), data = Auto)

summary(mpg.lm2)
summary(mpg.lm5)
summary(mpg.lm6)

# checking the model assumptions
par(mfrow=c(2,2))
plot(mpg.lm2)

# using the tidyverse and broom libraries to get extra information about the model
mpg.lm2.data <- augment(mpg.lm2) %>% mutate(index = 1:n())
# selecting observations with a standardised residual > 3
mpg.lm2.data %>% filter(abs(.std.resid)>3)
# finding the maximum Cook's distance held by an observation in the model
max(cooks.distance(mpg.lm2))

geom_boxplot(outlier.colour="black", outlier.shape = 16,
             outlier.size=2, notch = false)

# check that horsepower = 150 is within the scope of the model
#  (i.e. that it is not extrapolation)
par(mfrow=c(1,1))
hist(horsepower)

# make prediction using the second order linear model fitted, and give 95% confidence
#  interval of the prediction
predict(mpg.lm2, newdata = data.frame(horsepower = 150), interval='prediction')

# b)
library(boot)

# set the seed as my UNE student number
set.seed(220087256)

# bootstrappedPrediction
#  takes the formula for a linear regression model with horsepower
#   as the independent variable and mpg as the dependent variable, 
#   fits the model on the subset of the data frame indicated by
#   indicies
#  returns the average mpg for horsepower = 150 predicted by the model
#   trained on the subset of data indicated in the argument, the
#   95% CI of the average mpg for horsepower = 150, as well as the
#   predicted mpg for horsepower = 150 and the 95% CI of the prediction
bootstrappedPrediction <- function(formula, data, indicies) {
    d <- data[indicies,]
    bootstrapped.lm <- lm(formula, data=d)
    
    newConfidence <- predict(bootstrapped.lm, newdata = data.frame(horsepower = 150), interval='confidence')
    newPredict <- predict(bootstrapped.lm, newdata = data.frame(horsepower = 150), interval='prediction')

    combinedStatistics <- cbind(newConfidence, newPredict)
    
    return (combinedStatistics)
}

bootfit2 <- boot(data=Auto, statistic = bootstrappedPrediction,
                 R = 100000, formula = mpg~horsepower+poly(horsepower, 2))

bootfit2


# Question 3
# a)
body.df <- read.csv("Body.csv")

# initial view of data and factorisation of relevant
#  variables
fix(body.df)
is.factor(body.df$Gender)
body.df$Gender <- as.factor(body.df$Gender)
attach(body.df)

str(body.df)

# graphical exploration of data
#  distribution of observations within each variable
boxplot(body.df[1:9])
boxplot(body.df[10:15])
boxplot(body.df[16:21])
boxplot(body.df[22]), xlab='Age'
boxplot(body.df[23], xlab='Weight')
boxplot(body.df[24], xlab='Height')
barplot(table(body.df$Gender), col = "#ADD8E6")

#  pairwise correlations
ggcorr(body.df, method = c("everything", "pearson")) 
ggpairs(body.df)

# b) 
#generate test and training sets
set.seed(220087256)
subsetofbody <- sort(sample(nrow(body.df), nrow(body.df)*.6))
train.df <- body.df[subsetofbody,]                      
test.df <- body.df[-subsetofbody,]

# define the model containing intercepts (the minimal model)
intercept_only <- lm(Weight ~ 1, data=train.df)

# define the model with all predictors (the maximal model)
all_predictors <- lm(Weight ~ ., data=train.df)

# perform backward stepwise regression
backward.lm <- step(all_predictors, direction='backward', scope=formula(all_predictors), trace=1)

# view results of the backward stepwise regression
backward.lm$anova

# view coefficients for the final model
backward.lm$coefficients

# summary table for model fitted using backward stepwise selection
summary(backward.lm)

ggcorr(body.df[, c('Chest_dep', 'Chest_diam', 'Wrist_diam', 'Knee_diam',
                   'Chest_g', 'Waist_g', 'Hip_g', 'Thigh_g', 'Forearm_g',
                   'Knee_g', 'Calf_g', 'Age', 'Height', 'Gender')], method = c("everything", "pearson"))

ggpairs(body.df[, c('Chest_dep', 'Chest_diam', 'Wrist_diam', 'Knee_diam',
                    'Chest_g', 'Waist_g', 'Hip_g', 'Thigh_g', 'Forearm_g',
                    'Knee_g', 'Calf_g', 'Age', 'Height', 'Gender')])

# checking linear regression model assumptions with
#  residuals plots
par(mfrow=c(2,2))
plot(backward.lm)

# using the tidyverse and broom libraries to get extra information about the model
backward.lm.data <- augment(mpg.lm2) %>% mutate(index = 1:n())
# selecting observations with a standardised residual > 3
backward.lm.data %>% filter(abs(.std.resid)>3)
# finding the maximum Cook's distance held by an observation in the model
max(cooks.distance(backward.lm))


# exploring multicollinearity
vif(backward.lm)

# run the model given by backwards selection on the test data
backward.lm.pred <- predict(backward.lm, test.df)
# calculate the mean square error of the model on the 
#  test data by comparing the predictions from the model
# to the actual values of the response variable
mean((backward.lm.pred-test.df[,c("Weight")])^2)

# for comparison:
# calculate the test MSE for the model containing all
#  of the predictors, using the same process as above
all_predictors.pred <- predict(all_predictors, test.df)
mean((all_predictors.pred-test.df[,c("Weight")])^2)

# c)
library(glmnet)

# converting from dataframe object type to matrix, so that
#  object is compatible with glmnet functions

# training data
#  creating a matrix for the predictors
predictorTrain <- model.matrix(Weight~., train.df)
#  creating a single column matrix for the response variable
responseTrain <- subset(train.df, select=c(Weight))
responseTrain <- data.matrix(responseTrain)

# test data
#  creating a matrix for the predictors
predictorTest <- model.matrix(Weight~., test.df)
#  creating a single column matrix for the response variable
responseTest <- subset(test.df, select=c(Weight))
responseTest <- data.matrix(responseTest)


# run lasso, allowing glmnet to choose range of lambdas to try
lasso.mod <- glmnet(x=predictorTrain, y=responseTrain, alpha=1)
plot(lasso.mod)

# set the seed as my UNE student number
set.seed(220087256)

# use cross validation on the training dataset to 
#  find values for lambda
cv.out <- cv.glmnet(x=model, y=responseTrain, alpha=1)
# plot the models' MSE against log(Lambda)
plot(cv.out)
# find the lambda that gave the minimum cross-validated error
bestlam <- cv.out$lambda.min
bestlam

# output the coefficients after Lasso has been applied
lasso.coef = predict(cv.out, type="coefficients", s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]

# run the model selected by Lasso on the test data
lasso.pred = predict(lasso.mod, s=bestlam, newx=predictorTest)
# calculate the MSE of this model against the test data
mean((lasso.pred-responseTest)^2)
