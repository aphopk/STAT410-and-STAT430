# STAT430 
# Assignment 1
# Adam Hopkins
#
#  March 2022

# Q2

setwd("~/~ Uni/~Master of Data Science/STAT430/Assignments/A1")

library("ggplot2")
library("GGally")


# QUESTION 2
canola.df <- read.csv("canolaseed.csv")

# a)

#intial look at data
names(canola.df)
fix(canola.df)

#convert Site and Variety to categorical variables
canola.df$Site <- as.factor(canola.df$Site)
canola.df$Variety <- as.factor(canola.df$Variety)

#check conversion carried to dataframe
is.factor(canola.df$Site)
is.factor(canola.df$Variety)
#check levels that factor variables divided into
levels(canola.df$Site)
levels(canola.df$Variety)

summary(canola.df[,3:8])

# plot response variable and predictor variables to
# to visually assess for relationships and check
# correlation coefficients
ggpairs(canola.df[,3:8])

# create multiple linear regression model
canola.lm <- lm(SM~., data=canola.df)
# view summary table
summary(canola.lm)


# Q2 b)
# exploratory plots
par(mfrow=c(2,2))
plot(canola.lm)

# Q2 c)
#calculate the variance inflation factors to assess for 
#  multicollinearity
library(car)
vif(canola.lm)

#look at maximum Cook's distance
max(cooks.distance(canola.lm))

#a new linear regression model without RVI as a predictor
canola.lm2 <- lm(SM~Site+Variety+NDVI+Min_Temp+Max_Temp+Rainfall, data=canola.df)
vif(canola.lm2)
summary(canola.lm2)

# exploratory plots
par(mfrow=c(2,2))
plot(canola.lm2)

max(cooks.distance(canola.lm2))


# STAT430 
# Assignment 1
# Adam Hopkins
#
#  March 2022

# Q3 a), b), c)

setwd("~/~ Uni/~Master of Data Science/STAT430/Assignments/A1")

library("ggplot2")
library("GGally")
library("car")

library("tidyverse") #for easy data manipulation and visualization
library("broom") #creates a tidy data frame from statistical test results


# QUESTION 3
raisin.df <- read.csv("Raisin.csv")

#exploratory plots and analysis
fix(raisin.df)
summary(raisin.df)
raisin.df$Class <- as.factor(raisin.df$Class)
summary(raisin.df)
ggpairs(raisin.df)

#generate test and training sets
set.seed(220087256)
subsetofraisin <- sort(sample(nrow(raisin.df), nrow(raisin.df)*.7))
train <- raisin.df[subsetofraisin,]                      
test <- raisin.df[-subsetofraisin,]

#create multiple logistic regression model
raisins.glm <- glm(Class~., data = train, family = binomial(link='logit'))



#reviewing which predictors are significant in the model, 
# and how they affect the likelihood of an observation falling
# into either of the response classes
summary(raisins.glm)
anova(raisins.glm, test="Chisq")

# EVALUATING THE MODEL'S PERFORMANCE AGAINST
#   THE TRAINING OBSERVATION SET
# following the same steps as below to calculate the accuracy (proportion of 
# correct predictions) for the training dataset
glm.probs = predict(raisins.glm, newdata = train, type = "response")
glm.pred=rep("Besni", nrow(train))
glm.pred[glm.probs>.5]="Kecimen"
trainconfmatrix <- table(glm.pred, train$Class)
mean(glm.pred == train$Class)
rglm.train.sensitivity = trainconfmatrix[1,1] / (trainconfmatrix[1,1]+trainconfmatrix[2,1])
rglm.train.specificity = trainconfmatrix[2,2] / (trainconfmatrix[2,2]+trainconfmatrix[1,2])
rglm.train.PPV = trainconfmatrix[1,1] / (trainconfmatrix[1,1]+trainconfmatrix[1,2])
rglm.train.NPV = trainconfmatrix[2,2] / (trainconfmatrix[2,2] + trainconfmatrix[2,1])

# EVALUATING THE MODEL'S PERFORMANCE AGAINST
#   THE TEST OBSERVATION SET
# predict the probability (p) of belonging to Besni or Kecimen in the test data
#  produces a vector with the probability of an observation from the
#  test subset lying in the Kecimen class, based on the logistic
#  regression model raisins.glm just fit
glm.probs <- predict(raisins.glm, newdata = test, type = "response")

# create a vector with the class that each of the observations in the
#  test dataset is predicted to fall into, based on the raisins.glm 
#  logistic regression model
#prepopulate an array with the baseline class
glm.pred=rep("Besni", nrow(test)) 
#where probability of an observation in the test set > 0.5, set the class
# of the observation in our vector to "Kecimen"
glm.pred[glm.probs>.5]="Kecimen" 
#confusion matrix showing which observations were correctly, and which were
# incorrectly predicted in the test data
testconfmatrix <- table(glm.pred, test$Class)
# computes the fraction of observations for which the prediction was correct
mean(glm.pred == test$Class)

#calculating model sensitivity and specificity
rglm.test.sensitivity = testconfmatrix[1,1] / (testconfmatrix[1,1]+testconfmatrix[2,1])
rglm.test.specificity = testconfmatrix[2,2] / (testconfmatrix[2,2]+test[1,2])
rglm.test.PPV = testconfmatrix[1,1] / (testconfmatrix[1,1]+testconfmatrix[1,2])
rglm.test.NPV = testconfmatrix[2,2] / (testconfmatrix[2,2] + testconfmatrix[2,1])

# TESTING THE MODEL ASSUMPTIONS
# --- 1) binary outcome type --- 
#checks that it is a factor
is.factor(raisin.df$Class) 
#gives the names of the levels in the quantitative variable (and, by proxy, the number of levels)
levels(raisin.df$Class) 
#gives which level will be used as the baseline
contrasts(train$Class)

# --- 2) independence of observations
#Rule of Thumb: To check independence, plot residuals against any time variables present (e.g., order of observation), any spatial variables present, and any variables used in the technique (e.g., factors, regressors). A pattern that is not random suggests lack of independence.
fix(raisin.df)
# there are no time, spatial or other variables
#  specific to the data collection method that may dictate
#  lack of independence of observations --> so we assume
#  that they are independent

# --- 3) the assumption of linearity --- 
# that any independent continuous variables are linearly correlated to the log odds of the dependent variable --- 
#nb, if the variable contained qualitative and quantitative
# predictors we would need to include only the quantitative
# predictors for this test
# to test against the training data, we again re-create the
#  vector with the predicted probabilities of belonging to 
#  the Kecimen (i.e. the level 2, as opposed to level 1)
#  Class
glm.probs <- predict(raisins.glm, newdata = train, type = "response")

#create the logit of the probabilities
logit <- log(glm.probs/(1-glm.probs))

#plot the logit of our continuous variables
ggplot(train, aes(logit, Area))+
  geom_point(size=0.5, alpha=0.5) +
  geom_smooth(method = "loess") +
  theme_bw()

ggplot(train, aes(logit, MajorAxisLength))+
  geom_point(size=0.5, alpha=0.5) +
  geom_smooth(method = "loess") +
  theme_bw()

ggplot(train, aes(logit, MinorAxisLength))+
  geom_point(size=0.5, alpha=0.5) +
  geom_smooth(method = "loess") +
  theme_bw()

ggplot(train, aes(logit, Eccentricity))+
  geom_point(size=0.5, alpha=0.5) +
  geom_smooth(method = "loess") +
  theme_bw()


ggplot(train, aes(logit, ConvexArea))+
  geom_point(size=0.5, alpha=0.5) +
  geom_smooth(method = "loess") +
  theme_bw()

ggplot(train, aes(logit, Extent))+
  geom_point(size=0.5, alpha=0.5) +
  geom_smooth(method = "loess") +
  theme_bw()

ggplot(train, aes(logit, Perimeter))+
  geom_point(size=0.5, alpha=0.5) +
  geom_smooth(method = "loess") +
  theme_bw()

# --- 4) no strongly influential outliers ---

#looking for outliers
#extract model results
raisins.glm.data <- augment(raisins.glm) %>% mutate(index = 1:n())
#plot observations against standardised residuals
ggplot(raisins.glm.data, aes(index, .std.resid))+
  geom_point(aes(color=Class), alpha=.5)+
  theme_bw()
# filter for potentially influential outliers
raisins.glm.data %>% filter(abs(.std.resid)>3)


#looking for high leverage points
par(mfrow=c(1,2))
# plotting observations vs Cook's distance
#  and observation's residuals vs leverage  
plot(raisins.glm, which = 4:5) 
# getting the maximum Cook's distance in the model
max(cooks.distance(raisins.glm))



# --- 5) absence of multicollinearity ---
# getting VIF scores for the predictors
vif(raisins.glm)


# --- 6) sufficiently large sample size ---
#there should be > 10 observations with the least frequent
# for each independent variable

# output the number of observations in each outcome. 
summary(train$Class)
# get the number of predictors x 10
10*(length(names(train))-1)


#the total number of observations should be > 500
# give the total number of rows in the training dataset
nrow(train)



STAT430 
# Assignment 1
# Adam Hopkins
#
#  March 2022

# Q3 d)

setwd("~/~ Uni/~Master of Data Science/STAT430/Assignments/A1")

library("ggplot2")
library("GGally")
library("car")

library("class")

raisin.df <- read.csv("Raisin.csv")
raisin.df$Class <- as.factor(raisin.df$Class)

#generate test and training sets
set.seed(220087256)
subsetofraisin <- sort(sample(nrow(raisin.df), nrow(raisin.df)*.7))
train <- raisin.df[subsetofraisin,]                      
test <- raisin.df[-subsetofraisin,]

names(train)

# create vectors with:
#  predictors from training observation set
train.X <- train[,c('Area', 'MajorAxisLength', 'MinorAxisLength', 'Eccentricity', 'ConvexArea', 'Extent', 'Perimeter')]
#  predictors from test observation set
test.X <- test[,c('Area', 'MajorAxisLength', 'MinorAxisLength', 'Eccentricity', 'ConvexArea', 'Extent', 'Perimeter')]
#  response variable from training observation set
train.Y <- train[,c('Class')]
#  response variable from test observation set
test.Y <- test[,c('Class')]


# create a number of knn models for comparison, varying k
#  for k=1
knn1.pred=knn(train.X, test.X, train.Y, k=1)
#  for k=2
knn2.pred=knn(train.X, test.X, train.Y, k=2)
#  for k=3
knn3.pred=knn(train.X, test.X, train.Y, k=3)
#  for k=4
knn4.pred=knn(train.X, test.X, train.Y, k=4)
#  for k=5
knn5.pred=knn(train.X, test.X, train.Y, k=5)
#  for k=6
knn6.pred=knn(train.X, test.X, train.Y, k=6)
#  for k=7
knn7.pred=knn(train.X, test.X, train.Y, k=7)
# for k = 10
knn10.pred=knn(train.X, test.X, train.Y, k=10)

# assessing the performance of each of the knn models
#  against the test data
mean(knn1.pred==test.Y)
mean(knn2.pred==test.Y)
mean(knn3.pred==test.Y)
mean(knn4.pred==test.Y)
mean(knn5.pred==test.Y)
mean(knn6.pred==test.Y)
mean(knn7.pred==test.Y)
mean(knn10.pred==test.Y)

confmatrix <- table(knn5.pred, test.Y)
knn5.sensitivity = confmatrix[1,1] / (confmatrix[1,1]+confmatrix[2,1])
knn5.specificity = confmatrix[2,2] / (confmatrix[2,2]+confmatrix[1,2])
knn5.PPV = confmatrix[1,1] / (confmatrix[1,1]+confmatrix[1,2])
knn5.NPV = confmatrix[2,2] / (confmatrix[2,2] + confmatrix[2,1])


# assessing the performance of the knn model where k = 5
#  against the training data
knn5.train.pred=knn(train.X, train.X, train.Y, k=5)
mean(knn5.train.pred==train.Y)
confmatrix <- table(knn5.train.pred, train.Y)
knn5.train.sensitivity = confmatrix[1,1] / (confmatrix[1,1]+confmatrix[2,1])
knn5.train.specificity = confmatrix[2,2] / (confmatrix[2,2]+confmatrix[1,2])
knn5.train.PPV = confmatrix[1,1] / (confmatrix[1,1]+confmatrix[1,2])
knn5.train.NPV = confmatrix[2,2] / (confmatrix[2,2] + confmatrix[2,1])
