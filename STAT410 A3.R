# STAT410 
# ASSIGNMENT 3
# ADAM HOPKINS
# STUDENT NUMBER: 220087256

library(ggplot2)
library(GGally)

concrete.df <- read.csv("A3concrete.csv")
fix(concrete.df)

# Q1, a)
#exploratory pairs plot
ggpairs(concrete.df)

# Q1, b)
# fitting a main effects model
mod1 <- lm(Strength~., data = concrete.df)
summary(mod1)

# caculating the variance inflation factor
# associated with each predictor in the model
library (car)
vif(mod1)

# Q1, c)
# Run backward stepwise model selection with the complete
# second order model as the "upper" model. Include all relevant
# outputs and the summary table for the final model.

#create a complete second order model to use as a start model
start.model <- lm(Strength~Cement+I(Cement^2)+Water+I(Water^2)+Coarse.Aggregate+I(Coarse.Aggregate^2)+Water:Cement+Water:Coarse.Aggregate+Cement:Coarse.Aggregate, data = concrete.df)
formL <- formula(~1)
formU <- formula(~Cement+I(Cement^2)+Water+I(Water^2)+Coarse.Aggregate+I(Coarse.Aggregate^2)+Water:Cement+Water:Coarse.Aggregate+Cement:Coarse.Aggregate)

#backwards stepwise model selection, starting with the complete 
# second order model
# displaying AIC corresponding to resulting model from dropping
# a predictor at each step
bstep.model <- step(start.model, trace = 1, direction="backward", scope=list(lower=formL, upper=formU))
#summary of the final model arrived at using backward stepwise
# model selection
summary(bstep.model)

#d) 
# residual analysis with exploratory plots
par(mfrow=c(2,2))
plot(bstep.model)

# confidence interval of the coefficient estimates
confint(bstep.model, level = 0.95)



# QUESTION 2
# load data
ventilation.df <- read.csv("ventilation.csv", header = TRUE)

# initial visual data inspection
fix(ventilation.df)
names(ventilation.df)

is.factor(ventilation.df$ventilation)
ventilation.df$ventilation <- as.factor(ventilation.df$ventilation)
summary(ventilation.df)
levels(ventilation.df$ventilation)
# drop first two columns, which do not contain data that 
#  we are interested in for this analysis
ventilation.df <- ventilation.df[, c(3, 4)]

attach(ventilation.df)

# a)
aggregate(bacteria~ventilation, data = ventilation.df, mean)
aggregate(bacteria~ventilation, data = ventilation.df, var)


par(mfrow=c(1,1))
plot(bacteria~ventilation, data = ventilation.df, 
     main = "Ventilation Type vs. Bacteria Count",
     xlab = "Ventilation Type",
     ylab = "units of DNA copies / g of dust")
plot(bacteria~ventilation, data = ventilation.df, log='y',
     main = "Ventilation Type vs. Bacteria Count",
     xlab = "Ventilation Type",
     ylab = "log (units of DNA copies / g of dust)")

bacteria.lm <- lm(bacteria~ventilation, data = ventilation.df)
par(mfrow=c(2,2))
plot(bacteria.lm)


# b)
library(MASS)
bc <- boxcox(bacteria~ventilation, data = ventilation.df, lambda = seq(from=0, to=1, by=0.01))

#finds the confidence interval for lambda
range(bc$x[bc$y > max(bc$y)-qchisq(0.95,1)/2])
#give d that maximises the log-Likelihood
lambda <- bc$x[which.max(bc$y)]
lambda

#assessing residuals plots after power transformation of y
bacteria.lm2 <- lm((bacteria^0.06)~ventilation, data = ventilation.df)
par(mfrow=c(2,2))
plot(bacteria.lm2)

# Fmax calculations
# without transformation
(max(aggregate(bacteria~ventilation, data = ventilation.df, var)[,2])
 / min(aggregate(bacteria~ventilation, data = ventilation.df, var)[,2]))
# with lambda = 0.5
(max(aggregate((bacteria^0.5)~ventilation, data = ventilation.df, var)[,2])
        / min(aggregate((bacteria^0.5)~ventilation, data = ventilation.df, var)[,2]))
# with lambda = ln
(max(aggregate((log(bacteria))~ventilation, data = ventilation.df, var)[,2])
        / min(aggregate((log(bacteria))~ventilation, data = ventilation.df, var)[,2]))
# with lambda = -1
(max(aggregate((bacteria^-1)~ventilation, data = ventilation.df, var)[,2])
     / min(aggregate((bacteria^-1)~ventilation, data = ventilation.df, var)[,2]))
# with lambda = 0.06
(max(aggregate((bacteria^0.06)~ventilation, data = ventilation.df, var)[,2])
        / min(aggregate((bacteria^0.06)~ventilation, data = ventilation.df, var)[,2]))
