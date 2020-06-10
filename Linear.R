# Linear Regression
# _________________________________________________
# I sought to predict sales using variable Tv from the Advertisement data.

# Load libraries
library(MASS)
library(ISLR)
library(dplyr)
library(tidyverse)
library(ggpubr)
theme_set(theme_pubr())

# Import data
path = "http://faculty.marshall.usc.edu/gareth-james/ISL/Advertising.csv"
data <- read.csv(path, header = TRUE, na.strings = "?")
attach(data)

# View the data
sample_n(data, 4)

# Drop the column "X" as it matches the index of the table.
data = data[, -1]

# Check for any missing data
length(which(is.na(data) == TRUE))
visdat::vis_dat(data) # no missing data

# Visualization
ggplot(data, aes(x = TV, y = sales)) +
    geom_point() +
    stat_smooth(colour = "red") +
    ggtitle("TV vs SALES") + font("title", size = 29) +
    ylab("Sales") + font("ylab", size = 20) +
    xlab("TV Advertisement Budget(000)") + font("xlab", size = 20)
#The graph above suggests a linearly increasing relationship between the sales and the TV advertising budget variables

# Simple correlation.
cor(sales, TV)

# 0.7822244:
# It is right to strongly believe an increase in TV advertisement budget will result to an increase in Sales

# ________________________________________________________
# Simple Linear Regression
# ________________________________________________________
# The simple linear regression is used to predict a quantitative outcome y(sales) on the basis of one single predictor variable x(TV).
# Assumption: There is approximately a linear relationship between X and Y .
# Find if there is a relationship between sales(Y) and TV(X):


# Fit a simple linear regression model, with sales as the response and Tv as the predictor.
lm.fit = lm(sales~TV)
lm.fit

# The equation is: sales ~ 7.03259 + 0.04754 TV

ggplot(data, aes(x = TV, y = sales,, colour = "red")) + geom_point() + stat_smooth(method = lm, colour = "blue")+
    ggtitle("Regression: TV vs SALES") + font("title", size = 29) +
    ylab("Sales") + font("ylab", size = 20) +
    xlab("TV Advertisement Budget(000)") + font("xlab", size = 20)

# _________________________________________________________
# MODEL ASSESMENT(Quality of the model)
# ________________________________________________________
summary(lm.fit)
# _________________________________________________________
# Assessing the Accuracy of the Coefficient Estimates(Coefficients significance)
# Does TV as a media contribute to sales?
#   Examine the p-values associated with each predictor's t-statistic
#   H0 : ??1 = 0
#   Ha : ??1 != 0

summary(lm.fit)$coefficient

#             Estimate  Std. Error  t value Pr(>|t|)    
# (Intercept) 7.032594   0.457843   15.36   <2e-16 ***
# TV          0.047537   0.002691   17.67   <2e-16 ***
#             p-value: < 2.2e-16
# p < 0.05 There is a significant association between TV and sales
# Both the p-values ( Pr(r|t|) ) for the intercept and the predictor variable are highly significant,
# Reject null hypothesis and accept the alternative hypothesis: There is a relationship existing between X and Y.

# Confidence Interval for the model:
confint(lm.fit)
# That is, there is approximately a 95% chance that the interval [0.042, 0.053] will contain the true value of b1
 

#_______________________________________________________
# MODEL ACCURACY
# _______________________________________________________

# Assessing the Accuracy of the Model(This process is also referred to as the goodness-of-fit)
# How well does the model fit the data?
# Having established there being a significant relationship between TV advertisement budget and sales,
# determine the extent to which the model fits:


# 1. Residual Standard Error
cat("RSE: ",sigma(lm.fit), "\n")
#____________________________
# RSE is the residual variation
# RSE provides an absolute measure of patterns in the data that can't be explained by the model. The smaller the RSE, the better the model.
# Actual sales in each market deviate from the true regression line by approximately 3,259 units, on average.

# Prediction Error rate.
# It’s a measure of how well the model predicts the response variable.
# It is found by diving the RSE by the mean of sales: (RSE/mean(sales):
cat("PE:",sigma(lm.fit)/mean(sales), "\n") # 23% error rate


# 2. R-Squared
cat("R-squared: ",summary(lm.fit)$r.squared, "\n") # Equal to square of pearson coefficient[cor(sales, TV)^2]
#This tell us that 61.19% of variability in the response has been explained by the model.


# 3. F-statistic
# The F-statistic gives the overall significance of the model.
cat("F-statistic: \n", summary(lm.fit)$fstatistic, fill = 1)

# F-value = 312.145 with 1 degree of freedom, p-value 2.2e-16 which is highly significant.

# Pediction
lm.fit %>% predict(data.frame(TV =c(4))) # input a value of TV advertisement budget in the place of 4. I f multiple values then: c(a, b, c)