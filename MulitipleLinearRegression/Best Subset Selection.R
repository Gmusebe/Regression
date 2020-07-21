# Improving the linear Model.
#1. Best Subset Selection:
 # We wish to predict the sales cost on the basis of TV, newspaper and radio advertisement budgets.

# Import Libraries:
library(tidyverse)
library(dplyr)
library(caret)
library(ggpubr)
library(ggcorrplot)

# Import data:
ins_data <- read.csv("Insurance.csv", header = TRUE)
attach(ins_data)

str(ins_data)

ins_data$sex <- as.factor(ins_data$sex)
ins_data$smoker <- as.factor(ins_data$smoker)

unique(ins_data$region)

# Correlation:
ggcorrplot(round(cor(ins_data[, -c(2,5,6)]), 1), hc.order = TRUE, type = "lower", 
           outline.col = "white",
           ggtheme = ggpubr::theme_pubclean(),
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE)


# Model __________________
# Split data:
set.seed(123)
train <- ins_data$charges %>% 
   createDataPartition(p = 0.65, list = FALSE)

train.data <- ins_data[train, ]
Validation.data <- ins_data[-train, ]

# Linear Model
lm.fit <- lm(charges~., data = train.data)
summary(lm.fit)

# Predict ________________
predictions <- lm.fit %>% predict(Validation.data)

# Model performance:
data.frame(RMSE = RMSE(predictions, Validation.data$charges), 
           R2 = R2(predictions, Validation.data$charges), 
           MAE = MAE(predictions, Validation.data$charges), 
           PER = RMSE(predictions, Validation.data$charges)/mean(Validation.data$charges)) 

### Improving the model 
library(leaps)
set.seed(123)
 sub_fit <-  regsubsets(charges~., data = train.data, nvmax = 7)
 summary_sub_fit <- summary(sub_fit)
 
 names(summary(sub_fit))
 summary_sub_fit$rsq
 summary_sub_fit$adjr2 # the model with 5 variables has the highest adjr2
 
 # The prediction error rate:
 test.mat <- model.matrix(charges~., data = Validation.data)
 PER <- rep(NA, 7)
 for (i in 1:7){
    coefi = coef(sub_fit, id = i)
    pred = test.mat[, names(coefi)]%*%coefi
    PER[i] = RMSE(pred, Validation.data$charges)/mean(Validation.data$charges)
 }
 PER
 
 
 # Plot RSS, adjusted R2, Cp, and BIC for all of the models
theme_set(theme_pubr())
# par(mfrow = c(2, 2))
 
plot(summary_sub_fit$rss,
     xlab = "NUmber of Variables",
     ylab = "RSS",
     type = "l")

 
plot(summary_sub_fit$adjr2,
      xlab = "Number of Variables",
      ylab = "Adjusted RSq",
      type = "l")

# plot a red dot to indicate the model with the largest** adjusted R2 statistic, which is the most appropriate model.
 which.max(summary_sub_fit$adjr2)
 points(7, summary_sub_fit$adjr2[7], col = "red", cex = 2, pch = 20)
 
 
# Plot the models with the smallest statistic of cp and bic:
 
 plot(summary_sub_fit$bic, xlab = "NUmber of Variables", ylab = "BIC", type = "l")
 which.min(summary_sub_fit$bic)
 points(4, summary_sub_fit$bic[4], col = "red", cex = 2, pch = 20)

 plot(summary_sub_fit$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
 which.min(summary_sub_fit$cp)
 points(7, summary_sub_fit$cp[7], col = "red", cex = 2, pch = 20)
 
# Plot against adjr2
 plot(sub_fit, scale = "adjr2", main = "Ajusted R^2")
# From the above, by adjusted R2 the most efficient model is the one with the age, bmi, children & smoker and region(each as a dummy variable).
 
# To check for consistency:
library(car)
#layout(matrix(1:2), ncol =2)
 
 res.legend <- 
    subsets(sub_fit, statistic = "adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
 res.legend <- 
    subsets(sub_fit, static = "cp", legend = FALSE, min.size = 5, main = "Mallow Cp ")
 abline(a = 1, b = 1, lty = 2)
 
 # The model with 7 variables(counting dummy variables separately) has the highest R2.
 which.max(summary(sub_fit)$adjr2)
 
 # The five variables are:
summary(sub_fit)$which[7,]

# Now do regression with the best model:
  best_model <- 
     lm(charges ~ age + bmi + children + smoker + region, data = train.data)
  summary(best_model)
  
  predictions <- best_model %>% predict(Validation.data)

 # End....