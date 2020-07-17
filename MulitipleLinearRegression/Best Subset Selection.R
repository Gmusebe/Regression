# Improving the linear Model.
#1. Best Subset Selection:
 # We wish to predict the sales cost on the basis of TV, newspaper and radio adverisement budgets.

# Import Libraries:
library(tidyverse)
library(dplyr)
library(caret)
library(ggpubr)
library(ggcorrplot)

# Import data:
ins_data <- read.csv("Insurance.csv", header = TRUE)
attach(ins_data)

# Correlation:
ggcorrplot(round(cor(ins_data), 1), hc.order = TRUE, type = "lower", 
           outline.col = "white",
           ggtheme = ggpubr::theme_pubclean(),
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE)

ins_data$sex <- as.factor(ins_data$sex)
ins_data$smoker <- as.factor(ins_data$smoker)

# Unique values in children as a predictor:
unique(ins_data$children)
ins_data$children <-as.factor(ins_data$children) 

# Model __________________
# Split data:
train <- ins_data$insuranceclaim %>% 
   createDataPartition(p = 0.65, list = FALSE)

train.data <- ins_data[train, ]
Validation.data <- ins_data[-train, ]

# Linear Model
lm.fit <- lm(insuranceclaim~., data = train.data)
summary(lm.fit)

# Predict ________________
predictions <- lm.fit %>% predict(Validation.data)

# Model performance:
data.frame(RMSE = RMSE(predictions, Validation.data$insuranceclaim),
           R2 = R2(predictions, Validation.data$insuranceclaim),
           MAE = MAE(predictions, Validation.data$insuranceclaim),
           PER = RMSE(predictions, Validation.data$insuranceclaim)/mean(Validation.data$insuranceclaim))

### Improving the model 
library(leaps)
 sub_fit <-  regsubsets(insuranceclaim~., data = ins_data)
 summary_sub_fit <- summary(sub_fit)
 
 names(summary(sub_fit))
 summary_sub_fit$rsq
 
 # Plot RSS, adjusted R2, Cp, and BIC for all of the models
theme_set(theme_pubr())
 par(mfrow = c(2, 2))
 
plot(summary_sub_fit$rss,
     xlab = "NUmber of Variables",
     ylab = "RSS",
     type = "l")
 
plot(summary_sub_fit$adjr2,
      xlab = "Number of Variables",
      ylab = "Adjusted RSq",
      type = "l")
 
 which.max(summary_sub_fit$adjr2)
 points(2, summary_sub_fit$adjr2[2], col = "red", cex = 2, pch = 20)
 
 plot(summary_sub_fit$bic, xlab = "NUmber of Variables", ylab = "BIC", type = "l")
 which.min(summary_sub_fit$bic)
 
# From the above the most efficient model is the one with the bmi and smoker1.
# The coefficients are:
coef(sub_fit, 2)
