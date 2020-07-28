# Forward & Backward Stepwise selection:

# Import Libraries:
library(tidyverse)
library(ggcorrplot)
library(leaps)
library(caret)
library(dplyr)
library(gdata)
library(bestglm)
library(car)

# Import data:
Data <- read.csv("insurancelog.csv", header = TRUE)
attach(Data)

dim(Data)
str(Data)


# Correlation of the variables in the data:
ggcorrplot(round(cor(Data[, -c(2,6)]),5), hc.order = TRUE, type = "lower",
          outline.col = "white",
          ggtheme = ggpubr::theme_pubclean(),
          colors = c("#DAF7A6", "#FFC300", "#900C3F"),
          lab = TRUE)
# bmi is the highest correlated predictor with the insurance claim;
# An increase of bmi is more likely to lead to an insurance claim.
# age, charges & smoker are all positively correlated with insuranceclaim, increasing in the same order.
# children are negatively correlated with insuranceclaim

Data$sex <- as.factor(Data$sex)
Data$region <- as.factor(Data$region)

# logistic regression:
set.seed(123)

train <- Data$insuranceclaim %>%
  createDataPartition(p = 0.75, list = FALSE)

train.data <- Data[train,]
test.data <- Data[-train,]

log_fit <- glm(insuranceclaim~., data = train.data, family = binomial)
summary(log_fit)

# age, bmi, children and smoker are the significant predictors to insuranceclaim
# Select predictors of the best model by forward selection

log_reg.fit <- regsubsets(insuranceclaim~., data = train.data, method = "forward",nvmax = 8)
summary(log_reg.fit)

# The 4 predictor model:
coef(log_reg.fit, 4) # is inclusive of the significant predictors.

# The prediction error rate of the models:
test.matrix <- model.matrix(insuranceclaim~., data= test.data)
PER = rep(NA, 8)
for(i in 1:8){
  coef = coef(log_reg.fit, i)
  pred = test.matrix[, names(coef)]%*%coef
  PER[i] = RMSE(pred, test.data$insuranceclaim)/mean(test.data$insuranceclaim)
}
PER

# The predictors to include in the model by:
which.max(summary(log_reg.fit)$adjr2)
subsets(log_reg.fit, statistic = "adjr2", min.size = 5, legend = FALSE, main = "ADJR^2")

plot(log_reg.fit, scale = "adjr2", main = "Adjr^2")

# The best model by forward selection is thus:

best_model <- glm(insuranceclaim ~ 
                           age + bmi + children + smoker + region + charges,
                         data = Data,
                         family = binomial)
summary(best_model)

# Also 
names(Data) <- tolower(names(Data))
data_for_best_logistic <- within(Data, {
  y <- insuranceclaim
  insuranceclaim <- NULL
})

glm.fit <-
  bestglm(Xy = data_for_best_logistic,
          family = binomial,
          IC = "AIC",
          method = "exhaustive")

# The top models
glm.fit$BestModels

# The best Model:
summary(glm.fit$BestModel)


#...End