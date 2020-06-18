 # Import libraries 
library(MASS)
library(ISLR)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(car)
library(tidyr)
library(ggcorrplot)

# Setting visualisation theme
theme_set(theme_pubr())


# Import data
path = "http://faculty.marshall.usc.edu/gareth-james/ISL/Advertising.csv"
data = read.csv(path, header = TRUE)
attach(data)

data <- data[, -1]

# Correlation
ggcorrplot(round(cor(data), 1), hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggpubr::theme_classic2(),
           colors = c("#6D9EC1", "white", "#E46726"), 
           lab = TRUE)

# Multiple Linear Regression
lm.fit1 = lm(sales~., data = data)
summary(lm.fit1)

# VIF's
vif(lm.fit1)

# 1. Is there a relationship between advertising sales and budget?
# From the F-statistic test, p-value is <2.2e-16 indicating a clear evidence of a relationship between advetisement budgets and sales

# 2. How strong is the relationship?
#  Measures of Models of Accuracy:
#  a.RSE
  sigma(lm.fit1) #= 1.685551
# Mean sales = 14,022
# Percentage error indicated is 12% (1685/14,022)

#  b. R Squared
#  Approximately 90% of the variability in the data can be explained by the model
  
#  3. Which media contribute to sales?
#  Examine the p-values associated with each predictorâs t-statistic:
#  Apart from newspaper budget advertisement budget the other budgets are significantly related to sales.

#  4. How large is the effect of each medium on sales?
confint(lm.fit1)

# The confidence intervals for TV and radio are narrow and far from zero, providing evidence that these media are related to sales.
# But the interval for newspaper includes zero, indicating that the variable is not statistically significant given the values of TV and radio.

# 5. Is the relationship linear?
#  we saw that residual plots can be used in order to identify non-linearity.
data$M_predictions <- predict(lm.fit1)
data$M_residuals <- residuals(lm.fit1)

M_data <- data %>% select(TV, newspaper, radio, sales, M_predictions, M_residuals)

M_data %>% 
  gather(key = "iv", value = "x", -c(sales, M_predictions, M_residuals)) %>%
  ggplot(aes(x = x, y = sales)) +
  geom_segment(aes(xend = x, yend = M_predictions), alpha = .2) +
  geom_point(aes(color = M_residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = M_predictions), shape = 1 ) +
  facet_grid(~ iv, scales = "free_x")

# With this in mind, we can see, as expected, that there is less variability in the predicted values than the actual values..