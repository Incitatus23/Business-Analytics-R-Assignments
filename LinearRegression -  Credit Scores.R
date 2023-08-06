library(tidyverse)
library(lm.beta)

library(car)

library(olsrr)

setwd("C:/Users/adaml/Downloads/")
creditdf<-read.csv("Credit.csv")
View(creditdf)

summary(creditdf)

#partition the data into a training set and a validation set
#set seed so the random sample is reproducible
set.seed(42)
sample <- sample(c(TRUE, FALSE), nrow(creditdf), replace=TRUE, prob=c(0.5,0.5))
traincredit  <-creditdf[sample, ]
validatecredit <- creditdf[!sample, ]

#create a correlation matrix with all quantitative variables in the dataframe
cor(traincredit[c(1, 2, 3, 4, 5, 9)])

#Perform a multiple regression with all variables
training_MR <- lm(Balance ~ Income + Limit + Rating + Age + Education + Student + Gender + Married,  data = traincredit)
summary(training_MR)

vif(training_MR)

training_MR2 <- lm(Balance ~ Income + Rating + Age + Education + Student + Gender + Married,  data = traincredit)
summary(training_MR2)


lm.beta(training_MR2)

training_pred = predict(training_MR2)

#Create a vector of residuals generated from the multiple regression above
training_res = resid(training_MR2)


#Create a data frame of the predicted values and the residuals
pred_res_df <- data.frame(training_pred, training_res)

#create a scatterplot of the residuals versus the predicted values
ggplot(data = pred_res_df, mapping = aes(x = training_pred, y = training_res)) +
  geom_point() +
  labs(title = "Plot of residuals vs. predicted values", x = "Predicted values",
       y = "Residuals")


#Steps to create a Normal Probability Plot 

#create a vector of standardized residuals generated from the multiple
#regression above
training_std.res = rstandard(training_MR2)

#produce normal scores for the standardized residuals and create
#normal probability plot
qqnorm(training_std.res, ylab = "Standardized residuals", xlab = "Normal scores")

summary(training_MR2)

training_MR3 <- lm(Balance ~ Income + Rating + Age + Student,   data = traincredit)
summary(training_MR3)

lm.beta(training_MR3)

validate_MR <- lm(Balance ~ Income + Rating + Age + Student,   data = validatecredit)

summary(validate_MR)

#read inventory dataset into R
inventorydf <- read.csv("credit_pred.csv")
View(inventorydf)

predict(validate_MR, inventorydf, interval = "prediction", level = 0.95)