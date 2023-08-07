#install packages
install.packages ("tidyverse")
install.packages("TTR")

#load libraries
library(tidyverse)
library(TTR)

#set working directory
setwd("C:/Users/adaml/Downloads/")

#read dataset into R
milkdf <- read.csv("united_dairies.csv")
View(milkdf)

#create a time series plot showing 12 weeks of milk sales
ggplot(data = milkdf, mapping = aes(x = Week, y = Sales)) +
  geom_line () +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 13, by = 1)) +
  labs(title = "Weekly milk sales for United Dairies", x = "Week", y = "Sales")

#create a separate vector for the actual weekly sales
sales_actuals<-milkdf$Sales

#use the naive method to forecast the 13th week of milk sales
naive13 <- c(NA, sales_actuals)
naive13

#The last value in the vector is the forecast for sales for the 13th week

#Create functions for the accuracy measures with vector of actual values 
#and vector of predicted values as inputs
mae<-function(actual,pred){
  mae <- mean(abs(actual-pred), na.rm=TRUE)
  return (mae)
}

mse<-function(actual,pred){
  mse <- mean((actual-pred)^2, na.rm=TRUE)
  return (mse)
}

rmse<-function(actual,pred){
  rmse <- sqrt(mean((actual-pred)^2, na.rm=TRUE))
  return (rmse)
}  

mape<-function(actual,pred){
  mape <- mean(abs((actual - pred)/actual), na.rm=TRUE)*100
  return (mape)
}

#Adjust the vector of predicted values to align with the sales_actuals vector
Naive_pred <- naive13[-length(naive13)]


#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(sales_actuals, Naive_pred)
mse(sales_actuals, Naive_pred)
rmse(sales_actuals, Naive_pred)
mape(sales_actuals, Naive_pred)

#use the simple moving average method to forecast the 13th week of milk sales

sma13<-SMA (sales_actuals, n=3)
sma13

#The last value in the vector is the forecast for sales for the 13th week

#Adjust the vector of predicted values to align with the sales_actuals vector
sales_ma_pred<-c(NA, sma13[-length(sma13)]) 
sales_ma_pred

#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(sales_actuals, sales_ma_pred)
mse(sales_actuals, sales_ma_pred)
rmse(sales_actuals, sales_ma_pred)
mape(sales_actuals, sales_ma_pred)


#use the exponential smoothing method with alpha = 0.2 to forecast the 
#13th week of milk sales
exp13 <- EMA (sales_actuals, n=1, ratio = .2)
exp13

#The last value in the vector is the forecast for sales for the 13th week

#Adjust the vector of predicted values to align with the sales_actuals vector
exp_pred <- c(NA, exp13[-length(exp13)])

#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mape(sales_actuals, exp_pred)
mae(sales_actuals, exp_pred)
mse(sales_actuals, exp_pred)
rmse(sales_actuals, exp_pred)


#use the exponential smoothing method with alpha = 0.4 to forecast the 
#13th week of milk sales
exp13_4 <- EMA (sales_actuals, n=1, ratio = .4)
exp13_4

#The last value in the vector is the forecast for sales for the 13th week

#Adjust the vector of predicted values to align with the sales_actuals vector
exp_pred_4 <- c(NA, exp13_4[-length(exp13_4)])

#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(sales_actuals, exp_pred_4)
mse(sales_actuals, exp_pred_4)
rmse(sales_actuals, exp_pred_4)
mape(sales_actuals, exp_pred_4)
















#use the naive method to forecast the 13th week of milk sales
naive13 <- naive(milkdf$Sales, h=1)

#show the prediction for the 13th week of milk sales
naive13

#Calculate accuracy measures for the naive forecast
naive_acc <- accuracy (naive13)


