library(tidyverse)
library(caret)
library(ROCR)
library(ROSE)
library(Hmisc)

setwd("C:/Users/adaml/Downloads/")
#read dataset into R
insdf <- read.csv("insurance.csv")
View(insdf)

summary(insdf)
descr(insdf)
describe(insdf)

#set seed so the random sample is reproducible
set.seed(42)

#Partition the Optiva dataset into a training, validation and test set
Samples<-sample(seq(1,3),size=nrow(insdf),replace=TRUE,prob=c(0.6,0.2,0.2))
Train<-insdf[Samples==1,]
Validate<-insdf[Samples==2,]
Test<-insdf[Samples==3,]


#Convert categorical variables to factors with levels and labels
insdf$CLAIM<-factor(insdf$CLAIM,levels = c(0,1),labels = c("No","Yes"))
insdf$KIDSDRIV<-factor(insdf$KIDSDRIV,levels = c(0,1),labels = c("No","Yes"))
insdf$HOMEKIDS<-factor(insdf$HOMEKIDS,levels = c(0,1),labels = c("No","Yes"))
insdf$HOMEOWN<-factor(insdf$HOMEOWN,levels = c(0,1),labels = c("No","Yes"))
insdf$MSTATUS<-factor(insdf$MSTATUS,levels = c(0,1),labels = c("No","Yes"))
insdf$GENDER<-factor(insdf$GENDER,levels = c(0,1),labels = c("MALE","FEMALE"))
insdf$EDUCATION<-factor(insdf$EDUCATION,levels = c(0,1),labels = c("HIGH SCHOOL ONLY","COLLEGE OR BEYOND"))
insdf$CAR_USE<-factor(insdf$CAR_USE,levels = c(0,1),labels = c("PRIVATE","COMMERCIALs"))
insdf$RED_CAR<-factor(insdf$RED_CAR,levels = c(0,1),labels = c("No","Yes"))
insdf$CLM_BEF<-factor(insdf$CLM_BEF,levels = c(0,1),labels = c("No","Yes"))
insdf$REVOKED<-factor(insdf$REVOKED,levels = c(0,1),labels = c("No","Yes"))
insdf$MVR_PTS<-factor(insdf$MVR_PTS,levels = c(0,1),labels = c("No","Yes"))
insdf$URBANICITY<-factor(insdf$URBANICITY,levels = c(0,1),labels = c("No","Yes"))



#set seed so the random sample is reproducible
set.seed(42)

#Partition the Optiva dataset into a training, validation and test set
Samples<-sample(seq(1,3),size=nrow(insdf),replace=TRUE,prob=c(0.6,0.2,0.2))
Train<-insdf[Samples==1,]
Validate<-insdf[Samples==2,]
Test<-insdf[Samples==3,]

#Logistic regression is part of the general linear model family, so the R 
#function is glm.
options(scipen=999)
lrtrain <- glm(CLAIM ~ KIDSDRIV + AGE + HOMEKIDS + INCOME + HOMEOWN + MSTATUS + GENDER + 
                 EDUCATION + TRAVTIME + CAR_USE + BLUEBOOK + TWC + RED_CAR + CLM_BEF +
                 REVOKED + MVR_PTS + CAR_AGE + URBANICITY , data = Train, 
               family = binomial(link = "logit"))

summary(lrtrain)

# obtain probability of defaulting for each observation in validation set
lrprobs <- predict(lrtrain, newdata = Validate, type = "response")

#Attach probability scores to Validate dataframe
Validate <- cbind(Validate, Probabilities=lrprobs)

# obtain predicted class for each observation in validation set using threshold of 0.5
lrclass <- as.factor(ifelse(lrprobs > 0.5, "Yes","No"))

#Attach predicted class to Validate dataframe
Validate <- cbind(Validate, PredClass=lrclass)

#Create a confusion matrix using "Yes" as the positive class 
confusionMatrix(lrclass, Validate$CLAIM, positive = "Yes" )

#create a prediction object to use for the ROC Curve
predROC <- prediction(lrprobs, Validate$CLAIM)

#create a performance object to use for the ROC Curve
perfROC <- performance(predROC,"tpr", "fpr")

#plot the ROC Curve
plot(perfROC)
abline(a=0, b= 1)

# compute AUC 
performance(predROC, measure="auc")@y.values[[1]]


xsdf<-Train[c(-1)]
View(xsdf)


#Create an oversampled training subset
set.seed(42)
oversample<-upSample(x=xsdf, y=Train$CLAIM, yname = "CLAIM")

table(oversample$CLAIM)

lrtrain2 <- glm(CLAIM ~ KIDSDRIV + AGE + HOMEKIDS + INCOME + HOMEOWN + MSTATUS + GENDER + 
                 EDUCATION + TRAVTIME + CAR_USE + BLUEBOOK + TWC + RED_CAR + CLM_BEF +
                 REVOKED + MVR_PTS + CAR_AGE + URBANICITY , data = oversample, 
               family = binomial(link = "logit"))

# obtain probability of defaulting for each observation in validation set
lrprobs2 <- predict(lrtrain2, newdata = Validate, type = "response")

#Attach probability scores to Validate dataframe
Validate <- cbind(Validate, Probabilities=lrprobs2)

# obtain predicted class for each observation in validation set using threshold of 0.5
lrclass2 <- as.factor(ifelse(lrprobs2 > 0.5, "Yes","No"))

#Attach predicted class to Validate dataframe
Validate <- cbind(Validate, PredClass=lrclass2)

#Create a confusion matrix using "Yes" as the positive class 
confusionMatrix(lrclass2, Validate$CLAIM, positive = "Yes" )


#create a prediction object to use for the ROC Curve
predROC2 <- prediction(lrprobs2, Validate$CLAIM)

#create a performance object to use for the ROC Curve
perfROC2 <- performance(predROC2,"tpr", "fpr")

#plot the ROC Curve
plot(perfROC2)
abline(a=0, b= 1)

# compute AUC 
performance(predROC2, measure="auc")@y.values[[1]]


# obtain probability of defaulting for each observation in validation set
lrprobs3 <- predict(lrtrain2, newdata = Test, type = "response")

#Attach probability scores to Validate dataframe
Validate <- cbind(Validate, Probabilities=lrprobs3)

# obtain predicted class for each observation in validation set using threshold of 0.5
lrclass3 <- as.factor(ifelse(lrprobs3 > 0.5, "Yes","No"))

#Attach predicted class to Validate dataframe
Validate <- cbind(Validate, PredClass=lrclass3)

#Create a confusion matrix using "Yes" as the positive class 
confusionMatrix(lrclass3, Test$CLAIM, positive = "Yes" )

#create a prediction object to use for the ROC Curve
predROC3 <- prediction(lrprobs3, Test$CLAIM)

#create a performance object to use for the ROC Curve
perfROC3 <- performance(predROC3,"tpr", "fpr")

#plot the ROC Curve
plot(perfROC3)
abline(a=0, b= 1)

# compute AUC 
performance(predROC3, measure="auc")@y.values[[1]]


#predict probability of default for new customers

#read new dataset into R
new_customers <- read.csv("insurance_predictions.csv")
View(new_customers)

#Convert categorical variables to factors with levels and labels
#Convert categorical variables to factors with levels and labels
new_customers$CLAIM<-factor(new_customers$CLAIM,levels = c(0,1),labels = c("No","Yes"))
new_customers$KIDSDRIV<-factor(new_customers$KIDSDRIV,levels = c(0,1),labels = c("No","Yes"))
new_customers$HOMEKIDS<-factor(new_customers$HOMEKIDS,levels = c(0,1),labels = c("No","Yes"))
new_customers$HOMEOWN<-factor(new_customers$HOMEOWN,levels = c(0,1),labels = c("No","Yes"))
new_customers$MSTATUS<-factor(new_customers$MSTATUS,levels = c(0,1),labels = c("No","Yes"))
new_customers$GENDER<-factor(new_customers$GENDER,levels = c(0,1),labels = c("MALE","FEMALE"))
new_customers$EDUCATION<-factor(new_customers$EDUCATION,levels = c(0,1),labels = c("HIGH SCHOOL ONLY","COLLEGE OR BEYOND"))
new_customers$CAR_USE<-factor(new_customers$CAR_USE,levels = c(0,1),labels = c("PRIVATE","COMMERCIALs"))
new_customers$RED_CAR<-factor(new_customers$RED_CAR,levels = c(0,1),labels = c("No","Yes"))
new_customers$CLM_BEF<-factor(new_customers$CLM_BEF,levels = c(0,1),labels = c("No","Yes"))
new_customers$REVOKED<-factor(new_customers$REVOKED,levels = c(0,1),labels = c("No","Yes"))
new_customers$MVR_PTS<-factor(new_customers$MVR_PTS,levels = c(0,1),labels = c("No","Yes"))
new_customers$URBANICITY<-factor(new_customers$URBANICITY,levels = c(0,1),labels = c("No","Yes"))


# make predictions for new data (for which loan default is unknown)
lrprobsnew <- predict(lrOver, newdata = new_customers , type = "response")

#Attach probability scores to new_customers dataframe 
new_customers <- cbind(new_customers, Probabilities=lrprobsnew)
View(new_customers)
