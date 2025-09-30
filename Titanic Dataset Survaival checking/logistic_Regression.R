#import library
library(dplyr)
#importing the test and train data
test<-read.csv("test (1).csv")
train<-read.csv("train (1).csv")
#Convert train data into String
str(train)
#showing the summary of training data for each column
summary(train)
#Removing NA values from train and test data for age by mean value
#for Fare replace NA by 0
train$Age[is.na(train$Age)] = mean(train$Age,na.rm = TRUE)
test$Age[is.na(test$Age)] = mean(test$Age,na.rm = TRUE)
test$Fare[is.na(test$Fare)]<-0
dataset1_train=train %>% select(Survived,Pclass,Sex,Age,SibSp,Parch,Fare)
Titanic_survey = glm(Survived~.,data=dataset1_train,family = binomial)
summary(Titanic_survey)
#Inference based on the above model
#
#Estimate,Standard Error and Z value based on each feature or column and overall are listed
#Pvalue which is less than 0.05, then those features or column are relevant in logistic regression
#Based on the above point overall Intercept(<2e-16), is less than 0.05, so this model is statistically significant
#For Pclass,Sexmale,Age,SibSp are less than 0.05, which means those columns are statistically significant and have strong relationship.
#Fare and Parch are not statistically significant
#Null deviance indicates to what extend intercept only model fit data and high-value is low fit.
#Residual deviance is the deviance of the fitted model with predictors included,and also shows how well data is fitted compared to null model
#Reduction of residual deviance from Null deviance indicate,how much better our model is than just using the interscept.
#Our model improved the fit significantly (from 1186.66 → 788.73), suggesting that your predictors have strong explanatory power.
#The difference in deviance (~398) with 6 degrees of freedom[number of observations-1] (890–884) suggests a statistically significant improvement.
Predict=predict(Titanic_survey,type="response",newdata = test)
Predict
test$Survived= as.numeric(Predict >=0.5)
t=table(test$Survived)
prediction= data.frame(test[c("PassengerId","Survived")])
print(prediction)
#install.packages("gmodels")
library(gmodels)
CrossTable(test$Pclass,test$Survived)
CrossTable(test$Sex,test$Survived)
CrossTable(test$SibSp,test$Survived)
