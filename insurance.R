library(dplyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(doBy)
library(rpart.plot)
library(caret)
library(psych)
library(GPArotation)
library(randomForest)
install.packages("CRAN")
library(CRAN)
install.packages("gbm")
library(gbm)

df=read.csv('C:/Users/saluv/OneDrive/Desktop/DA/R/health_insurance_2.csv')
head(df)

#linear regression
index=createDataPartition(y=df$charges,p=0.7,list=FALSE)
df_train=df[index,]
df_test=df[-index,]

trcl=trainControl(method='repeatedcv',number=10,repeats=3)

line_reg=train(charges~.,data=df,method='lm',trainControl=trcl,tuneLength=10)

summary(line_reg)
line_reg$results[c("RMSE","Rsquared")]
df_test$predict=predict(line_reg,newdata=df_test)
df_test$error=df_test$charges-df_test$predict
df_test$errorsquare=df_test$error^2
error_sum=sum(df_test$errorsquare)
error_mean=error_sum/length(df_test$charges)
root_mse=error_mean^0.5
root_mse
df_predict=predict(line_reg,newdata=df_test)

#SVm
index=createDataPartition(y=df$charges,p=0.7,list=FALSE)
df_train=df[index,]
df_test=df[-index,]
trcl=trainControl(method='repeatedcv',number=10,repeats=3)
svm=train(charges~.,data=df_train,trControl=trcl,method='svmLinear',tuneLength=10)
svm$results[c('RMSE','Rsquared')]
df_test$predict=predict(svm,newdata=df_test)
head(df_test)
df_test$error=df_test$charges-df_test$predict
df_test$sqerror=df_test$error^2
sumerror=sum(df_test$sqerror)
root_sme=(sumerror/length(df_test$charges))^0.5
root_sme

#Random forest
tree_fit=randomForest(charges~.,data=df_train)
df_test$predict=predict(tree_fit,newdata=df_test)
head(df_test)

#gradient boosting
boost_fit=gbm(charges~.,data=df_train,distribution="gaussian",n.trees=10000)
summary(boost_fit)
df_test$predict=predict(boost_fit,newdata=df_test,n.trees=10000)
head(df_test)
df_test$error=df_test$charges-df_test$predict
df_test$sqerror=df_test$error^2
sumerror=sum(df_test$sqerror)
root_sme=(sumerror/length(df_test$charges))^0.5
root_sme

