rm(list = ls())

#Set working directory and read the data file
setwd("C:/Users/vkumar/Downloads")
library("readxl")
dataset=read_excel("premium.xlsx" , sheet = 1)
str(dataset)

library(DMwR)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(data.table)
library(ROCR)
library(ineq)
library(randomForest)
library(default)

anyNA(dataset)
summary(dataset)
str(dataset)
attach(dataset)

dataset$cashPercent = dataset$perc_premium_paid_by_cash_credit*100
head(dataset$perc_premium_paid_by_cash_credit)
head(dataset$cashPercent)

dataset$age = round(dataset$age_in_days/365, digits=0)
# summary(dataset$age)

dataset$Income = round(dataset$Income/1000000, digits=2)
# summary(dataset$Income)

dataset$countLatePayment = dataset$`Count_3-6_months_late`+dataset$`Count_6-12_months_late`+dataset$Count_more_than_12_months_late
head(dataset$countLatePayment)

dataset$residence_area_type=ifelse(dataset$residence_area_type=="Rural",1,0)
head(dataset$residence_area_type)

dataset$sourcing_channel=ifelse(dataset$sourcing_channel=="A",1,(ifelse(dataset$sourcing_channel=="B",2,(ifelse(dataset$sourcing_channel=="C",3,(ifelse(dataset$sourcing_channel=="D",4,5)))))))

# str(dataset)
# summary(dataset)
attach(dataset)

x <- Income
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
Income=x
hist(Income)
boxplot(Income)

x <- age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
caps
H <- 1.5 * IQR(x, na.rm = T)
H
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
age=x
hist(age)
boxplot(age)

x <- cashPercent
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
cashPercent=x
hist(cashPercent)
boxplot(cashPercent)

x <- countLatePayment
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
countLatePayment=x
hist(countLatePayment)
boxplot(countLatePayment)

x <- risk_score
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
risk_score=x
hist(risk_score)
boxplot(risk_score)

x <- `Count_3-6_months_late`
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
`Count_3-6_months_late`=x
hist(`Count_3-6_months_late`)
boxplot(`Count_3-6_months_late`)

x <- `Count_6-12_months_late`
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
`Count_6-12_months_late`=x
hist(`Count_6-12_months_late`)
boxplot(`Count_6-12_months_late`)

x <- Count_more_than_12_months_late
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
caps
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
Count_more_than_12_months_late=x
hist(`Count_more_than_12_months_late`)
boxplot(`Count_more_than_12_months_late`)

hist(dataset$`Marital Status`)
 boxplot(`Marital Status`)
 hist(Veh_Owned)
 boxplot(Veh_Owned)

 hist(No_of_dep)
 boxplot(No_of_dep)
 hist(Accomodation)
 boxplot(Accomodation)
 hist(risk_score)
 boxplot(risk_score)
 hist(no_of_premiums_paid)
 boxplot(no_of_premiums_paid)

x <- no_of_premiums_paid
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
no_of_premiums_paid=x
hist(no_of_premiums_paid)
boxplot(no_of_premiums_paid)

x <- premium
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
premium=x
hist(premium)
boxplot(premium)

 hist(renewal)
 boxplot(renewal)
 
 hist(residence_area_type)
 boxplot(residence_area_type)

dataset_const_vars = dataset[,c(1,6,7,9,10,12,14,15,16)]
corrmatrix = cor(dataset_const_vars)
library(corrplot)
corrplot(corrmatrix,method='number', type='upper', order='FPC')
plot(dataset_const_vars)

dataset$`Marital Status`=as.factor(dataset$`Marital Status`)
dataset$Accomodation=as.factor(dataset$Accomodation)
dataset$residence_area_type=as.factor(dataset$residence_area_type)
dataset$sourcing_channel=as.factor(dataset$sourcing_channel)

#summary(dataset)
par(mfrow=c(2,2)) 
boxplot(Income~residence_area_type, main="Income vs  Residence Area Type")
boxplot(countLatePayment~residence_area_type, main="Late Payment Count vs  Residence Area Type")
boxplot(no_of_premiums_paid~residence_area_type, main="No. of premium paid vs Residence Area Type")
boxplot(risk_score~residence_area_type, main="Risk Score vs Residence Area Type")

 par(mfrow=c(2,2)) 
 boxplot(Income~Accomodation, main="Income vs  Accomodation")
 boxplot(countLatePayment~Accomodation, main="Late Payment Count vs  Accomodation")
 boxplot(no_of_premiums_paid~Accomodation, main="No. of premium paid vs Accomodation")
 boxplot(risk_score~Accomodation, main="Risk Score vs Accomodation")
 
 par(mfrow=c(2,2)) 
 boxplot(Income~sourcing_channel, main="Income vs  sourcing channel")
 boxplot(countLatePayment~sourcing_channel, main="Late Payment Count vs  sourcing channel")
 boxplot(no_of_premiums_paid~sourcing_channel, main="No. of premium paid vs sourcing channel")
 boxplot(risk_score~sourcing_channel, main="Risk Score vs sourcing channel")


dataset = dataset[,c(-1)]
str(dataset)

normalize<-function(x){
  +return((x-min(x))/(max(x)-min(x)))}
dataset$perc_premium_paid_by_cash_credit = normalize(dataset$perc_premium_paid_by_cash_credit)
#dataset$age_in_days = normalize(dataset$age_in_days)
dataset$Income = normalize(dataset$Income)
#dataset$`Count_3-6_months_late` = normalize(dataset$`Count_3-6_months_late`)
#dataset$`Count_6-12_months_late` = normalize(dataset$`Count_6-12_months_late`)
#dataset$Count_more_than_12_months_late = normalize(dataset$Count_more_than_12_months_late)
#dataset$Veh_Owned = normalize(dataset$Veh_Owned)
#dataset$No_of_dep = normalize(dataset$No_of_dep)
dataset$risk_score = normalize(dataset$risk_score)
dataset$no_of_premiums_paid = normalize(dataset$no_of_premiums_paid)
dataset$premium = normalize(dataset$premium)
dataset$age = normalize(dataset$age)
dataset$cashPercent = normalize(dataset$cashPercent)
#dataset$countLatePayment = normalize(dataset$countLatePayment)
#change name of Marital Status column
names(dataset)[7]=paste("Marital_Status")


str(dataset)
dataset = dataset[,c(-1,-2)]
str(dataset)

set.seed(1234)
pd<-sample(2,nrow(dataset),replace=TRUE, prob=c(0.7,0.3))
train<-dataset[pd==1,]
test<-dataset[pd==2,]

table(train$renewal)

prop.table(table(train$renewal))

train=as.data.frame(train)
train$renewal=as.factor(train$renewal)

smote.train <- SMOTE(renewal ~., data = train, perc.over = 500, k = 5, perc.under = 200)

table(smote.train$renewal)
prop.table(table(smote.train$renewal))

table(test$renewal)
test=as.data.frame(test)
test$renewal=as.factor(test$renewal)
smote.test <- SMOTE(renewal ~., data = test, perc.over = 1000, k = 5, perc.under = 100)
table(smote.test$renewal)
prop.table(table(smote.test$renewal))

#  str(smote.test)
#  smote_features_train<-as.matrix(smote.test[,c(1:15,17:19)])
#  smote_label_train<-as.matrix(smote.test$renewal)
#  str(smote_features_train)
# smote.xgb.fit <- xgboost(
#   data = smote_features_train,
#   label = smote_label_train,
#   eta = 0.7,
#   max_depth = 5,
#   nrounds = 50,
#   nfold = 5,
#   objective = "binary:logistic",  # for regression models
#   verbose = 0,               # silent,
#   early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
# )
# 
# smote_features_test<-as.matrix(smote.test[,c(1:9,11:13)])
# smote.test$smote.pred.class <- predict(smote.xgb.fit, smote_features_test)
# table(smote.test$renewal,smote.test$smote.pred.class>=0.5)
# str(smote.train)
# 

str(smote.train)

#base data
smote.train.base=smote.train[,c(-17)]
str(smote.train.base)
german_logistic_base <- glm(renewal~ ., data=smote.train.base, family=binomial(link="logit"))
summary(german_logistic_base)

german_logistic_base <- glm(renewal~ .-Veh_Owned -sourcing_channel -residence_area_type, data=smote.train.base, family=binomial(link="logit"))
summary(german_logistic_base)

attach(smote.train.base)
# 
# #revised data
# str(smote.train)
# smote.train.filter=smote.train[,c(-2,-3,-4)]
# str(smote.train.filter)
# german_logistic_filter <- glm(renewal~., data=smote.train.filter, family=binomial(link="logit"))
# summary(german_logistic_filter)
# 
# german_logistic_filter <- glm(renewal~. -Veh_Owned -sourcing_channel -residence_area_type, data=smote.train.filter, family=binomial(link="logit"))
# summary(german_logistic_filter)

str(smote.test)
smote.test.base=smote.test[,c(-1)]
str(smote.test.base)
smote.test.base$log.pred<-predict(german_logistic_base, smote.test.base[,c(-14)], type="response")
smote.test.base$log.pred
p_class<-ifelse(smote.test.base$log.pred > 0.75,"good", "bad")
table(p_class)
write.csv(file="test_logistic.csv",smote.test.base)


library(car)
library(caret)
library(class)
library(devtools)
library(e1071)
install.packages("ggord")
library(ggord)
library(ggplot2)
library(Hmisc)
library(klaR)
library(klaR)
library(MASS)
library(nnet)
library(plyr)
library(pROC)
library(psych)
library(scatterplot3d)
require(devtools)
install_version("SDMTools", "1.1-20")
library(SDMTools)
library(dplyr)
install_version("ElemStatLearn", "2015.6.26.2")
library(ElemStatLearn)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)
library(ROCR)

confusion.matrix(smote.test.base$renewal,smote.test.base$log.pred,threshold=0.85)

tab.logit= table(smote.test.base$renewal,smote.test.base$log.pred>0.85)
tab.logit
accuracy.logit<-sum(diag(tab.logit))/sum(tab.logit)
accuracy.logit
loss.logit<-tab.logit[1,2]/(tab.logit[1,2]+tab.logit[1,1])
loss.logit
opp.loss.logit<-tab.logit[2,1]/(tab.logit[2,1]+tab.logit[2,2])
opp.loss.logit
tot.loss.logit<-0.95*loss.logit+0.05*opp.loss.logit
tot.loss.logit
#smote.test.base$log.pred = as.data.frame(smote.test.base$log.pred)
smote.test.base$renewal = as.data.frame(smote.test.base$renewal)
pred=prediction(smote.test.base$log.pred,smote.test.base$renewal)
pred
perf = performance(pred, "tpr", "fpr")
plot(perf)
PRcurve(smote.test.base$log.pred,smote.test.base$renewal)

# AUC
train.auc = performance(pred, "auc")
train.area = as.numeric(slot(train.auc, "y.values"))
train.area

# KS
ks.train <- performance(pred, "tpr", "fpr")
train.ks <- max(attr(ks.train, "y.values")[[1]] - (attr(ks.train, "x.values")[[1]]))
train.ks

# smote.test.filter=smote.test[,c(-1,-2,-4,-5,-6)]
# str(smote.test.filter)
# smote.test.filter$log.pred<-predict(german_logistic_filter, smote.test.filter[,c(-11)], type="response")
# tab.logit= table(smote.test.filter$renewal,smote.test.filter$log.pred>0.5)
# tab.logit
# accuracy.logit<-sum(diag(tab.logit))/sum(tab.logit)
# accuracy.logit
# loss.logit<-tab.logit[1,2]/(tab.logit[1,2]+tab.logit[1,1])
# loss.logit
# opp.loss.logit<-tab.logit[2,1]/(tab.logit[2,1]+tab.logit[2,2])
# opp.loss.logit
# tot.loss.logit<-0.95*loss.logit+0.05*opp.loss.logit
# tot.loss.logit
# pred= prediction(smote.test.filter$log.pred,smote.test.filter$renewal)
# perf = performance(pred, "tpr", "fpr")
# plot(perf)
# 

library(car)
vif(german_logistic_base)
# vif(german_logistic_filter)

str(smote.train.base)
names(smote.train.base)[2]=paste("Count_3_6_months_late")
names(smote.train.base)[3]=paste("Count_6_12_months_late")


####use Train data to build random forest ntree = 501 and mtry = sqrt(p)
rndFor = randomForest(renewal ~ ., data = smote.train.base, 
                      ntree=501, mtry = 3, nodesize = 10,
                      importance=TRUE)
rndFor
attributes(rndFor)

##OOB error here is using partial data
rndFor$confusion

plot(rndFor, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest tget.train")

print(rndFor$importance)
smote.train.base$predict.class = predict(rndFor, smote.train.base, type="class")
head(smote.train.base$predict.class)
head(smote.train.base$renewal)
confusionMatrix(smote.train.base$predict.class,smote.train.base$renewal)
smote.train.base$prob1 = predict(rndFor, smote.train.filter, type="prob")[,"1"]
str(smote.train.base)

#tune with mtry=51
tRndFor = tuneRF(x = smote.train.base[,c(-14)], 
                 y=smote.train.base$renewal,
                 mtryStart = 4, 
                 ntreeTry = 51, 
                 stepFactor = 1.5, 
                 improve = 0.0001, 
                 trace=TRUE, 
                 plot = TRUE,
                 doBest = TRUE,
                 nodesize = 10, 
                 importance=TRUE
)

tRndFor = randomForest(renewal ~ ., data = smote.train.base, 
                       ntree=501, mtry = 13, nodesize = 10,
                       importance=TRUE)
importance(tRndFor)

varUsed(tRndFor)
str(smote.test)
smote.test.base=smote.test[,c(-17)]
str(smote.test.base)
str(smote.train.base)
names(smote.test.base)[2]=paste("Count_3_6_months_late")
names(smote.test.base)[3]=paste("Count_6_12_months_late")
smote.test.base$predict.class = predict(tRndFor, smote.test.base[,c(-14)], type="class")

confusionMatrix(smote.test.base$predict.class,smote.test.base$renewal)
smote.test.base$prob1 = predict(tRndFor, smote.test.base[,c(-14)], type="prob")[,"1"]
#tbl=table(smote.test.base$renewal, smote.test.base$predict.class)
#tbl
accuracy.rf<-sum(diag(tbl))/sum(tbl)
accuracy.rf

loss.rf<-tbl[2,1]/(tbl[2,1]+tbl[1,1])
loss.rf

opp.loss.rf<-tbl[1,2]/(tbl[1,2]+tbl[2,2])
opp.loss.rf

tot.loss.rf<-0.95*loss.rf+0.05*opp.loss.rf
tot.loss.rf

library(ROCR)
library(ineq)
predObj = prediction(smote.test.base$prob1, smote.test.base$renewal)
perf = performance(predObj, "tpr", "fpr")
plot(perf)

KS = max(perf@y.values[[1]]-perf@x.values[[1]])
KS

auc = performance(predObj,"auc"); 
auc = as.numeric(auc@y.values)
auc

PRcurve(smote.test.base$prob1,smote.test.base$renewal)

#loading a few libraries
install.packages('gbm')
library(gbm)          # basic implementation using AdaBoost
install.packages('xgboost')
library(xgboost)      # a faster implementation of a gbm
install.packages('caret')
library(caret)        # an aggregator package for performing many machine learning 
library(ipred)
library(rpart)
German.bagging <- bagging(renewal ~., data= smote.train.base,control=rpart.control(maxdepth=5, minsplit=4))

str(smote.test)
smote.test.base=smote.test[,c(-17)]
str(smote.test.base)
names(smote.test.base)[2]=paste("Count_3_6_months_late")
names(smote.test.base)[3]=paste("Count_6_12_months_late")
smote.test.base$pred.class <- predict(German.bagging, smote.test.base[-14])

tab.bagging = table(smote.test.base$renewal, smote.test.base$pred.class)

accuracy.bagging<-sum(diag(tab.bagging))/sum(tab.bagging)
accuracy.bagging

loss.bagging<-tab.bagging[1,2]/(tab.bagging[1,2]+tab.bagging[1,1])
loss.bagging

opp.loss.bagging<-tab.bagging[2,1]/(tab.bagging[2,1]+tab.bagging[2,2])
opp.loss.bagging

tot.loss.bagging<-0.95*loss.bagging+0.05*opp.loss.bagging
tot.loss.bagging



