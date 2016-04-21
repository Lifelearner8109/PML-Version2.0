#Practical Machine Learning- Final Project
#VERSION 2
#4-21-2016


#INSTALL LIBRARIES
library(caret)
library(e1071)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)


#LOAD DATA
train=read.csv("pml-training.csv")
test=read.csv("pml-testing.csv")

dim(train)
dim(test)

names(train)
summary(train)


#DATA CLEANING
nzvCols = nearZeroVar(train)
nzvCols

trainClean=train[,-nzvCols]
testClean=test[,-nzvCols]

dim(trainClean)
dim(testClean)

names(trainClean)
summary(trainClean)


#IDENTIFY VARIABLES WHERE NA >= 19216
ColsToRemove=which(sapply(trainClean, function(x) sum(is.na(x)))>=19216)

trainReady=trainClean[,-ColsToRemove]
testReady=testClean[,-ColsToRemove]

dim(trainReady)
dim(testReady)

names(trainReady)
summary(trainReady)


#CREATE VALIDATION SET
inTraining=createDataPartition(y=trainReady$classe, p=0.7, list=FALSE)
training=trainReady[inTraining,]
validation=trainReady[-inTraining,]

testing=testReady

dim(training)
dim(validation)
dim(testing)

mod1=randomForest(classe~.-X - user_name - raw_timestamp_part_1 - raw_timestamp_part_2 - cvtd_timestamp, data=training, ntree=200, method="class")
pred1=predict(mod1, newdata=validation, type="class")
confusionMatrix(pred1, validation$classe)

mod2=train(classe~.-X - user_name - raw_timestamp_part_1 - raw_timestamp_part_2 - cvtd_timestamp, data=training, method="rf", trControl=trainControl(method="cv", number=3))
pred2=predict(mod2, newdata=validation)
confusionMatrix(pred2, validation$classe)


finalPred=predict(mod1, newdata=test)
finalPred
