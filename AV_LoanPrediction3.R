# Loan Prediction 3 Hackathon - Analytics vidhya 
# AV Article Code Version

library(caret)
library(mice)
library(gmodels)
library(dummies)
library(RANN)
library(randomForest)

set.seed(1)
#### Reading Files
setwd("C:\\Users\\User\\Dropbox\\temp\\Hackathons\\AV\\Loanprediction3\\data")
train<-read.csv("train.csv",header = T)
str(train)
sum(is.na(train))
################
#Imputing missing values using median
preProcValues <- preProcess(train, method = c("medianImpute","center","scale"))
library('RANN')
data_processed <- predict(preProcValues, train)
sum(is.na(data_processed))

# Splitting data into two parts
#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(data_processed$Loan_Status, p=0.75, list=FALSE)
trainSet <- data_processed[ index,]
testSet <- data_processed[-index,]


#Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

#Defining the predictors and outcome
predictors<-c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome",
              "CoapplicantIncome")
outcomeName<-'Loan_Status'
######## Building Model


##################################################################################################
#Training the random forest model
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)

#Predicting using random forest model
testSet$pred_rf<-predict(object = model_rf,testSet[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet$Loan_Status,testSet$pred_rf)


###################################################################################################
#Training the knn model
model_knn<-train(trainSet[,predictors],trainSet[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)

#Predicting using knn model
testSet$pred_knn<-predict(object = model_knn,testSet[,predictors])

#Checking the accuracy of the  KNN model
confusionMatrix(testSet$Loan_Status,testSet$pred_knn)

##################################################################################################

# Prediction
test<-read.csv("test.csv",header = T)
preProcValues_test <- preProcess(test, method = c("medianImpute","center","scale"))
data_processed_test <- predict(preProcValues_test, test)
sum(is.na(data_processed_test))

# Random Forest
data_processed_test$pred_rf<-predict(object = model_rf,data_processed_test[,predictors])
Submission<-data.frame(Loan_ID=data_processed_test$Loan_ID,Loan_Status=data_processed_test$pred_rf,stringsAsFactors=FALSE)
summary(Submission)
write.csv(Submission,file="Submission.csv",row.names = F)

# KNN Prediction
data_processed_test$pred_knn<-predict(object = model_knn,data_processed_test[,predictors])
Submission<-data.frame(Loan_ID=data_processed_test$Loan_ID,Loan_Status=data_processed_test$pred_knn,stringsAsFactors=FALSE)
summary(Submission)
write.csv(Submission,file="Submission.csv",row.names = F)



