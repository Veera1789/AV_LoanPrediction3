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
#Imputing missing values using median for training data
preProcValues <- preProcess(train, method = c("medianImpute","center","scale"))
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
#Training the Logistic regression model
model_lr<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)

#Predicting using Log reg model
testSet$pred_lr<-predict(object = model_lr,testSet[,predictors])

#Checking the accuracy of the lr model
confusionMatrix(testSet$Loan_Status,testSet$pred_lr)
#################################################################################################
# imputting missing values
test<-read.csv("test.csv",header = T)
preProcValues_test <- preProcess(test, method = c("medianImpute","center","scale"))
data_processed_test <- predict(preProcValues_test, test)
sum(is.na(data_processed_test))

# Predicting Probability 
data_processed_test$pred_rf_prob<-predict(object = model_rf,data_processed_test[,predictors],type='prob')
data_processed_test$pred_knn_prob<-predict(object = model_knn,data_processed_test[,predictors],type='prob')
data_processed_test$pred_lr_prob<-predict(object = model_lr,data_processed_test[,predictors],type='prob')

#Taking average of predictions
data_processed_test$pred_avg<-(data_processed_test$pred_rf_prob$Y+data_processed_test$pred_knn_prob$Y+data_processed_test$pred_lr_prob$Y)/3

#Splitting into binary classes at 0.5
data_processed_test$pred_avg<-as.factor(ifelse(data_processed_test$pred_avg>0.5,'Y','N'))

#Writing Files
Submission<-data.frame(Loan_ID=data_processed_test$Loan_ID,Loan_Status=data_processed_test$pred_avg,stringsAsFactors=FALSE)
summary(Submission)
write.csv(Submission,file="Submission.csv",row.names = F)
