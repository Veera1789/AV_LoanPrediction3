# Loan Prediction 3 Hackathon - Analytics vidhya - file 1
# V9 Base Log Regression, Var imp, EDA, Feat engg - 

library(caret)
library(mice)
library(gmodels)
library(dummies)

#### Reading Files
setwd("C:\\Users\\User\\Google Drive\\Personal_Veera\\Hackathons\\av\\Loanprediction3")
train75<-read.csv("train75.csv",header = T)
train75$Credit_History<-as.factor(train75$Credit_History)
# Removing Loan Ids from the training set
train75<-train75[,-13]
test25<-read.csv("test25.csv",header = T)
test25$Credit_History<-as.factor(test25$Credit_History)
################
ch<-dummy(train75$Credit_History,sep="_")
pa<-dummy(train75$Property_Area,sep="_")
Mar<-dummy(train75$Married,sep="_")
edn<-dummy(train75$Education,sep="_")
emp<-dummy(train75$Self_Employed,sep="_")
traindata<-cbind(data.frame(ch),data.frame(pa),inc_loan_ratio=train75$income_loanrati,overall_income=train75$Overallincome,data.frame(Mar),term=train75$Loan_Amount_Term,data.frame(edn),data.frame(emp),status=train75$Loan_Status)



#########
ch<-dummy(test$Credit_History,sep="_")
pa<-dummy(test$Property_Area,sep="_")
Mar<-dummy(test$Married,sep="_")
edn<-dummy(test$Education,sep="_")
emp<-dummy(test$Self_Employed,sep="_")
testdata<-cbind(data.frame(ch),data.frame(pa),inc_loan_ratio=test$income_loanratio,overall_income=test$Overallincome,data.frame(Mar),term=test$Loan_Amount_Term,data.frame(edn),data.frame(emp))
length(testdata$Credit_History_0)





ch=NULL
pa=NULL
Mar=NULL
edn=NULL
emp=NULL



################

######## Buildling Model

model1<-train(status~.,data=traindata,method="glm",family = "binomial")
#model1<-train(Loan_Status~.,data=train,method="glm",family = "binomial")
summary(model1)
varImp(model1)
pred<-predict(model1,test25)
CrossTable(pred,test25$Loan_Status,prop.r = F,prop.c = T,digits = 2,prop.chisq = F)


# Prediction
test<-read.csv("test.csv",header = T)
test$Credit_History<-as.factor(test$Credit_History)
output<-predict(model1,test)
Submission<-data.frame(Loan_ID=test$Loan_ID,Loan_Status=output,stringsAsFactors=FALSE)
summary(Submission)
write.csv(Submission,file="Submission.csv",row.names = F)



