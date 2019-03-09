
#################################### SVM ##############################





#################C type linear classification###########################
##
train1$diagnosis1 <- ifelse(train1$diagnosis=="M",1,0)

train1$diagnosis <- NULL
tuned <- tune(svm, diagnosis1~., data=train1, kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,10,100)))
summary(tuned)

train1$diagnosis <- ifelse(train1$diagnosis1==1,"M","B")
train1$diagnosis1 <- NULL
##
svmfit<-svm(diagnosis~., data=train1, kernel="linear", cost=.1, scale=FALSE,
            type="C-classification")
print(svmfit)
##
p<- predict(svmfit, test1, type="class")
plot(p)
##
library(caret)
table_pred <- table(p, test1$diagnosis)
confusionMatrix(table_pred)
##################c type polynomial classification###########################
##
train1$diagnosis1 <- ifelse(train1$diagnosis=="M",1,0)

train1$diagnosis <- NULL
tuned <- tune(svm, diagnosis1~., data=train1, kernel="polynomial",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,10,100)))
summary(tuned)

train1$diagnosis <- ifelse(train1$diagnosis1==1,"M","B")
train1$diagnosis1 <- NULL
##
svmfit<-svm(diagnosis~., data=train1, kernel="polynomial", cost=.01, scale=FALSE,
            type="C-classification")
print(svmfit)
##
p<- predict(svmfit, test1, type="class")
plot(p)
##
library(caret)
table_pred <- table(p, test1$diagnosis)
confusionMatrix(table_pred)
##################c type sigmoid classification###########################
##
train1$diagnosis1 <- ifelse(train1$diagnosis=="M",1,0)

train1$diagnosis <- NULL
tuned <- tune(svm, diagnosis1~., data=train1, kernel="sigmoid",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,10,100)))
summary(tuned)

train1$diagnosis <- ifelse(train1$diagnosis1==1,"M","B")
train1$diagnosis1 <- NULL
##
svmfit<-svm(diagnosis~., data=train1, kernel="sigmoid", cost=.1, scale=FALSE,
            type="C-classification")
print(svmfit)
##
p<- predict(svmfit, test1, type="class")
plot(p)
##
library(caret)
table_pred <- table(p, test1$diagnosis)
confusionMatrix(table_pred)

##################nu type linear classification###########################
##
train1$diagnosis1 <- ifelse(train1$diagnosis=="M",1,0)

train1$diagnosis <- NULL
tuned <- tune(svm, diagnosis1~., data=train1, kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,10,100)))
summary(tuned)

train1$diagnosis <- ifelse(train1$diagnosis1==1,"M","B")
train1$diagnosis1 <- NULL
##
svmfit<-svm(diagnosis~., data=train1, kernel="linear", cost=.1, scale=FALSE,
            type="nu-classification")
print(svmfit)
##
p<- predict(svmfit, test1, type="class")
plot(p)
##
library(caret)
table_pred <- table(p, test1$diagnosis)
confusionMatrix(table_pred)

##################nu type polynomial classification###########################
##
train1$diagnosis1 <- ifelse(train1$diagnosis=="M",1,0)

train1$diagnosis <- NULL
tuned <- tune(svm, diagnosis1~., data=train1, kernel="polynomial",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,10,100)))
summary(tuned)

train1$diagnosis <- ifelse(train1$diagnosis1==1,"M","B")
train1$diagnosis1 <- NULL
##
svmfit<-svm(diagnosis~., data=train1, kernel="polynomial", cost=.01, scale=FALSE,
            type="nu-classification")
print(svmfit)
##
p<- predict(svmfit, test1, type="class")
plot(p)
##
library(caret)
table_pred <- table(p, test1$diagnosis)
confusionMatrix(table_pred)

##################nu type sigmoid classification###########################
##
train1$diagnosis1 <- ifelse(train1$diagnosis=="M",1,0)

train1$diagnosis <- NULL
tuned <- tune(svm, diagnosis1~., data=train1, kernel="sigmoid",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,10,100)))
summary(tuned)

train1$diagnosis <- ifelse(train1$diagnosis1==1,"M","B")
train1$diagnosis1 <- NULL
##
svmfit<-svm(diagnosis~., data=train1, kernel="sigmoid", cost=.1, scale=FALSE,
            type="nu-classification")
print(svmfit)
###gives an error