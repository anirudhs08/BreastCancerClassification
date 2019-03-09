library(readr)
train4 <- read_csv("C:/abhilash/5th sem/Data Analytics/datasets/breast-cancer-wisconsin-data/train4.csv")
View(train4)

#Training file
#Making diagnosis into 0 or 1
#0<- B
#1<- M

train4$diagnosis2<-0
View(train4)
for(x in c(1:nrow(train4))){
  if(train4[x,"diagnosis"]=="M"){
    train4[x,"diagnosis2"]<-1
  }
}

#Creating a test file

library(readr)
test <- read_csv("C:/abhilash/5th sem/Data Analytics/datasets/breast-cancer-wisconsin-data/test.csv")
View(test)

#colnames(test)[9]<-"concave.points_mean"
#colnames(test)[19]<-"concave.points_se"
#colnames(test)[29]<-"concave.points_worst"

#Test file 
#Making diagnosis into 0 or 1
#0<- B
#1<- M

test$diagnosis2<-0
for(x in c(1:nrow(test))){
  if(test[x,"diagnosis"]=="M"){
    test[x,"diagnosis2"]<-1
  }
}

test$diagnosis<-NULL
train4$diagnosis<-NULL

#upsampling
library(caret)
up_df <- upSample(x = train4[, -ncol(train4)],y = factor(train4$diagnosis2))
up_df <- as.data.frame(up_df)
b <- 0
e <- 0
for(x in c(1:nrow(up_df))){
  if(up_df[x,"Class"]==1){
    b <- b+1
  }
  if(up_df[x,"Class"]==0){
    e <- e+1
  }
}
k <- c(b, e)
lbls <- c("Malignant", "Benign")
pie(k, labels = lbls, main="Pie Chart of Characters")



# Model fitting

train4$diagnosis2<-as.factor(train4$diagnosis2)
library(stats)
model <- glm(diagnosis2 ~ .,family=binomial(link='logit'),data=train4)
summary(model)

View(test)
#test$diagnosis2<-NULL
#test$diagnosis<-NULL

#Bayesian Classification

library(e1071)
model1 <- naiveBayes(as.factor(diagnosis2) ~ ., data = train4)
plot(model1)
pred<-predict(model1,test)
class(model1)
summary(model1)
print(model1)
#str(pred)
print(pred)
print(test$diagnosis2)
nrow(test)
nrow(model)

#Graphs

mK <- NaiveBayes(diagnosis2 ~ ., data = train4, usekernel = TRUE)
plot(mK)

#Confusion Matrix 

library(caret)
confusionMatrix(pred,as.factor(test$diagnosis2))


#10 folds 3 repeatation
# load the library
library(caret)
# define training control
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
View(train4)
model3 <- train(diagnosis2~., data=train4, trControl=train_control, method="nb")
# summarize results
print(model3)


