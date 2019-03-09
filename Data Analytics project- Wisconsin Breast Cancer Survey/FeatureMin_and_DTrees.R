library(readr)

Cancer_data <- read_csv("E:/5thsemcse/DA/project/train2-imp5.csv")

View(Cancer_data)

# Checking the dimension of original dataset

dim(Cancer_data)

sum(is.na(Cancer_data))
#checking if there are any NA values 

# Remove columns with lowest variance after normalization of data 
# Normlized values of all columns
ndf <- Cancer_data
nam <- colnames(ndf)
nam
nam <- nam[4:33]
as.vector(nam)

variances = c()
for(x in nam)
{
  ndf[x] <- (ndf[x] - min(ndf[x]))/(max(ndf[x]) - min(ndf[x]))
  variances[x] <- var(ndf[x])
}
# Sorting the variances
var_ord <- sort(variances)
var_ord

# Removing columns with var less than 0.015
can_drop <- names(var_ord[var_ord < 0.015])
# The variables that can be dropped are: 
can_drop
# Remove 3 columns , as they dont contribute much to the overall variability
# Concavity_se, smoothness_se, fractional_dimension_se


library(corrgram)
library(corrplot)

# Lets remove columns based on correlation
corrgram(Cancer_data[4:33],order = TRUE, lower.panel = panel.shade ,upper.panel = panel.pie ,text.panel = panel.txt, main = "Corrgram - Scatter plots")
# The variables corresponding to pie charts with most shaded areas are correlated

# We will use a corrplot to check the correlation oefficients

abc <- cor(Cancer_data[4:33])
corrplot(abc , method = "circle" , type = "upper")
#corrplot(M , method = "number")

# Getting the correlation coefficients
cor <- cor(Cancer_data[4:33])
m <- data.frame(cor)

index <- 1
firstvar <- c()
secondvar <- c()
#Selecting the attributes which are highly correlated
for(i in 1:nrow(m))
{
  for(j in 1:ncol(m))
  {
    if(i!=j)
    {
      r <- m[i,j]
      if(abs(r) > 0.90)
      {
        firstvar[index] <- j
        secondvar[index] <- i
        index <- index + 1
      }
    }
  }
}

firstvar
secondvar
n <- unique(firstvar)
names(m[,n])

drop_att <- unique(c(names(m[,n]),can_drop))
drop_att

can_keep<-c()
k <- 1
for(i in colnames(m)){
  if(!(i %in% drop_att))
  {
    can_keep[k] <- i
    k <- k + 1
  }
}
can_keep
M <- cor(Cancer_data[,can_keep])
corrplot(M , method = "circle" , type = "upper")


#----------------classification model #1-------------------


apply(Cancer_data,2,function(x) sum(is.na(x)))

#M = 1
#B = 0
# rpart for "Recursive Partitioning and Regression Trees" and uses the CART 
# decision tree algorithm
library(rpart)
library(RColorBrewer)
library(rattle)
library(caret)
library(rpart.plot)
can_keep


#-----------------------Start from here-------------------------------
#The parameters taken here have a correlation limit of 0.80 with each other
#And their indiviual variation should be greater than 0.015
fit1 <- rpart(diagnosis ~ compactness_mean+concavity_mean+symmetry_mean+fractal_dimension_mean +texture_se +compactness_se+concave.points_se+symmetry_se+smoothness_worst+compactness_worst+concavity_worst+symmetry_worst+ fractal_dimension_worst,
              data=Cancer_data,
              method="class"
)

fancyRpartPlot(fit1)

names(test)[20] <- 'concave.points_se'

prediction <- predict(fit1, test[,can_keep], type = "class")
test$prediction <- prediction
table_pred <- table(Predict = prediction,Actual = test$diagnosis)
plot(prediction)


table_pred
confusionMatrix(table_pred)


# overly complex decisions being made, and kill the nodes that appear to go to far.

#-------------------Using K-Fold validation-----------------------

 
# “k- fold cross validation”. steps:
#   
#   Randomly split your entire dataset into k”folds”.
# For each k folds in your dataset, build your model on k – 1 folds of 
# the data set. Then, test the model to check the effectiveness for kth fold.
# Record the error you see on each of the predictions.
# Repeat this until each of the k folds has served as the test set.
# The average of your k recorded errors is called the cross-validation error 
# and will serve as your performance metric for the model.


#Using caret to perform K-fold on the rpart tree 

set.seed(11)
train_control<- trainControl(method="cv", number=10,verboseIter = TRUE ,savePredictions = TRUE)

model1 <- train(diagnosis ~ compactness_mean+concavity_mean+symmetry_mean+fractal_dimension_mean +texture_se +compactness_se+concave.points_se+symmetry_se+smoothness_worst+compactness_worst+concavity_worst+symmetry_worst+ fractal_dimension_worst,
              data=Cancer_data, 
              trControl=train_control, 
              method="rpart")


model1$results

#         cp   Accuracy  Kappa     AccuracySD KappaSD
# 1 0.02352941 0.8598718 0.6833257 0.03426125 0.07589933
# 2 0.04705882 0.8753974 0.7267778 0.04549179 0.09752670
# 3 0.69411765 0.7933846 0.4697404 0.09222423 0.32721156
print(model1)
plot(model1 , main = "Cross Validation Accuracy")
fancyRpartPlot(model1$finalModel)

# CART 
# 250 samples
#
# 13 predictor
# 2 classes: 'B', 'M' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 226, 226, 225, 224, 226, 225, ... 
# Resampling results across tuning parameters:
#   
#   cp          Accuracy   Kappa    
# 0.02352941  0.8598718  0.6833257
# 0.04705882  0.8753974  0.7267778
# 0.69411765  0.7933846  0.4697404
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was cp = 0.04705882.

#using best model of CV to predict

predictions <- predict(model1 , test)

confusionMatrix(table(Predict = predictions , Actual = test$diagnosis))

