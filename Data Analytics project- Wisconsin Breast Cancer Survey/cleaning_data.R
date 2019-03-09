library(readr)
train <- read_csv("C:/abhilash/5th sem/Data Analytics/datasets/breast-cancer-wisconsin-data/train.csv", 
                  na = "0")
View(train)

#Converting the 0 values to NA 
write.csv(train,'train2.csv')
library(readr)
train2 <- read_csv("C:/abhilash/5th sem/Data Analytics/datasets/breast-cancer-wisconsin-data/train2.csv")

#Deleting the extra column
train2[,2]<-NULL
View(train2)
write.csv(train,'train3.csv')

#Imputed unknown values with Amelia package
library("Amelia", lib.loc="~/R/win-library/3.4")
 
