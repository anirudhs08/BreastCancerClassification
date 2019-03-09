library(readr)
train1 <- read_csv("C:/abhilash/5th sem/Data Analytics/datasets/breast-cancer-wisconsin-data/train1.csv")
View(train1)

train <- train1[sample(nrow(train1),250),]
write.csv(train,'train.csv')
