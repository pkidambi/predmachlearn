setwd("C:\\Users\\CECS-INT\\Desktop\\Coursera\\16.PracticalMachineLearning\\Project\\")
library(xlsx)
library(caret)
library(kernlab)
library(dplyr)
library(data.table)
library(rattle)
options(warn=-1)
set.seed(1016)
# read all the dataset 
testing <- read.csv("pml-testing.csv")
training<- read.csv("pml-training.csv")
t1 <- training[,which(names(testing) %like% c("belt") | names(testing) %like% c("arm")| names(testing) %like% c("dumbbell"))]
                      
t2 <- t1[,- which(names(t1) %like% c("max")| names(t1) %like% c("min")| names(t1) %like% c("amplitude")| names(t1) %like% c("var")| names(t1) %like% c("avg")| names(t1) %like% c("stdev")| names(t1) %like% c("skewness")| names(t1) %like% c("kurtosis")| names(t1) %like% c("stddev"))] 
# names(t1)[!(names(t1) %in% names(t2)) ]
t3 <- data.frame(t2,training$classe)

intrain <- createDataPartition(t3$training.classe,p=0.75,list = FALSE)
train_d <- t3[intrain,]
train_d <- as.data.frame(train_d)
valid_d <- t3[-intrain,]
# train_prep <- preProcess(train_d[,-14], method = c("center","scale"))
# valid_prep <- preProcess(valid_d[,-14], method = c("center","scale"))

#modelFit <- train(train_d$training.classe ~., data = as.data.frame(train_d), method = "glm",preProcess(c("center","scale")) )
modelFit <- train(train_d$training.classe ~., data = as.data.frame(train_d), method = "rf" )
modelFit$finalModel

val <- predict(modelFit,valid_d)
confusionMatrix(val, valid_d$training.classe)
val <- data.frame(val)
valid_d_classe <- data.frame(valid_d$training.classe)

sim_val <- ifelse(val$val==valid_d_classe$valid_d.training.classe,1,0)

# 35 incorrect, 4869 correct (99.2% correct)
sum(sim_val)
hist(sim_val)



#modelFit <- train(new$training.classe ~., data = as.data.frame(new), method = "glm" )


#modelFit <- train(train_d$training.classe ~., data = as.data.frame(train_d), method = "glm",preProcess(c("center","scale")) )