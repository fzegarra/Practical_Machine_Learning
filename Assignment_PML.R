# Libraries for processing data
library(caret)
library(randomForest)
# Create repository directory
setwd("D:/DataScience/Practical_Machine_Learning/data")
# Reading the data
training_source<- read.csv("pml-training.csv", na.strings=c("", "NA", "#DIV/0!"))
testing_source<- read.csv("pml-testing.csv", na.strings=c("", "NA", "#DIV/0!"))
# Data processing 
# i. Cleaning data
training_tidy<- training_source[, colSums(is.na(training_source)) == 0] 
testing_tidy<-testing_source[, colSums(is.na(testing_source)) == 0] 
# ii. Cleaning data
training<-training_tidy[, -c(1:7)] #columns with descriptive data only 
testing<- testing_tidy[, -c(1:7, 60)] #columns with descriptive data only
# Data slicing
inTrain<-createDataPartition(y=training$classe, p=0.75, list=FALSE)
trainset<-training[inTrain,] 
testset<-training[-inTrain,] 
set.seed(2613) #setting the sed 
ctrl<-trainControl(method="cv", number=5, allowParallel=TRUE, verbose=T)
modelfit<-train(classe~.,data=trainset, method="rf", trControl=ctrl, verbose=F)
predict<-predict(modelfit, newdata=testset)
confmatrix<-confusionMatrix(table(predict, testset$classe))
results<-predict(modelfit, newdata=testing)
print(results)
