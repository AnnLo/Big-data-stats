library(randomForestSRC)
library(ggplot2)
library(class)
library(ggRandomForests)
library(plot3D)
library(dplyr)
library(parallel)
library(data.table)
library(RColorBrewer)
library(GGally)
library(ElemStatLearn)
library(rpart)
library(rpart.plot)
library(gmodels)
library(ggRandomForests)
library(nnet)
library(caret)
library(MASS)
library(e1071)
library(randomForest)
library(foreach)
##### Evaluate performance of different classification methods on 2-3 test sets ####

# 1, iris
data(iris)
set.seed(1)
# Separate in test and traning set
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
#removing factorvariable from training and test datase
trainData1 <- trainData[-5]
testData1 <- testData[-5]

iris_train_labels <- trainData$Species
iris_test_labels <- testData$Species

# User inputs method
while(TRUE){
    cat("Enter method: ");
    method <- readLines("stdin",n=1);
    if(method == "knn"){
    #### KNN ####
    model <- knn(train = trainData1, test = testData1, cl= iris_train_labels,k = 3,prob=TRUE)

    #CrossTable(x = iris_test_labels, y = model,prop.chisq=FALSE)
    confMat <- confusionMatrix(data=model, reference = testData$Species)
    print(confMat)
    #library(ggplot2)
    break
    }else if(method == "rfs"){
    ### Random Forest ###
    model <- rfsrc(Species~., data=trainData ,statistics=T,tree.err=T,proximity=T,importance=T)
    confMat <- confusionMatrix(data=pr, reference = testData$Species)
    print(confMat)
    plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])
    break
    }else if(method == "cart"){
    ### CART ###
    ii<-sample(seq(1,dim(iris)[1]),50)
    #training data - subsample of patients. Try different training data sizes here and see what happens.
    model<-rpart(Species~.,data=iris,subset=ii,control=rpart.control(cp=0.01, minsplit=5,xval=10))
    confMat <- confusionMatrix(data=pr, reference = testData$Species)
    print(confMat)
    prp(model,extra=100)
    # plotting the full tree
    plot(model)
    text(model,cex=.75,digits=2)
    break
    }else if(method == "multinom"){
    ### Multinomail model ###
    model <-multinom(Species~., data=trainData)
    pr<-predict(model, newdata = testData)
    confMat <- confusionMatrix(data=pr, reference = testData$Species)
    print(confMat)
    break
    }else if(method == "lda"){
    ### Discriminant analysis ###
    # https://rpubs.com/gokul108/19670
    ### Linear ###
    lda(Species ~ ., data=iris) -> model
    # Confusion matrix: rows = actual, columns = classified into
    table( Actual=(actual<-iris$Species),
       Classified=(classified<-predict(model)$class) )
    plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])
    break
    }else if(method == "quad"){
    ### Quadratic ###
    ### Start Quadratic discriminant analysis (from MASS library)
    qda(Species ~ ., data=iris) -> model
    # Confusion matrix: rows = actual, columns = classified into
    table(actual<-iris$Species, classified<-predict(model)$class)
    plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])
    break
    }else if(method == "svm"){
    model <- svm(Species~.,data=trainData)
    svm_predictions<-predict(model,newdata=testData)
    confMat<-confusionMatrix(data=svm_predictions, reference = testData$Species)
    print(confMat)
    plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])
    break
    }else if(method == "ensemble"){
    #https://www.r-bloggers.com/using-support-vector-machines-as-flower-finders-name-that-iris/
    # http://www.vikparuchuri.com/blog/intro-to-ensemble-learning-in-r/
    # https://machinelearningmastery.com/machine-learning-ensembles-with-r/
    # Bagging. To compare with, randomForest already does bagging.
    # Convert to numeric
    rf_fit<-randomForest(Species~.,data=trainData,ntree=500)
    rf_predictions<-predict(rf_fit,newdata=testData)
    print(rf_predictions)
    print(testData$Species)
    rf_predictions <- as.numeric(rf_predictions)
    testData$Species <- as.numeric(testData$Species)
    error<-sqrt((sum((testData$Species-rf_predictions)^2))/nrow(testData))
    print(error)

    length_divisor<-6
    iterations<-5000
    predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
    training_positions <- sample(nrow(trainData), size=floor((nrow(trainData)/length_divisor)))
    train_pos<-1:nrow(trainData) %in% training_positions
    lm_fit<-lm(Species~.,data=trainData[train_pos,])
    predict(lm_fit,newdata=testData)
    }
    lm_predictions<-rowMeans(predictions)
    error_lm <- sqrt((sum((testData$Species-lm_predictions)^2))/nrow(testData))
    print(error_lm)


    length_divisor<-6
    iterations<-5000
    predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
    training_positions <- sample(nrow(trainData), size=floor((nrow(trainData)/length_divisor)))
    train_pos<-1:nrow(trainData) %in% training_positions
    svm_fit<-svm(Species~.,data=trainData[train_pos,])
    predict(svm_fit,newdata=testData)
    }
    svm_predictions<-rowMeans(predictions)

    error_svm <- sqrt((sum((testData$Species-svm_predictions)^2))/nrow(testData))
    predictions_bag2 <-(svm_predictions+lm_predictions)/2
    predictions_bag1<-(svm_predictions+rf_predictions)/2
    error_bag1<-sqrt((sum((testData$Species-predictions_bag1)^2))/nrow(testData))
    error_bag2<-sqrt((sum((testData$Species-predictions_bag2)^2))/nrow(testData))
    print(error_bag1)
    print(error_bag2)
    print(error_svm)
    break

    }
}
