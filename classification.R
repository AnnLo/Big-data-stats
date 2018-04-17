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
##### Evaluate performance of different classification methods on 2-3 test sets ####

# 1, iris
data(iris)
set.seed(1)
# Separate in test and traning set
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
#removing factorvariable from training and test datasets
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
    iris_test_pred1 <- knn(train = trainData1, test = testData1, cl= iris_train_labels,k = 3,prob=TRUE)

    CrossTable(x = iris_test_labels, y = iris_test_pred1,prop.chisq=FALSE)
    break
    }else if(method == "rfs"){
    ### Random Forest ###
    iris_pred.rf <- rfsrc(Species~., data=trainData ,statistics=T,tree.err=T,proximity=T,importance=T)
    print(iris_pred.rf)
    break
    }else if(method == "cart"){
    ### CART ###
    ii<-sample(seq(1,dim(iris)[1]),50)
    #training data - subsample of patients. Try different training data sizes here and see what happens.
    tree1<-rpart(Species~.,data=iris,subset=ii,control=rpart.control(cp=0.01, minsplit=5,xval=10))
    prp(tree1,extra=100)
    # plotting the full tree
    plot(tree1)
    text(tree1,cex=.75,digits=2)
    break
    }else if(method == "multinom"){
    ### Multinomail model ###
    model <-multinom(Species~., data=trainData)
    pr<-predict(model, newdata = testData)
    confusionMatrix(data=pr, reference = testData$Species)
    break
    }else if(method == "lda"){
    ### Discriminant analysis ###
    # https://rpubs.com/gokul108/19670
    ### Linear ###
    lda(Species ~ ., data=iris) -> irisLDA
    # Confusion matrix: rows = actual, columns = classified into
    table( Actual=(actual<-iris$Species),
       Classified=(classified<-predict(irisLDA)$class) )
    break
    }else if(method == "quad"){
    ### Quadratic ###
    ### Start Quadratic discriminant analysis (from MASS library)
    qda(Species ~ ., data=iris) -> irisQDA
    # Confusion matrix: rows = actual, columns = classified into
    table(actual<-iris$Species, classified<-predict(irisQDA)$class)
    break
    }
}
