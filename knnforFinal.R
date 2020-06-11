
library(class)
library(dplyr)
library(ggplot2)

if(!(packageName%in%rownames(installed.packages()))){
  install.packages(packageName)
}

setwd("G:/RCode")

#測試模型,可隨機產生(訓練資料,測試資料)

traindata=read.csv("SPECTF_test.csv")
testdata=read.csv("SPECTF_train.csv")

#去除第一欄PK不適合分岔屬性

trainLabels <- traindata$status

dim(trainLabels)

cl = trainLabels

kv <- round(sqrt(length(knnTrain)))

prediction <- knn(train = knnTrain, test = knnTest, cl, k=kv)
dim(train_points)
dim(test_points)
length(cl)

cm <- table(testdata$status,prediction,dnn=c("實際","預測"))
print(cm)

if(length(cm)==4){
  nycolName <- colnames(cm)
  mycolName[1]
  
  precision <- (cm[[1]]/sum(cm[,1]))
  print(paste("預測",mycolName[1],"的正確率,precision=",round(precision,2)))
  
  TPR <- (cm[[1]]/sum(cm[1,]))
  print(paste("預測",mycolName[1],"的正確率,TPR=",round(TPR,2)))
  
  TNR <- (cm[[4]]/sum(cm[2,]))
  print(paste("預測",mycolName[1],"的正確率,TNR=",round(TNR,2)))
  
  precision <- (cm[[4]]/sum(cm[,2]))
  print(paste("預測",mycolName[2],"的正確率,precision=",round(precision,2)))
  
  TPR <- (cm[[4]]/sum(cm[2,]))
  print(paste("預測",mycolName[2],"的正確率,TPR=",round(TPR,2)))
  
  TNR <- (cm[1]/sum(cm[1,]))
  print(paste("預測",mycolName[2],"的正確率,TNR=",round(TNR,2)))
  
  accuracy <- sum(diag(cm))/sum(cm)
  print(paste("整體準確率 accuracy=",round(accuracy,2)))
}else if(length(cm)==2){
  cm
  precision <- (cm[[2]]/sum(cm[,1]))
  print(paste("預測",mycolName[2],"的正確率,precision=",round(precision,2)))
  
  TPR <- (cm[[2]]/sum(cm[2,]))
  print(paste("預測",mycolName[2],"的正確率,TPR=",round(TPR,2)))
  
  accuracy <- sum(cm[[2]]/sum(cm[,1]))
  print(paste("整體準確率 accuracy=",round(accuracy,2)))
  cm
}

Real <- as.factor(ifelse(testdata$status==1,"Y","N"))
Pred <- as.factor(ifelse(prediction==1,"Y","N"))

cmKNN <- table(Real=Real,Pred=Pred) 
print(cmKNN)
cmKNN.factor <- table(factor(Real,ordered=TRUE,levels=c("Y","N")),factor(Pred,ordered=TRUE,levels=c("Y","N")),dnn=c("Real","Pred"))
print(cmKNN.factor)


if(length(cmKNN.factor)==4){
  mycolName <- colnames(cmKNN.factor)
  mycolName[1]
  
  precision <- (cmKNN.factor[[1]]/sum(cmKNN.factor[,1]))
  print(paste("預測",mycolName[1],"的正確率,precision=",round(precision,2)))
  
  TPR <- (cmKNN.factor[[1]]/sum(cmKNN.factor[1,]))
  print(paste("預測",mycolName[1],"的正確率,TPR=",round(TPR,2)))
  
  TNR <- (cmKNN.factor[[4]]/sum(cmKNN.factor[2,]))
  print(paste("預測",mycolName[1],"的正確率,TNR=",round(TNR,2)))
  
  precision <- (cmKNN.factor[[4]]/sum(cmKNN.factor[,2]))
  print(paste("預測",mycolName[2],"的正確率,precision=",round(precision,2)))
  
  TPR <- (cmKNN.factor[[4]]/sum(cmKNN.factor[2,]))
  print(paste("預測",mycolName[2],"的正確率,TPR=",round(TPR,2)))
  
  TNR <- (cmKNN.factor[1]/sum(cmKNN.factor[1,]))
  print(paste("預測",mycolName[2],"的正確率,TNR=",round(TNR,2)))
  
  accuracy <- sum(diag(cmKNN.factor))/sum(cmKNN.factor)
  print(paste("整體準確率=",round(accuracy,2)))
}else if(length(cmKNN.factor)==2){
  
  precision <- (cmKNN.factor[[2]]/sum(cmKNN.factor[,1]))
  paste("預測",mycolName[2],"的正確率,precision=",round(precision))
  
  TPR <- (cmKNN.factor[[2]]/sum(cmKNN.factor[2,]))
  print(paste("預測",mycolName[2],"的正確率,TPR=",round(TPR)))
  
  accuracy <- sum(diag(cmKNN.factor)[[2]])/sum(cmKNN.factor[,1])
  print(paste("整體準確率=",round(accuracy,2)))
}
print(cmKNN.factor)

klist <- seq(1:(kv+kv))
knnFunction <- function(x,knnTrain,knnTest,trainLabels,testLabels){
  prediction <- knn(train = knnTrain,test=knnTest,cl=trainLabels,k=x)
  cm <- table(x=testLabels,y=prediction)
  accuracy <- sum(diag(cm))/sum(cm)
}
accuracies <- sapply(klist,knnFunction,knnTrain=knnTrain,knnTest=knnTest,trainLabels=trainLabels,testLabels=testdata$status)
df <- data.frame(kv=klist,accuracy=accuracies)


ggplot(df,aes(x=kv,y=accuracy,label=kv,color=accuracy))+geom_point(size=5)+geom_text(vjust=2)

