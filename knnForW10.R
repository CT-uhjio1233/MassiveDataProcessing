
packageName="dplyr"

if(!(packageName %in% rownames(installed.packages()))){
  install.packages(packageName)
}

library(class)
library(dplyr)

setwd("D:/r place")

#代刚家,繦诀玻ネ(癡絤戈,代刚戈)
traindata=read.csv("Parkinsons_TestANN.csv")
testdata=read.csv("Parkinsons_TestANN.csv")

#埃材逆PKぃ続だ妮┦
testdata <- testdata[-c(1)]
traindata <- traindata[-c(1)]

trainLabels <- traindata$status

knnTrain <- traindata[,-c(24)]
knnTest <- testdata[,-c(24)]

kv <- round(sqrt(length(knnTrain)))

prediction <- knn(train = knnTrain, test = knnTest, cl = trainLabels, k=kv)


cm <- table(testdata$status,prediction,dnn=c("龟悔","箇代"))
print(cm)

if(length(cm)==4){
  mycolName <- colnames(cm)
  mycolName[1]
  
  precision <- (cm[[1]]/sum(cm[,1]))
  print(paste("箇代",mycolName[1],"タ絋瞯,precision=",round(precision,2)))
  
  TPR <- (cm[[1]]/sum(cm[1,]))
  print(paste("箇代",mycolName[1],"タ絋瞯,TPR=",round(TPR,2)))
  
  TNR <- (cm[[4]]/sum(cm[2,]))
  print(paste("箇代",mycolName[1],"タ絋瞯,TNR=",round(TNR,2)))
  
  precision <- (cm[[4]]/sum(cm[,2]))
  print(paste("箇代",mycolName[2],"タ絋瞯,precision=",round(precision,2)))
  
  TPR <- (cm[[4]]/sum(cm[2,]))
  print(paste("箇代",mycolName[2],"タ絋瞯,TPR=",round(TPR,2)))
  
  TNR <- (cm[1]/sum(cm[1,]))
  print(paste("箇代",mycolName[2],"タ絋瞯,TNR=",round(TNR,2)))
  
  accuracy <- sum(diag(cm))/sum(cm)
  print(paste("俱砰非絋瞯 accuracy=",round(accuracy,2)))
}else if(length(cm)==2){
  
  precision <- (cm[[2]]/sum(cm[,1]))
  print(paste("箇代",mycolName[2],"タ絋瞯,precision=",round(precision,2)))
  
  TPR <- (cm[[2]]/sum(cm[2,]))
  print(paste("箇代",mycolName[2],"タ絋瞯,TPR=",round(TPR,2)))
  
  accuracy <- (cm[[2]]/sum(cm[,1]))
  print(paste("俱砰非絋瞯 accuracy=",round(accuracy,2)))
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
  print(paste("箇代",mycolName[1],"タ絋瞯,precision=",round(precision,2)))
  
  TPR <- (cmKNN.factor[[1]]/sum(cmKNN.factor[1,]))
  print(paste("箇代",mycolName[1],"タ絋瞯,TPR=",round(TPR,2)))
  
  TNR <- (cmKNN.factor[[4]]/sum(cmKNN.factor[2,]))
  print(paste("箇代",mycolName[1],"タ絋瞯,TNR=",round(TNR,2)))
  
  precision <- (cmKNN.factor[[4]]/sum(cmKNN.factor[,2]))
  print(paste("箇代",mycolName[2],"タ絋瞯,precision=",round(precision,2)))
  
  TPR <- (cmKNN.factor[[4]]/sum(cmKNN.factor[2,]))
  print(paste("箇代",mycolName[2],"タ絋瞯,TPR=",round(TPR,2)))
  
  TNR <- (cmKNN.factor[1]/sum(cmKNN.factor[1,]))
  print(paste("箇代",mycolName[2],"タ絋瞯,TNR=",round(TNR,2)))
  
  accuracy <- sum(diag(cmKNN.factor))/sum(cmKNN.factor)
  print(paste("俱砰非絋瞯=",round(accuracy,2)))
}else if(length(cmKNN.factor)==2){
  
  precision <- (cmKNN.factor[[2]]/sum(cmKNN.factor[,1]))
  paste("箇代",mycolName[2],"タ絋瞯,precision=",round(precision))
  
  TPR <- (cmKNN.factor[[2]]/sum(cmKNN.factor[2,]))
  print(paste("箇代",mycolName[2],"タ絋瞯,TPR=",round(TPR)))
  
  accuracy <- sum(diag(cmKNN.factor)[[2]])/sum(cmKNN.factor[,1])
  print(paste("俱砰非絋瞯=",round(accuracy,2)))
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



library(ggplot2)

ggplot(df,aes(x=kv,y=accuracy,label=kv,color=accuracy))+geom_point(size=5)+geom_text(vjust=2)


