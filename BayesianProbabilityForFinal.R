
packageName="e1071"
if(!(packageName%in%rownames(installed.packages()))){
  install.packages("e1071")
}

library(e1071)

setwd("G:/RCode")

traindata=read.csv("census_train.csv")
testdata=read.csv("census_test.csv")

nbcm <- naiveBayes(status~.,data=traindata)

result<-predict(nbcm,testdata)


cm <- table(testdata$status,result,dnn=c("實際","預測"))
cm
mycolName <- colnames(cm)
mycolName[1]

precision <- (cm[[1]]/sum(cm[,1]))
TPR <- (cm[[1]]/sum(cm[1,]))
TNR <- (cm[[4]]/sum(cm[2,]))
precision <- (cm[[4]]/sum(cm[,2]))
TPR <- (cm[[4]]/sum(cm[2,]))
TNR <- (cm[[1]]/sum(cm[1,]))
accuracy <- sum(diag(cm))/sum(cm)


print(paste("預測",mycolName[1],"的正確率,precision=",round(precision,2)))
print(paste("預測",mycolName[1],"的正確率,TPR=",round(TPR)))
print(paste("預測",mycolName[1],"的正確率,TNR=",round(TNR)))
print(paste("預測",mycolName[2],"的正確率,precision=",round(precision)))
print(paste("預測",mycolName[2],"的正確率,TPR=",round(TPR)))
print(paste("預測",mycolName[2],"的正確率,TNR=",round(TNR)))
print(paste("整體準確率=",round(accuracy,2)))
cm

cmBN.factor<- table(factor(testdata$status,ordered=TRUE,levels=c("Y","N")),factor(result,ordered=TRUE,levels=c("Y","N")),dnn=c("Real","Pred"))
cmBN.factor

if(length(cmBN.factor)==4){
  mycolName <- colnames(cmBN.factor)
  mycolName[1]
  
  ecision <- (cmBN.factor[[1]]/sum(cmBN.factor[,1]))
  print(paste("預測",mycolName[1],"的正確率,precision=",round(precision,2)))
  
  TPR <- (cmBN.factor[[1]]/sum(cmBN.factor[1,]))
  print(paste("預測",mycolName[1],"的正確率,TPR=",round(TPR,2)))
  
  TNR <- (cmBN.factor[[4]]/sum(cmBN.factor[2,]))
  print(paste("預測",mycolName[1],"的正確率,TNR=",round(TNR,2)))
  
  precision <- (cmBN.factor[[4]]/sum(cmBN.factor[,2]))
  print(paste("預測",mycolName[2],"的正確率,precision=",round(precision,2)))
  
  TPR <- (cmBN.factor[[4]]/sum(cmBN.factor[2,]))
  print(paste("預測",mycolName[2],"的正確率,TPR=",round(TPR,2)))
  
  TNR <- (cmBN.factor[[1]]/sum(cmBN.factor[1,]))
  print(paste("預測",mycolName[2],"的正確率,TNR=",round(TNR,2)))
  
  accuracy <- sum(diag(cmBN.factor))/sum(cmBN.factor)
  print(paste("整體準確率=",round(accuracy,2)))
}
cmBN.factor


