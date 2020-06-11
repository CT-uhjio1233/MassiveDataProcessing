
setwd('G:/RCode')
traindata=read.csv("SPECTF_train.csv")
testdata=read.csv("SPECTF_test.csv")

testdata<-testdata[-c(1)]
traindata<-traindata[-c(1)]

packageName="e1071"
if(!(packageName %in%rownames(installed.packages()))){
  install.packages(packageName)
}
library(e1071)

svmM<-svm(status~.,data=traindata,probability=TRUE)

results<-predict(svmM,testdata,probability=TRUE)

result.TR<-round(1/(1+exp(-results)))

Real<-as.factor(ifelse(testdata$status==1,"Y","N"))
Pred<-as.factor(ifelse(result.TR==1,"Y","N"))

cm<-table(R=Real,P=Pred)
cm

cmSVM<-table(Real=Real,Pred=Pred)
cmSVM
cmSVM.factor<-table(factor(Real,ordered=TRUE,levels=c("Y","N")),factor(Pred,order=TRUE,levels=c("Y","N")),dnn=c("Real","Pred"))
cmSVM.factor

SVMaccuracy<-round(sum(diag(cmSVM.factor))/sum(cmSVM.factor),4)
SVMaccuracy

if(length(cmSVM.factor)==4){
  mycolNames<-colnames(cmSVM.factor)
  mycolName[1]
  
  precision <- (cmSVM.factor[[1]]/sum(cmSVM.factor[,1]))
  paste("預測",mycolName[1],"的正確率,precision=",precision)
  
  TPR <- (cmSVM.factor[[1]]/sum(cmSVM.factor[1,]))
  print(paste("預測",mycolName[1],"的正確率,TPR=",TPR))
  
  TNR <- (cmSVM.factor[[4]]/sum(cmSVM.factor[2,]))
  print(paste("預測",mycolName[1],"的正確率,TNR=",TNR))
  
  precision <- (cmSVM.factor[[4]]/sum(cmSVM.factor[,2]))
  paste("預測",mycolName[1],"的正確率,precision=",precision)
  
  TPR <- (cmSVM.factor[[4]]/sum(cmSVM.factor[2,]))
  print(paste("預測",mycolName[1],"的正確率,TPR=",TPR))
  
  TNR <- (cmSVM.factor[[1]]/sum(cmSVM.factor[1,]))
  print(paste("預測",mycolName[1],"的正確率,TNR=",TNR))
  
  accuracy <- sum(diag(cmSVM.factor))/sum(cmSVM.factor)
  print(paste("整體準確率=",round(accuracy,2)))
}else #if(length(cmSVM.factor)==2)
{
  cmSVM.factor
  precision <- (cmSVM.factor[[2]]/sum(cmSVM.factor[,1]))
  print(paste("預測",mycolName[2],"的正確率,precision=",round(precision,2)))
  
  TPR <- (cmSVM.factor[[2]]/sum(cmSVM.factor[2,]))
  print(paste("預測",mycolName[2],"的正確率,TPR=",round(TPR,2)))
  
  accuracy <- (cmSVM.factor[[2]]/sum(cmSVM.factor[,1]))
  print(paste("整體準確率=",round(accuracy,2)))
}
cmSVM.factor

