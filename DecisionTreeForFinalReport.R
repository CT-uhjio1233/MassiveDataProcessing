
library("rpart")
library("rpart.plot")
library("rattle")

#???J????(?]?w?u?@?ؿ?,???ƤΦs???a)

setwd("G:/RCode")

#???ռҫ?,?i?H??????(?V?m????,???ո???)
traindata=read.csv("census_train.csv")
testdata=read.csv("census_test.csv")

require(rpart)

#?إߨM?????ҫ?;(?]?ܼ?~???ܼ?)
DataTree <- rpart(status~.,data=traindata,method = "class")


#?e?M????
require(rpart.plot)
prp(DataTree,          #?ҫ?
    faclen=0,          #?e?{???ܼƤ??n?Y?g
    fallen.leaves=TRUE,#?????K?H???????覡?e?{
    shadow.col="gray", #?̤U?誺?`?I???W???v
    extra=0)

fancyRpartPlot(DataTree)

#?w??

result <- predict(DataTree,newdata=testdata,type="class")

#?إ߲V?c?x?}(confusion,matrix)?[???ҫ????{
cm <- table(testdata$status,result,dnn=c("????","?w??"))
print(cm)
mycolName<- colnames(cm)
mycolName[1]

#(6A)???T?v
#?p?⥿?T?v(precision)
precision <- (cm[[1]]/sum(cm[,1]))
print(cm[,1])
st<-paste("?w??",mycolName[1],"?????T?v,precision=",precision)
print(st)

#?p?⥿?T?v(TPR)
TPR <- (cm[[1]]/sum(cm[1,]))
st<-paste("?w??",mycolName[1],"?????T?v,TPR=",TPR)
print(st)

#?p?⥿?T?v(TNR)
TNR<-(cm[[4]]/sum(cm[2,]))
st<-paste("?w??",mycolName[1],"?????T?v,TNR=",TNR)
print(st)

#(6B)???T?v
#?p?⥿?T?v(precision)
precision <- (cm[[4]]/sum(cm[,2]))
st<-paste("?w??",mycolName[2],"?????T?v,=precision",precision)
print(st)

#?p?⥿?T?v(TPR)
TPR <- (cm[[4]]/sum(cm[2,]))
st<-paste("?w??",mycolName[2],"?????T?v,TPR=",TPR)
print(st)

#?p?⥿?T?v(TNR)
TNR <- (cm[[1]]/sum(cm[1,]))
st<-paste("?w??",mycolName[2],"?????T?v,TNR=",TNR)
print(st)

#?????ǽT?v(???X?﨤/?`??)
accuracy <- sum(diag(cm))/sum(cm)
st<-paste("?????ǽT?v=",accuracy)
print(st)
st<-paste("?????ǽT?v=",round(accuracy,2))
print(st)
print(cm)

