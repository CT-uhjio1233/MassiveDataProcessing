
library("rpart")
library("rpart.plot")
library("rattle")

#載入資料(設定工作目錄,資料及存放地)
getwd()
setwd("D:/r place")
getwd()
#測試模型,可隨機產生(訓練資料,測試資料)
traindata=read.csv("Parkinsons_Train.csv")
testdata=read.csv("Parkinsons_Test.csv")

require(rpart)

#建立決策樹模型;(因變數~自變數)
DataTree <- rpart(status ~ ., data = traindata, method = "class")

#畫決策樹
require(rpart.plot)
prp(DataTree,          #模型
    faclen=0,          #呈現的變數不要縮寫
    fallen.leaves=TRUE,#讓樹枝以垂直的方式呈現
    shadow.col="gray", #最下方的節點塗上陰影
    #number of correct classification /number of observation is that node
    extra=2)

fancyRpartPlot(DataTree)

#預測
result <- predict(DataTree, newdata=testdata, type="class")

#建立混淆矩陣(confusion,matrix)觀察模型表現
cm <- table(testdata$status,result,dnn=c("x","y"))#x為實際y為預測
cm
mycolName<- colnames(cm)
mycolName[1]

#(6A)正確率
#計算正確率(precision)
precision <- (cm[[1]]/sum(cm[,1]))
paste("預測",mycolName[1],"的正確率,precision=",precision)

#計算正確率(TPR)
TPR <- (cm[[1]]/sum(cm[1,]))
paste("預測",mycolName[1],"的正確率,TPR=",TPR)

#計算正確率(TNR)
TNR<-(cm[[4]]/sum(cm[2,]))
paste("預測",mycolName[2],"的正確率,TNR=",TNR)

#(6B)正確率
#計算正確率(precision)
precision <- (cm[4]/sum(cm[,2]))
paste("預測",mycolName[1],"的正確率,=precision",precision)

#計算正確率(TPR)
TPR <- (cm[[4]]/sum(cm[2,]))
paste("預測",mycolName[2],"的正確率,TPR=",TPR)

#計算正確率(TNR)
TNR <- (cm[[4]]/sum(cm[2,]))
paste("預測",mycolName[2],"的正確率,TNR=",TNR)


#整體準確率(取出對角/總數)
accuracy <- sum(diag(cm))/sum(cm)
paste("整體準確率=",accuracy)
paste("整體準確率=",round(accuracy,2))



