




#載入資料library
library(randomForest)

#設定工作目錄
setwd("d:/r place")

#測試模型,可隨機產生(訓練資料,測試資料)
traindata=read.csv("Parkinsons_Train.csv")
testdata=read.csv("Parkinsons_Test.csv")

#第二種資料載入方式
#Parkinsons=read.csv("Parkinsons_Test.csv")
#Parkinsons<-Parkinsons[-c(1)]
#np=ceiling(0.1*nrow(Parkinsons))#無條件進位
#test.index=sample(1:norw(Parkinsons),np)#取所引值
#testdata=Parkinsons[test.index,]#設定測試資料
#traindata=Parkinsons[-test.index,]#設定訓練資料

#建立決策樹模型;(因變數~自變數)
Parkinsons.RF=randomForest(status~.,data=traindata,importance=TRUE,proximity=TRUE,ntree=500,action=na.fail)
#第一個參數(G3~.):表示除了G3屬性之外，其他屬性皆為模型之引數(因為我們要預測G3呀~)
#第二個參數(data=stud_math):表示模型中含有變數的一組資料
#第三個參數(importance=TRUE):是否計算每個模型中各屬性之重要值，資料型態為布林
#第四個參數(proximity=TRUE):是否計算模型的鄰近矩陣，此參數搭配函數MDSplot()使用，資料型態為布林
#第五個參數(ntree=500):表示森林中的樹木數量
#第六個參數(subset=stud_math.train):表示選出的訓練集資料為第幾筆(此參數的資料格式為向量)
#第七個參數(na.action = na.fail):表示遺漏值之處理，na.fail表示不能出現遺漏值

#畫圖
print(Parkinsons.RF)
plot(Parkinsons.RF)

#利用importance()函數,得到MeanDecreaseAccuracy與MeanDecreaseGini
#值愈高就表示該屬性對於該模型的影響愈大
#可以作為後續利用其他演算法建模時刪減屬性的依據
importance(Parkinsons.RF)

#建構預測模型
result<- predict(Parkinsons.RF,newdata=testdata,type="class")


#建立混淆矩陣(confusion,matrix)觀察模型表現
cm <- table(testdata$status,result,dnn=c("實際","預測"))
cm
mycolName<- colnames(cm)#檢查欄位名稱
mycolName[1]

#(6A)正確率
#計算正確率(precision)
precision <- (cm[[1]]/sum(cm[,1]))
st<- paste("預測",mycolName[1],"的正確率,precision=",precision)
print(st)
#計算正確率(TPR)
TPR <- (cm[[1]]/sum(cm[1,]))
st<- paste("預測",mycolName[1],"的正確率,TPR=",TPR)
print(st)
#計算正確率(TNR)
TNR<-(cm[[4]]/sum(cm[2,]))
st<- paste("預測",mycolName[2],"的正確率,TNR=",TNR)
print(st)
#(6B)正確率
#計算正確率(precision)
precision <- (cm[4]/sum(cm[,2]))
st<- paste("預測",mycolName[1],"的正確率,=precision",precision)
print(st)
#計算正確率(TPR)
TPR <- (cm[[4]]/sum(cm[2,]))
st<- paste("預測",mycolName[2],"的正確率,TPR=",TPR)
print(st)
#計算正確率(TNR)
TNR <- (cm[[4]]/sum(cm[2,]))
st<- paste("預測",mycolName[2],"的正確率,TNR=",TNR)
print(st)

#整體準確率(取出對角/總數)
accuracy <- sum(diag(cm))/sum(cm)
st<- paste("整體準確率=",accuracy)
print(st)
st<- paste("整體準確率=",round(accuracy,2))
print(st)

#建立混淆矩陣(confusion,matrix)觀察模型表現
cm <- table(testdata$status,result,dnn=c("實際","預測"))
cmRF.factor <- table(factor(testdata$status,ordered=TRUE,levels=c("Y","N")),factor(result,order=TRUE,levels=c("Y","N")),dnn=c("Real","Pred"))
cmRF.factor








