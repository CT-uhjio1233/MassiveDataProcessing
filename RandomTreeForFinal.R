
#安裝package
#install.packages("randowForest")

packageName="randomForest"
if(!(packageName %in% rownames(installed.packages()))){
  install.packages(packageName)
}

#載入library
library(randomForest)

#(1)載入資料
setwd('G:/RCode') #設定工作目錄(資料集存放的地方)

#(2)測試模型，可隨機產生。(訓練資料、測試資料)
#第一種分割方式:自己把資料分割好再做

traindata=read.csv("SPECTF_train.csv")
testdata=read.csv("SPECTF_test.csv")

#第二種分割方式:電腦把資料分割好再做
#parkinsons=read.csv(ParkinsonsANN.csv)
#去除第一欄(決策樹)PK欄位不適合分岔屬性
#parkinsons <- parkinsons[- c(1)]
#np = ceiling(0.1*nrow(Parkinsons)) #無條件近位
#test.index=sample(1:nrow(Parkinsons),np) #取索引值，抽樣10%(np)個
#testdata=Parkinsons[test.index,] #設定測試資料
#traindata=Parkinsons[-test.index,] #設定訓練資料


#(3)建立決策樹模型:因變數~自變數
Parkinsons.RF=randomForest(status~.,data=traindata,importance=TRUE,proximity=TRUE,ntree=500,na.action=na.fail)
#第一個參數(status~.):表示除了status屬性之外，其他屬性皆為模型之引數
#第二個參數(data):表示模型中含有變數的一組資料
#第三個參數(importance=TRUE):是否計算模型中各屬性之重要值，資料型態為布林
#第四個參數(proximty=TRUE):	是否計算模型的鄰近矩陣，此參數搭配含數MDSplot()使用，資料型態為布林
#第五個參數(ntree=500):表示森林中的樹木數量
#第六個參數(subset):表示選出的訓練集資料為第幾筆(此參數的資料格式為向量)，當data為整個資料集時
#第七個參數(na.action=na.fail):表示遺漏值之處理，na.fail表示不能出現遺漏值


#(4)畫圖
print(Parkinsons.RF)
plot(Parkinsons.RF)

#利用importance()函數，得到MeanDecreaseAccuracy與MeanDecreaseGini，
#值愈高就表示該屬性對於該模型的判別影響愈大，
#可以做為後續利用其他演算法建模時刪減屬性的依據。
importance(Parkinsons.RF)

#(5)預測
result <- predict(parkinsons.RF,newdata=testdata,type="class")

#(6)建立混淆矩陣(confusion matrix)觀察模型表現
cm <- table(testdata$status,result,dnn=c("x","y")) #x為實際y為預測
cm

mycolName <- colnames(cm)
mycolName[1]
#(6A)正確率
#計算正確率(precision)
precision <- (cm[[1]] / sum(cm[,1]))
paste("預測",mycolName[1],"的正確率，precision=",precision)

#計算正確率(TPR)
TPR <- (cm[[1]] / sum(cm[1,]))
paste("預測",mycolName[1],"的正確率，TPR=",TPR)

#計算戶正確率(TNR)
TNR <- (cm[[4]] / sum(cm[2,]))
paste("預測",mycolName[2],"的正確率，TNR=",TNR)

#(6B)正確率
#計算正確率(precision)
precision <- (cm[[4]] / sum(cm[,2]))
paste("預測",mycolName[2],"的正確率，precision=",precision)

#計算正確率(TPR)
TPR <- (cm[[4]] / sum(cm[2,]))
paste("預測",mycolName[2],"的正確率，TPR=",TPR)

#計算戶正確率(TNR)
TNR <- (cm[[1]] / sum(cm[1,]))
paste("預測",mycolName[2],"的正確率，TNR=",TNR)

#整體準確率(取出對角/總數)
accuracy <- sum(diag(cm)) / sum(cm)
paste("整體準確率 =",accuracy)
cm

#(6)建立混淆矩陣(confusion matrix)觀察模型表現
cmRF <- table(testdata$status,result,dnn=c("x","y")) #x為實際y為預測
cmRF.factor <- table(factor(testdata$status,ordered=TRUE,levels=c("Y","N")),factor(result,ordered=TRUE,levels=c("Y","N")),dnn=c("Real","Pred"))
cmRF.factor

