#install.package("neuralnet")
#install.package("nnet")
#install.package("caret")

require(neuralnet)#for neuralnet(),nn model
require(nnet)#for class.ind()
require(caret)#for train(),tune paramenters

setwd("~/Documents/database")#設定工作目錄

#測試模型,可隨機產生.(訓練資料,測試資料)
#Parkinsons=read.csv("ParkinsonsANN.csv")
#去除第一欄(決策樹)PK欄位不適合分岔屬性
#Parkinsons<-Parksons[-c(1)]
#np=ceiling(0.1*nrow(Parkinsons))#無條件進位
#test.index=sample(1:nrow(Parkinsons),np)#取索引值,抽樣10%(np)個
#testdata=Parkinsons[test.index,]#設定測試資料
#traindata=Parkinsons[-test.index,]#設定訓練資料

traindata=read.csv("Parkinsons_TrainANN.csv")
testdata=read.csv("Parkinsons_TestANN.csv")
#去除第一欄PK不適合分岔屬性
testdata <- testdata[-c(1)]
traindata <- traindata[-c(1)]

#原始資料就會變成像這樣
head(traindata)
formula.bpn <- as.formula(status ~ B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W)

bpn <- neuralnet(data=traindata,
                 formula=formula.bpn,
                 
                 hidden=c(2),#一個隱藏層:2個node
                 learningrate=0.01,#leearing rate
                 threshold=0.01,#partial derivatives of the error function, a stopping criteria
                 stepmax=5e5#最大的iteration數=500000(5*10^5)
                 )

plot(bpn)

pred<- compute(bpn,testdata[1:22])

pred$net.result

#pred.result<-round(pre$net.result)
pred.result<-round(1/(1+exp(-pred$net.result)))
pred.result

cm <- table(testdata$status,pred.result)
print(cm)

if(length(cm)==4){
  mycolNames<-colnames(cm)
  mycolName[1]
  
  precision <- (cm[[1]]/sum(cm[,1]))
  paste("預測",mycolName[1],"的正確率,precision=",precision)
  
  TPR <- (cm[[1]]/sum(cm[1,]))
  print(paste("預測",mycolName[1],"的正確率,TPR=",TPR))
  
  TNR <- (cm[[4]]/sum(cm[2,]))
  print(paste("預測",mycolName[1],"的正確率,TNR=",TNR))
  
  precision <- (cm[[4]]/sum(cm[,2]))
  paste("預測",mycolName[1],"的正確率,precision=",precision)
  
  TPR <- (cm[[4]]/sum(cm[2,]))
  print(paste("預測",mycolName[1],"的正確率,TPR=",TPR))
  
  TNR <- (cm[[1]]/sum(cm[1,]))
  print(paste("預測",mycolName[1],"的正確率,TNR=",TNR))
  
  accuracy <- sum(diag(cm))/sum(cm)
  print(paste("整體準確率=",round(accuracy,2)))
}else{
  print("confusuion matrix 的個數<4,需額外計算")
  }

