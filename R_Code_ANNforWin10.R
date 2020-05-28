#install.package("neuralnet")
#install.package("nnet")
#install.package("caret")

require(neuralnet)#for neuralnet(),nn model
require(nnet)#for class.ind()
require(caret)#for train(),tune paramenters

setwd("D:/r place")#�]�w�u�@�ؿ�

#���ռҫ�,�i�H������.(�V�m���,���ո��)
#Parkinsons=read.csv("ParkinsonsANN.csv")
#�h���Ĥ@��(�M����)PK��줣�A�X�����ݩ�
#Parkinsons<-Parksons[-c(1)]
#np=ceiling(0.1*nrow(Parkinsons))#�L����i��
#test.index=sample(1:nrow(Parkinsons),np)#�����ޭ�,���10%(np)��
#testdata=Parkinsons[test.index,]#�]�w���ո��
#traindata=Parkinsons[-test.index,]#�]�w�V�m���

traindata=read.csv("Parkinsons_TrainANN.csv")
testdata=read.csv("Parkinsons_TestANN.csv")
#�h���Ĥ@��PK���A�X�����ݩ�
testdata <- testdata[-c(1)]
traindata <- traindata[-c(1)]

#��l��ƴN�|�ܦ����o��
head(traindata)
formula.bpn <- as.formula(status ~ B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W)

bpn <- neuralnet(data=traindata,
                 formula=formula.bpn,
                 
                 hidden=c(2),#�@�����üh:2��node
                 learningrate=0.01,#leearing rate
                 threshold=0.01,#partial derivatives of the error function, a stopping criteria
                 stepmax=5e5#�̤j��iteration��=500000(5*10^5)
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
  paste("�w��",mycolName[1],"�����T�v,precision=",precision)
  
  TPR <- (cm[[1]]/sum(cm[1,]))
  print(paste("�w��",mycolName[1],"�����T�v,TPR=",TPR))
  
  TNR <- (cm[[4]]/sum(cm[2,]))
  print(paste("�w��",mycolName[1],"�����T�v,TNR=",TNR))
  
  precision <- (cm[[4]]/sum(cm[,2]))
  paste("�w��",mycolName[1],"�����T�v,precision=",precision)
  
  TPR <- (cm[[4]]/sum(cm[2,]))
  print(paste("�w��",mycolName[1],"�����T�v,TPR=",TPR))
  
  TNR <- (cm[[1]]/sum(cm[1,]))
  print(paste("�w��",mycolName[1],"�����T�v,TNR=",TNR))
  
  accuracy <- sum(diag(cm))/sum(cm)
  print(paste("����ǽT�v=",round(accuracy,2)))
}else{
  print("confusuion matrix ���Ӽ�<4,���B�~�p��")
}


