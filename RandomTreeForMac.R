




#è¼‰å…¥è³‡æ?™library
library(randomForest)

#è¨­å?šå·¥ä½œç›®???
setwd("~/Documents/database")

#æ¸¬è©¦æ¨¡å??,?¯?š¨æ©Ÿç”¢???(è¨“ç·´è³‡æ??,æ¸¬è©¦è³‡æ??)
traindata=read.csv("Parkinsons_Train.csv")
testdata=read.csv("Parkinsons_Test.csv")

#ç¬¬ä?Œç¨®è³‡æ?™è?‰å…¥?–¹å¼?
#Parkinsons=read.csv("Parkinsons_Test.csv")
#Parkinsons<-Parkinsons[-c(1)]
#np=ceiling(0.1*nrow(Parkinsons))#?„¡æ¢ä»¶?€²ä??
#test.index=sample(1:norw(Parkinsons),np)#??–æ?€å¼•å€?
#testdata=Parkinsons[test.index,]#è¨­å?šæ¸¬è©¦è?‡æ??
#traindata=Parkinsons[-test.index,]#è¨­å?šè?“ç·´è³‡æ??

#å»ºç?‹æ±ºç­–æ¨¹æ¨¡å??;(?? è?Šæ•¸~?‡ªè®Šæ•¸)
Parkinsons.RF=randomForest(status~.,data=traindata.,importance=TRUE,proximity=TRUE,ntree=500,action=na.fail)
#ç¬¬ä?€?€‹å?ƒæ•¸(G3~.):è¡¨ç¤º?™¤äº†G3å±¬æ€§ä?‹å?–ï?Œå…¶ä»–å±¬?€§ç?†ç‚ºæ¨¡å?‹ä?‹å?•æ•¸(?? ç‚º??‘å€‘è?é?æ¸¬G3??€~)
#ç¬¬ä?Œå€‹å?ƒæ•¸(data=stud_math):è¡¨ç¤ºæ¨¡å?‹ä¸­?«??‰è?Šæ•¸??„ä?€çµ„è?‡æ??
#ç¬¬ä?‰å€‹å?ƒæ•¸(importance=TRUE):?˜¯?¦è¨ˆç?—æ?å€‹æ¨¡??‹ä¸­??„å±¬?€§ä?‹é?è?å€¼ï?Œè?‡æ?™å?‹æ?‹ç‚ºå¸ƒæ??
#ç¬¬å?›å€‹å?ƒæ•¸(proximity=TRUE):?˜¯?¦è¨ˆç?—æ¨¡??‹ç?„é„°è¿‘çŸ©?™£ï¼Œæ­¤??ƒæ•¸?­??å‡½?•¸MDSplot()ä½¿ç”¨ï¼Œè?‡æ?™å?‹æ?‹ç‚ºå¸ƒæ??
#ç¬¬ä?”å€‹å?ƒæ•¸(ntree=500):è¡¨ç¤ºæ£®æ?—ä¸­??„æ¨¹?œ¨?•¸???
#ç¬¬å…­?€‹å?ƒæ•¸(subset=stud_math.train):è¡¨ç¤º?¸?‡º??„è?“ç·´??†è?‡æ?™ç‚ºç¬¬å¹¾ç­?(æ­¤å?ƒæ•¸??„è?‡æ?™æ ¼å¼ç‚º??‘é??)
#ç¬¬ä?ƒå€‹å?ƒæ•¸(na.action = na.fail):è¡¨ç¤º?ºæ¼å€¼ä?‹è?•ç?†ï?Œna.failè¡¨ç¤ºä¸èƒ½?‡º?¾?ºæ¼å€?

#?•«???
print(Parkinsons.RF)
plot(Parkinsons.RF)

#?ˆ©?”¨importance()?‡½?•¸,å¾—åˆ°MeanDecreaseAccuracy??‡MeanDecreaseGini
#?€¼æ?ˆé?˜å°±è¡¨ç¤ºè©²å±¬?€§å?æ–¼è©²æ¨¡??‹ç?„å½±?Ÿ¿??ˆå¤§
#?¯ä»¥ä?œç‚ºå¾Œç?Œåˆ©?”¨?…¶ä»–æ?”ç?—æ?•å»ºæ¨¡æ?‚åˆªæ¸›å±¬?€§ç?„ä?æ??
importance(Parkinsons.RF)

#å»ºæ?‹é?æ¸¬æ¨¡å??
result<- predict(Parkinsons.RF,newdata=testdata,type="class")


#å»ºç?‹æ··æ·†çŸ©?™£(confusion,matrix)è§€å¯Ÿæ¨¡??‹è¡¨?¾
cm <- table(testdata$status,result,dnn=c("å¯¦é??","??æ¸¬"))
cm
mycolName<- colnames(cm)#æª¢æŸ¥æ¬„ä?å?ç¨±
mycolName[1]

#(6A)æ­?ç¢ºç??
#è¨ˆç?—æ­£ç¢ºç??(precision)
precision <- (cm[[1]]/sum(cm[,1]))
st<- paste("??æ¸¬",mycolName[1],"??„æ­£ç¢ºç??,precision=",precision)
print(st)
#è¨ˆç?—æ­£ç¢ºç??(TPR)
TPR <- (cm[[1]]/sum(cm[1,]))
st<- paste("??æ¸¬",mycolName[1],"??„æ­£ç¢ºç??,TPR=",TPR)
print(st)
#è¨ˆç?—æ­£ç¢ºç??(TNR)
TNR<-(cm[[4]]/sum(cm[2,]))
st<- paste("??æ¸¬",mycolName[2],"??„æ­£ç¢ºç??,TNR=",TNR)
print(st)
#(6B)æ­?ç¢ºç??
#è¨ˆç?—æ­£ç¢ºç??(precision)
precision <- (cm[4]/sum(cm[,2]))
st<- paste("??æ¸¬",mycolName[1],"??„æ­£ç¢ºç??,=precision",precision)
print(st)
#è¨ˆç?—æ­£ç¢ºç??(TPR)
TPR <- (cm[[4]]/sum(cm[2,]))
st<- paste("??æ¸¬",mycolName[2],"??„æ­£ç¢ºç??,TPR=",TPR)
print(st)
#è¨ˆç?—æ­£ç¢ºç??(TNR)
TNR <- (cm[[4]]/sum(cm[2,]))
st<- paste("??æ¸¬",mycolName[2],"??„æ­£ç¢ºç??,TNR=",TNR)
print(st)

#?•´é«”æ?–ç¢º???(??–å‡ºå°è??/ç¸½æ•¸)
accuracy <- sum(diag(cm))/sum(cm)
st<- paste("?•´é«”æ?–ç¢º???=",accuracy)
print(st)
st<- paste("?•´é«”æ?–ç¢º???=",round(accuracy,2))
print(st)

#å»ºç?‹æ··æ·†çŸ©?™£(confusion,matrix)è§€å¯Ÿæ¨¡??‹è¡¨?¾
cm <- table(testdata$status,result,dnn=c("å¯¦é??","??æ¸¬"))
cmRF.factor <- table(factor(testdata$status,ordered=TRUE,levels=c("Y","N")),factor(result,order=TRUE,levels=c("Y","N")),dnn=c("Real","Pred"))
cmRF.factor







