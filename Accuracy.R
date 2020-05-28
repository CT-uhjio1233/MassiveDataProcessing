Accuracy.DF=data.frame(Name=c("KNN","RF","NaiveBay","SVM","Dtree","ANN"))
Accuracy.DF$Accuracy=c(knnaccuracy,RFaccuracy,NBaccuracy,SVMaccuracy,DTreeaccuracy,ANNaccuracy)
Accuracy.DF
arrange(Accuracy.DF,desc(Accuracy))
df<- data.frame(Accuracy = c(knnaccuracy,RFaccuracy,NBaccuracy,SVMaccuracy,DTreeaccuracy,ANNaccuracy),
                name=c("KNN","RF","NaiveBay","SVM","DTree","ANN"))
ggplot(df,aes(x=name,y=Accuracy,color=name,label=Accuracy))+geom_point(size=5)+geom_text(vjust=2)