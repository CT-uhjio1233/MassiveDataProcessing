plot(formula=Petal.Length~Petal.Width,data=iris,col=iris$Species)

packageName="ggplot"
if(!(packageName %in% rownames(installed.packages()))){
  install.packages(packageName)
}
data<-iris[-5]
km<-kmeans(data,centers=3,nstart = 10)
plot(formula =Petal.Length~Petal.Width,data=data,col=km$cluster,main="鳶尾花(iris)分群",xlab="Petal.Width(花瓣寬度)",
     ylab="Petal.Length(花瓣長度)")
library("ggplot2")
ggplot(data,aes(x=Petal.Length,y=Petal.Width))+geom_point(aes(x=Petal.Length,y=Petal.Width))+
  geom_point(aes(color=factor(km$cluster)))

WSS<-km$tot.withinss
WSS
BSS<- km$betweenss
BSS
TSS<- BSS+WSS
TSS
ratio<-WSS/TSS
ratio
