# C-TREE

d<-read.csv("fin-ratio.csv")
library(rpart)
ctree<-rpart(HSI~EY+CFTP+ln_MV+DY+BTME+DTE,data=d,method="class")
plot(ctree,asp=0.5,main="Fin-ratio")
text(ctree,use.n=T,cex=0.6)
readline("Hit <Return> to continue:")

print(ctree)
readline("Hit <Return> to continue:")

plot(d$HSI,d$ln_MV,pch=21,bg=c("red","blue")[d$HSI+1])
abline(h=9.478)
readline("Hit <Return> to continue:")

pr<-predict(ctree)
cl<-0*(pr[,1]>0.5)+1*(pr[,2]>0.5)
table(cl,d$HSI)
readline("Hit <Return> to continue:")

d<-read.csv("iris.csv")
names(d)
ctree<-rpart(Species~Sepal_len+Sepal_wid+Petal_len+Petal_wid,data=d,method="class")
plot(ctree,asp=2)
text(ctree,use.n=T,cex=0.6)
readline("Hit <Return> to continue:")

print(ctree)
readline("Hit <Return> to continue:")

plot(d$Petal_len,d$Petal_wid,pch=21,bg=c("red","blue","green")[d$Species])
#plot(d$Petal_len,d$Petal_wid,pch=21,col=d$Species)
abline(h=1.75)
abline(v=2.45)
readline("Hit <Return> to continue:")

pr<-predict(ctree)
cl<-max.col(pr)
table(cl,d$Species)
readline("Hit <Return> to continue:")

# Training and Testing data

d<-read.csv("titanic.csv")
names(d)
set.seed(12345)
dim(d)
id<-sample(1:2201,1980)
d1<-d[id,]
d2<-d[!((1:2201)%in%id),]
dim(d1)
dim(d2)
ctree<-rpart(Survive~Class+Age+Sex,data=d1,method="class")
plot(ctree,asp=6)
text(ctree,cex=0.5,use.n=T)
readline("Hit <Return> to continue:")

prob<-predict(ctree)
pr<-prob[,1]<0.5
table(pr,d1$Survive)
readline("Hit <Return> to continue:")

prob<-predict(ctree,d2)
pr<-prob[,1]<0.5
table(pr,d2$Survive)
readline("Hit <Return> to continue:")



# K-mean clustering

d<-read.csv("iris.csv")
d1<-d[,1:4]
km<-kmeans(d1,3)
print(km)
readline("Hit <Return> to continue:")

plot(d1,col=km$cluster)
readline("Hit <Return> to continue:")

table(km$cluster,d$Species)
readline("Hit <Return> to continue:")

d<-read.table("fin-ratio.csv",sep=",",header=T)
d1<-d[,1:6]
km<-kmeans(d1,2)
plot(d1,col=km$cluster)
readline("Hit <Return> to continue:")

# Choosing suitable k

d<-read.csv("iris.csv")
x<-d[,1:4]
km<-kmeans(x,3)
km

d1<-x[km$cluster==1,]
d2<-x[km$cluster==2,]
d3<-x[km$cluster==3,]
dim(d1)
dim(d2)
dim(d3)

n1<-dim(d1)[1]
n2<-dim(d2)[1]
n3<-dim(d3)[1]

apply(d1,2,mean)
apply(d2,2,mean)
apply(d3,2,mean)

(n1-1)*var(d1)
sum(diag((n1-1)*var(d1)))
sum(diag((n2-1)*var(d2)))
sum(diag((n3-1)*var(d3)))

source("km.r")
km2<-km(x,2)
km3<-km(x,3)
km4<-km(x,4)
km5<-km(x,5)

table(km3,d[,5])


# financial ratio data
d<-read.csv("fin-ratio1.csv")	# read in data
x<-d[,1:6]                   	# excludet HSI

km2<-km(x,2)   
km3<-km(x,3)   
km4<-km(x,4)   
km5<-km(x,5)   

lab<-names(x)			# save var. names
par(mfrow=c(2,3))		# define 2x3 multi-frame graphic
boxplot(x[,1]~km2,main=lab[1])	# boxplots for each variable
boxplot(x[,2]~km2,main=lab[2])
boxplot(x[,3]~km2,main=lab[3])
boxplot(x[,4]~km2,main=lab[4])
boxplot(x[,5]~km2,main=lab[5])
boxplot(x[,6]~km2,main=lab[6])










