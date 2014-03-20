# ANN

library(nnet)
d<-read.csv("iris.csv")
iris.nn<-nnet(d[,1:4],d[,5],size=2,linout=T)
readline("Hit <Return> to continue:")

summary(iris.nn)
readline("Hit <Return> to continue:")

pred<-round(iris.nn$fit)
table(d[,5],pred)
readline("Hit <Return> to continue:")

d<-read.csv("fin-ratio.csv")
names(d)
x<-d[,1:6]
fin.nn<-nnet(x,d$HSI,size=2,linout=T,maxit=200)
readline("Hit <Return> to continue:")

summary(fin.nn)
readline("Hit <Return> to continue:")

pred<-round(fin.nn$fit)
table(d$HSI,pred)
readline("Hit <Return> to continue:")

# Improved version ann()

source("ann.r")						# load ann() function
d<-read.table("fin-ratio.csv",sep=",",header=T)
fin.nn<-ann(d[,1:6],d[,7],size=2,linout=T,try=10)	# try 10 times
fin.nn$value						# display the best result
summary(fin.nn)
readline("Hit <Return> to continue:")

pred<-round(fin.nn$fit)
table(d$HSI,pred)
readline("Hit <Return> to continue:")

# Ann (logistic output)

library(nnet)
d<-read.csv("iris.csv")
y<-as.factor(d[,5])
x<-d[,1:4]
iris.nn<-ann(x,y,size=2,maxit=200,try=10)	# logistic output 
iris.nn$value
summary(iris.nn)
pred<-max.col(iris.nn$fit)
table(pred,d[,5])
readline("Hit <Return> to continue:")


d<-read.csv("fin-ratio.csv")
x<-d[,1:6]
y<-as.factor(d$HSI)
fin.nn<-ann(x,y,size=2,maxit=200,try=10)	# logistic output
fin.nn$value
summary(fin.nn)
pred<-1*(fin.nn$fit>1/2)
table(pred,d$HSI)
readline("Hit <Return> to continue:")


# Backpropagation (logistic output)

logistic<-function(x) {1/(1+exp(-x))}   # define activiation function

x<-matrix(c(0.4,0.7,0.8,0.9,1.3,1.8,-1.3,-0.9),ncol=2,byrow=T)  # input x
x<-cbind(1,x)                                           # attach a column of ones

y<-c(0,0,1,0)
w1<-matrix(c(0.1,-0.2,0.1,0.4,0.2,0.9),byrow=T,nrow=2)  # hidden layer weights
w2<-c(0.2,-0.5,0.1)                     # output layer weights
h<-rbind(1,logistic(w1%*%t(x)))         # hidden layer values
out<-logistic(w2%*%h)                   # output value

e1<-y-out                               # initial error
sse<-sum(e1^2)                          # compute sum of squared error
e1
sse
readline("Hit <Return> to continue:")

lr<-0.5                                 # learning rate
del2<-out*(1-out)*e1                    # output layer
del_w2<-2*lr*del2%*%t(h)
new_w2<-w2+del_w2                       # new output layer weights
err<-y-logistic(new_w2%*%h)

del1<-h*(1-h)*(w2%*%del2)               # hidden layer
del1<-del1[c(2,3),]
del_w1<-2*lr*del1%*%x
new_w1<-w1+del_w1                       # new hidden layer weights

err<-y-logistic(new_w2%*%rbind(1,logistic(new_w1%*%t(x))))

sse<-sum(err^2)                         # final error
sse
readline("Hit <Return> to continue:")


# Ann for prediction (linear output)

d<-read.csv("iris.csv")
set.seed(12345)
id<-sample(1:150,size=30,replace=F)
d1<-d[-id,]
d2<-d[id,]
iris.nn<-ann(d1[,1:4],d1[,5],size=2,linout=T,maxit=200,try=10)
iris.nn$value
summary(iris.nn)

w1<-matrix(iris.nn$wts[1:10],nr=2,byrow=T)
w2<-matrix(iris.nn$wts[11:13],nr=1,byrow=T)
w1
w2

logistic<-function(x) { 1/(1+exp(-x)) }
x<-cbind(1,d2[,1:4])
dim(x)

out1<-logistic(w1%*%t(x))
out1<-rbind(1,out1)
dim(out1)

out2<-t(w2%*%out1)
dim(out2)

pr<-round(out2)
table(pr,d2[,5])
readline("Hit <Return> to continue:")

# or using predict() function

pr<-predict(iris.nn,d2)
table(round(pr),d2[,5])
readline("Hit <Return> to continue:")

    
# Ann for prediciton (logistic out)

y<-as.factor(d1[,5])		# change y to factor 
iris.nn<-ann(d1[,1:4],y,size=2,maxit=200,try=10)
iris.nn$value

pr<-predict(iris.nn,d2)
pred<-max.col(pr)
table(pred,d2[,5])




