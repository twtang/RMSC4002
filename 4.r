# logistic regression
#
d<-read.csv("fin-ratio.csv")
names(d)

summary(lm(HSI~EY+CFTP+ln_MV+DY+BTME+DTE,data=d))
readline("Hit <Return> to continue:")

summary(lm(HSI~EY+CFTP+ln_MV+DY+BTME,data=d))
readline("Hit <Return> to continue:")

summary(lm(HSI~EY+CFTP+ln_MV+DY,data=d))
readline("Hit <Return> to continue:")

summary(lm(HSI~CFTP+ln_MV+DY,data=d))
readline("Hit <Return> to continue:")

summary(lm(HSI~CFTP+ln_MV,data=d))
readline("Hit <Return> to continue:")

reg<-lm(HSI~CFTP+ln_MV,data=d)
names(reg)
par(mfrow=c(2,2))
plot(reg$fit,reg$resid)
qqnorm(reg$resid)
qqline(reg$resid)
res<-as.ts(reg$resid)
plot(res,lag(res))
plot(reg$resid)
readline("Hit <Return> to continue:")

summary(glm(HSI~EY+CFTP+ln_MV+DY+BTME+DTE,data=d,binomial))
readline("Hit <Return> to continue:")

lreg<-glm(HSI~EY+CFTP+ln_MV+DY+BTME+DTE,data=d,binomial)
names(lreg)
pr<-(lreg$fit>0.5)
table(pr,d$HSI)
readline("Hit <Return> to continue:")

# outlier detection
#
d0<-d[d$HSI==0,]
d1<-d[d$HSI==1,]
dim(d0)
dim(d1)

source("mdist.r")
x<-d0[,1:6]
md<-mdist(x)
par(mfrow=c(1,1))
plot(md)

c<-qchisq(0.99,df=6)
c
d2<-d0[md<c,]
dim(d2)
d3<-rbind(d1,d2)
dim(d3)
write.table(d3,file="fin-ratio1.csv",sep=",",row.names=F)

summary(glm(HSI~CFTP+ln_MV+BTME,data=d3,binomial))
lreg<-glm(HSI~CFTP+ln_MV+BTME,data=d3,binomial)
pr<-(lreg$fitted.values>0.5)
table(pr,d3$HSI)
readline("Hit <Return> to continue:")


# Dummy variable in logistic regression
#
g<-(d3$ln_MV>9.4766)+1
summary(glm(HSI~EY+CFTP+g+DY+BTME+DTE+EY*g+CFTP*g+DY*g+BTME*g+DTE*g,data=d3,binomial))
readline("Hit <Return> to continue:")

summary(glm(HSI~EY+CFTP+g+DY+DTE+EY*g+CFTP*g+DY*g+DTE*g,data=d3,binomial))
readline("Hit <Return> to continue:")

summary(glm(HSI~EY+CFTP+g+DY+EY*g+CFTP*g+DY*g,data=d3,binomial))
readline("Hit <Return> to continue:")

summary(glm(HSI~EY+g+DY+EY*g+DY*g,data=d3,binomial))
readline("Hit <Return> to continue:")

summary(glm(HSI~g+DY+DY*g,data=d3,binomial))
readline("Hit <Return> to continue:")

lreg<-glm(HSI~g+DY+DY*g,data=d3,binomial)
pr<-(lreg$fitted.values>0.5)
table(pr,d3$HSI)
readline("Hit <Return> to continue:")


# multinomial logit
#
d<-read.csv("iris.csv")
names(d)
library(nnet)
mn1<-multinom(Species~.,data=d)
summary(mn1)
readline("Hit <Return> to continue:")

pred<-predict(mn1)
table(pred,d$Species)
readline("Hit <Return> to continue:")

# model selection
#
d<-read.csv("fin-ratio1.csv")
lreg<-glm(HSI~.,data=d,binomial)
step(lreg)
readline("Hit <Return> to continue:")


# LDA
#
d<-read.csv("fin-ratio.csv")
x<-d[,1:6]
d1<-x[d$HSI==0,]
d2<-x[d$HSI==1,]
n1<-dim(d1)[1]
n2<-dim(d2)[1]
m1<-apply(d1,2,mean)
m2<-apply(d2,2,mean)
s1<-var(d1)
s2<-var(d2)
sp<-((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
dist1<-mahalanobis(x,m1,sp)
dist2<-mahalanobis(x,m2,sp)
w12<-(dist2-dist1)/2
pr<-w12<0
table(pr,d$HSI)
readline("Hit <Return> to continue:")

c<-log(n2/n1)
pr<-(w12<c)
table(pr,d$HSI)
readline("Hit <Return> to continue:")

library(MASS)
lda<-lda(HSI~EY+CFTP+ln_MV+DY+BTME+DTE,data=d)
pred<-predict(lda)$class
table(pred,d$HSI)
readline("Hit <Return> to continue:")


d<-read.csv("iris.csv")
names(d)
lda<-lda(Species~.,data=d)
pred<-predict(lda)$class
table(pred,d$Species)
readline("Hit <Return> to continue:")

d<-read.csv("fin-ratio.csv")
lda<-lda(HSI~.,data=d,CV=T)
table(lda$class,d$HSI)

