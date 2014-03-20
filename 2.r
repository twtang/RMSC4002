d<-read.csv("stock.csv")
t1<-as.ts(d$HSBC)
t2<-as.ts(d$CLP)
t3<-as.ts(d$CK)

u1<-(lag(t1)-t1)/t1
u2<-(lag(t2)-t2)/t2
u3<-(lag(t3)-t3)/t3

msd<-function(t,w) {
   n<-length(t)-w+1
   out<-c()
   for (i in 1:n) {
      j<-i+w-1
      s<-sd(window(t,i,j))
      out<-append(out,s)
   }
   out<-as.ts(out)
}

s1_90<-msd(u1,90)
s1_180<-msd(u1,180)

par(mfrow=c(2,1))
plot(s1_90)
plot(s1_180)
readline("Hit <Return> to continue:")

library(tseries)
res<-garch(u1,order=c(1,1))
names(res)
round(res$coef,6)
-2*res$n.likeli
readline("Hit <Return> to continue:")

summary(res)
plot(res)
readline("Hit <Return> to continue:")

Box.test(u1^2,lag=15,type="Ljung")
Box.test(res$resid^2,lag=15,type="Ljung")
readline("Hit <Return> to continue:")

par(mfrow=c(1,1))
s<-cbind(res$fitted.values[,1],s1_90,s1_180)
matplot(s,type="l")
readline("Hit <Return> to continue:")

u<-cbind(u1,u2,u3)
u[1042,]
cor(u[953:1042,])
var(u[953:1042,])
readline("Hit <Return> to continue:")

res1<-garch(u1)
res2<-garch(u2)
res3<-garch(u3)
coef<-rbind(res1$coef,res2$coef,res3$coef)
coef
round(apply(coef,2,mean),6)

