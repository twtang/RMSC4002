d<-read.csv("stock.csv")	# read in data file
names(d)			# display names in d
t1<-as.ts(d$HSBC)		# save as time series
t2<-as.ts(d$CLP)
t3<-as.ts(d$CK)

u1<-(lag(t1)-t1)/t1		# compute daily percentage return
u2<-(lag(t2)-t2)/t2
u3<-(lag(t3)-t3)/t3

par(mfrow=c(3,1))		# define multi-frame for ploting
plot(u1)
plot(u2)
plot(u3)
readline("Hit <Return> to continue:")

par(mfrow=c(3,2))
hist(u1)			# histogram
qqnorm(u1)			# qq-normal plot
qqline(u1)			# add a line for reference
hist(u2)
qqnorm(u2)
qqline(u2)
hist(u3)
qqnorm(u3)
qqline(u3)
readline("Hit <Return> to continue:")

su1<-sort(u1)			# sort u1
n<-length(u1)			# length of u1
s1<-var(u1)			# sample var
ku1<-sum(u1^4)/(n*s1^2)-3	# excess kurtosis
v1<-round(6/ku1+4)		# v rounded to nearest integer
i<-((1:n)-0.5)/n		# create a vector of percentiles
q<-qt(i,v1)*sqrt((v1-2)/v1)	# percentile point from t(v)

hist(u1)			# histogram of ui
plot(q,su1,main="qq-t plot")	# plot (q,su1)
abline(lsfit(q,su1))		# add reference line


su2<-sort(u2)
n<-length(u2)
s2<-var(u2)
ku2<-sum(u2^4)/(n*s2^2)-3
v2<-round(6/ku2+4)
i<-((1:n)-0.5)/n
q<-qt(i,v2)*sqrt((v2-2)/v2)

hist(u2)
plot(q,su2,main="qq-t plot")
abline(lsfit(q,su2))


su3<-sort(u3)
n<-length(u3)
s3<-var(u3)
ku3<-sum(u3^4)/(n*s3^2)-3
v3<-round(6/ku3+4)
i<-((1:n)-0.5)/n
q<-qt(i,v3)*sqrt((v3-2)/v3)

hist(u3)
plot(q,su3,main="qq-t plot")
abline(lsfit(q,su3))

readline("Hit <Return> to continue:")

u<-cbind(u1,u2,u3)		# combine into matrix u
n2<-nrow(u)			# no. of row in u
n1<-n2-180+1			# starting index: 180th obs before n2
u180<-u[n1:n2,]			# save the most recent 180 days to u180
m<-apply(u180,2,mean)		# compute column mean of u180
m
s<-var(u180)			# compute variance of u180
s

par(mfrow=c(1,1))		
sinv<-solve(s)				# compute inv(s)
m<-matrix(m,nr=180,nc=3,byrow=T)	# transform m into a 180x3 matrix, each row is the col. mean
d2<-diag((u180-m)%*%sinv%*%t(u180-m))	# compute squared gen. dist.
d2<-sort(d2)				# sort d2 in ascending order
i<-((1:180)-0.5)/180			# create a vector of percentiles
q<-qchisq(i,3)				# compute quantile of chisq(3)
qqplot(q,d2)				# QQ-chisquare plot
abline(lsfit(q,d2))			# add the reference line
readline("Hit <Return> to continue:")

cor(u180)				# correlation matrix of u180
pairs(u180)				# scatter plot matrix
readline("Hit <Return> to continue:")

t<-cbind(t1,t2,t3)			# combine the time series
matplot(t,type="l")			# plot the series in one plot
readline("Hit <Return> to continue:")

plot(t)					# plot the series using separate axis
readline("Hit <Return> to continue:")

set.seed(7)			# set random seed
mu<-252*apply(u180,2,mean)	# compute annual return rate
sigma<-252*var(u180)		# compute annula variance rate
C<-chol(sigma)			# Cholesky decomposition of sigma
s<-cbind(t1,t2,t3)		# combine t1,t2,t3 to form s
s0<-s[n2+1,]			# set s0 to the most recent price
dt<-1/252			# compute delta t
rdt<-sqrt(dt)			# compute square root of delta t
for(i in 1:90) {		# simulate price for future 90 days
   z<-rnorm(3)			# generate normal random vector
   v<-dt*mu+rdt*t(C)%*%z	# transform to multivariate normal
   s1<-s0*(1+v)			# new stock price
   s<-rbind(s,t(s1))		# append s1 to s
   s0<-s1			# update s0
}

matplot(s,type="l")
readline("Hit <Return> to continue:")

plot(as.ts(s))
readline("Hit <Return> to continue:")


par(mfrow=c(3,2))
hist(t1)			# histogram
qqnorm(t1)			# qq-normal plot
qqline(t1)			# add a line for reference
hist(t2)
qqnorm(t2)
qqline(t2)
hist(t3)
qqnorm(t3)
qqline(t3)


plot(t1,lag(t1))	# plot t(i+1) vs t(i)
plot(u1,lag(u1))	# plot u(i+1) vs u(i)
plot(t2,lag(t2))
plot(u2,lag(u2))
plot(t3,lag(t3))
plot(u3,lag(u3))


