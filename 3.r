# PCA

d<-read.csv("us-rate.csv")
label<-c("1m","3m","6m","9m","12m","18","2y","3y","4y","5y","7y","10y","15y")
names(d)<-label
options(digits=2)
cor(d)
readline("Hit <Return> to continue:")

# library(mva)
pca<-princomp(d,cor=T)
pca$loadings[,1:6]
readline("Hit <Return> to continue:")

pc1<-pca$loadings[,1]
pc2<-pca$loadings[,2]
pc3<-pca$loadings[,3]
pc1 %*% pc1
pc2 %*% pc2
pc1 %*% pc2

s<-pca$sdev
s

round(s^2,4)

t<-sum(s^2)
round(s^2/t,4)

cumsum(s^2/t)
readline("Hit <Return> to continue:")

screeplot(pca,type="lines")
readline("Hit <Return> to continue:")

par(mfrow=c(3,1))
plot(pc1,ylim=c(-0.6,0.6),type="o")
plot(pc2,ylim=c(-0.6,0.6),type="o")
plot(pc3,ylim=c(-0.6,0.6),type="o")
readline("Hit <Return> to continue:")

# scatterplot matrix of the scores of PC1, PC2 and PC3
score<-pca$scores[,1:3]
pairs(score)
readline("Hit <Return> to continue:")


# canonical correlation analysis

options(digits=4)
d<-read.table("us-rate.csv",sep=",",header=T)	# read in data
x<-d[,1:4]
y<-d[,8:13]


cxy<-cancor(x,y)		# save CCA result to cxy
x<-as.matrix(x)
y<-as.matrix(y)
cxy$cor
round(cxy$xcoef,3)
round(cxy$ycoef,3)
				# display result

u<-x%*%cxy$xcoef		# canonical variate U
v<-y%*%cxy$ycoef		# canonical variate V
cor(u[,1],v[,1])		# corr. between U1 and V1
cor(u[,2],v[,2])		# corr. between U2 and V2
cor(u[,1],v[,2])		# corr. between U1 and V2
cor(u[,2],v[,1])		# corr. between U2 and V1

par(mfrow=c(2,2))		# set up multiframe graphics
plot(u[,1],v[,1])		# plot V1 vs U1             
plot(u[,1],v[,2])		# plot V2 vs U1             
plot(u[,2],v[,1])		# plot V1 vs U2             
plot(u[,2],v[,2])		# plot V2 vs U2    

a<-cxy$xcoef[,1]		# normalize the xcoef
a/sqrt(sum(a^2))

b<-cxy$ycoef[,1]		# normalize the ycoef
b/sqrt(sum(b^2))


# check results with SAS
a<-c(-0.018647,0.017505,-0.245986,0.529705)	# results from SAS Raw scoring coeff.
a/sqrt(sum(a^2))

b<-c(1.221497,-0.074078,-1.489928,0.7313,-0.236164,0.128244)
b/sqrt(sum(b^2))



