qqchisq<-function(d){
  x<-as.matrix(d)
  n<-dim(x)[1]
  p<-dim(x)[2]
  m<-apply(x,2,mean)
  s<-var(x)
  sinv<-solve(s,diag(p))
  d2<-diag((x-m)%*%sinv%*%t(x-m))
  d2<-sort(d2)
  i<-((1:n)-0.5)/n
  q<-qchisq(i,p)
  qqplot(q,d2)
  b<-mean(d2)/mean(q)
  abline(0,b)
}
  
