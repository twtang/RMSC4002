# improved nnet()
# Try nnet(x,y) k times and output the best trial
# x is the matrix of input variable
# y is the dependent value; y must be factor if linout=F is used

library(nnet)
ann<-function(x,y,size,maxit=100,linout=F,try=5) {
  ann1<-nnet(y~.,data=x,size=size,maxit=maxit,linout=linout)
  v1<-ann1$value

  for (i in 2:try) {
    ann<-nnet(y~.,data=x,size=size,maxit=maxit,linout=linout)
    if (ann$value<v1) {
      v1<-ann$value
      ann1<-ann
    }
  }
  ann1
}  



