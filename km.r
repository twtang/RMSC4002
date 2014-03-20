# improved kmeans()
# Try kmeans(x,k) serveal times and output the best (largest ratio) trial
# x is the matrix of input variable, k is the no. of clusters
# try is no. of trials
# display cluster size and stat, output cluster label

kmstat<-function(x,k) {
  km<-kmeans(x,k)			# k-means clustering with k=3
  ng<-km$size				# sample size
  n<-dim(x)[1]
  m<-apply(x,2,mean)			# compute overall mean
  m<-matrix(rep(m,k),nr=k,byrow=T)	# create a matrix whose row is m
  dm<-(km$centers-m)^2
  ssb<-apply(dm,1,sum)
  ssb<-sum(ng*ssb)				# compute total between group ss
  ssw<-sum(km$withinss)				# compute total within group ss
  out<-list((n-k)*ssb/((k-1)*ssw),ng,km$cluster)# save stat, ng and cluster index into a list
  names(out)<-c("stat","size","cluster")	# apply names to list
  out						# output
}

km<-function(x,k,try=5) {		# default no. of trial is 5

  res0<-kmstat(x,k)			# save the result of the first trial
  r0<-res0$stat				# save the stat from the first trial

  for (i in 2:try) {
    res<-kmstat(x,k)  			# new trial 
    if (res$stat>r0) {			# if new trial is better
      r0<-res$stat			# update r0 and res
      res0<-res
    }
  }
  cat("cluster size=",res0$size,"\n")	# display cluster size
  cat("stat=",res0$stat,"\n")		# display stat
  res0$cluster				# output cluster label
}  




