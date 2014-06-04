
Sigma<-function( x,y, l=1 ){
  
  Sigma<-matrix(rep(0,length(x)*length(y)),nrow=length(x))
  
  for(i in 1:nrow(Sigma)){
    
    for (j in 1:ncol(Sigma)) Sigma[i,j]<-exp(-1/2*(abs(x[i]-y[j])/l)^2)
    
  }
  
  return(Sigma)
}
