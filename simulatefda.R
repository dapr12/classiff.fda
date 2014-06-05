simulatefda<-function ( nsamples, ndrawn, rangeval , mean , sigma )
  {
  
  samples<- nsamples
  
  nval<-4 ## Need to change
  
  sn<- sigma
  
  mn<- mean
  
  valmax<-rangeval[2]
  
  valmin<-rangeval[1]
    
  x.star<-seq(valmin,valmax,l=ndrawn)
  
  y<-rnorm(nval,mn,0)
  
  f <-data.frame(x.star,y)
  
  k.xx <- Sigma(f$x,f$x)
  
  k.xxs <- Sigma(f$x,x.star)
  
  k.xsx <- Sigma(x.star,f$x)
  
  k.xsxs <- Sigma(x.star,x.star)
  
  f.bar.star <-k.xsx%*%solve(k.xx+sn^2*diag(1,ncol(k.xx)))%*%f$y
  
  cov.f.star <-k.xsxs-k.xsx%*%solve(k.xx+sn^2*diag(1,ncol(k.xx)))%*%k.xxs
  
  values<-matrix(rep(0,length(x.star)*samples),ncol=samples)
  
  for (i in 1:samples){ 
        
    values[,i]<-mvrnorm(1,f.bar.star,cov.f.star)
    
  }
  
  list( rangetime= x.star, simulation = values  ) 
  
}
