
fdaclass<- function(mdata, rangeval = NULL) 
{
  
  if ( is.null(rangeval) )
  { 
    
    data<- mdata  
  
    m<-dim(data)[1] #Row
    
    n<- dim(data)[2] #Col
  
    argvals<- seq(0, 1, length.out = n)
     
    rangeval<- c(0,1)
    
    colnames(data) <- argvals
  
    rownames(data) <- rownames(data, do.NULL = FALSE, prefix = "Obs.")
  
    list(data = data, 
       
         argvals = argvals, 
         
        rangevals = rangeval
       
       ) 
  }
  
  else {
    
    data<- mdata  
  
    m<-dim(data)[1] #Row
  
    n<- dim(data)[2] #Col
  
    argvalmax<-rangeval[2]
      
    argvalmin<-rangeval[1]
    
    argvals<- seq(argvalmin, argvalmax, length.out = n)
  
    rangeval<- rangeval
  
    colnames(data) <- argvals
  
    rownames(data) <- rownames(data, do.NULL = FALSE, prefix = "Obs.")
  
    list(data = data, 
       
         argvals = argvals, 
       
         rangevals = rangeval
       
    ) 
 }

}

  

#a1<-seq(0,1,by=.01)
#a2=rnorm(length(a1),sd=0.2)
#f1<-(sin(2*pi*a1))+rnorm(length(a1),sd=0.2)
#nc<-10
#np<-length(f1)
#tt=seq(0,1,len=101)
#mdata<-matrix(NA,ncol=np,nrow=nc)
#for (i in 1:nc) mdata[i,]<- (sin(2*pi*a1))+rnorm(length(a1),sd=0.2)

#fdataobj1<-fdaclass(mdata)
#fdataobj1

#fdataobj2<-fdaclass(mdata, c(2,4))
#fdataobj2


