

smoothmat<- function( fdata, rangeval ) 
{
  
    fdat<- as.matrix(fdata)  
    
    m<-dim(fdat)[1] #Row(Curves)
    
    n<- dim(fdat)[2] #Col
    
    argvalmax<-rangeval[2]
    
    argvalmin<-rangeval[1]
        
    range<- round(seq(argvalmin, argvalmax, length.out = n), 3)
    
    smoothmat<-matrix(0,m,401)
    
    for( i in 1:m)
    { 
        for( j in 1:401  )
        {
        
      h<-dpill(range, fdat[i,])   
      smoothmat[i,j]<- locpoly(range, fdat[i,] , bandwidth = h)$y[j]
        
      }
    }
    
    meansmooth<-apply(smoothmat,1,mean)
      
    list(smoothdata = smoothmat, rangeval= range, mean= meansmooth )
    
}
