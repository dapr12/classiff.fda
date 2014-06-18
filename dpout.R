dpout<-function(lamb, plotting=TRUE, title="Observations",ylabel="x(t)",linecol=2,legend=T, type="l", lty=2, lwd=1, cex=1, col=NULL, cold=NULL, ylim=NULL, colRef=NULL,...)
{
  #function dpout = dplot(lambda)
  #
  #Graphical Bayesian selection of PCs
  #Computes and plots "d-plot" and related quantities
  #
  # INPUT
  #   lambda:  Eigenvalues of covariance or correlation matrix
  #            (Must be in decreasing order and strictly positive -- drop the zeros)
  #
  # OUTPUT
  #   Struct OUTDPLOT with the following fields:
  #       d:  Model dimensions
  #       fhat:  Function "F-hat"
  #       imaj:  Indices of FHAT that are vertices of the concave majorant
  #       thmin:  Theta_(d-1)
  #       thmax:  Theta^(d-1)
  #       ijump:  Indices of THMAX where d-function has a step
  #       bstck:  Broken stick
  #
  #   The d-function, the scree graph and the broken-stick graph are plotted
  #
  
  lambda<-sort(lamb,decreasing=T)  
  
  m<-length(lambda)
  
  d<-seq(0,m-1,1)
  
  fhat<-rep(0,m)
  
  for( i in 1:m)
    {
      fhat[i]<-sum(log(lambda[i:m])) - (m-i+1)*log(mean(lambda[i:m]))
  }

  imaj<-chull(d, fhat)

  thmin<-rep(0,m)
  
  for( i in 2:m-1)
    {
      thmin[i]<-max((fhat[(i+1):m]- fhat[i])/(d[(i+1):m]-d[i]))
  }

  thmin[m]<-0

  thmax<-rep(0,m)

  thmax[1]<-Inf

  for( i in 2:m )
    {
    
    thmax[i]<-min((fhat[i]-fhat[1:(i-1)])/(d[i]-d[1:(i-1)]))
    
  }
  
  ijump<-which(thmin<=thmax)
  
  bstck<-rep(0,m)

  for(i in 1:m)
    {
    bstck[i]<-(1/m)*sum(1/i:m)
          
  }

  bstck<-sum(lambda)*bstck
  
  if (plotting) {  
    
    if (linecol==1){
      
      color<-rep(8,m)
      
      color[sh.out]<-1
      
    }
    
    else{
      
      hues = seq(15, 375, length=m+1)
      
      color<-hcl(h=hues, l=65, c=100)[1:m]
      
    }
    
    dev.new(width=12,height=6)
    
    layout(matrix(c(1,2), 1, 2, byrow = T))
    
    if (length(t)<1) {t<-1:p}
    
    plot(lambda, type="l",col =color, xlab="", ylab=ylabel,main=title,lty=1)
    
    lines(bstck)
        
    plot(thmax[ijump],d[ijump], type="S", xlab="Modified Epigrahp Index", ylab="Modified Band Depth",main="Outliergram")
    
  
      
  }  ## end IF plotting  
  
  
  list(dimension = d, 
     
     fhat = fhat, 
     
     imaj = imaj,
     
     ijump = ijump,
    
     bstck = bstck  ) 
  
}
