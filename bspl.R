bspl<-function( x, k, ti, r)
{
  tKnots <-as.array(ti)
  #tt <-as.array(ti)
  xmat<-as.array(x)
  K<-k
  R<-r
  M<-length(xmat)
  N<-length(tKnots)
  y<-matrix(0, M,N+K-2)
  
  if ( r == 0)
  {
    if ( nrow(tKnots)> 1)
    {
      
      tknots<-t(tKnots)
      
    }
    tKnots<- c(repmat(tKnots[1],1,K-1), tKnots, repmat(tKnots[N], 1, K-1))
    N<-length(tKnots)
    b<-rep(0,K)
    #y<-matrix(0, M, N+K-2)
    for( l in 1:M)
    { b[1]<-1
      i<- max( which(tKnots <= xmat[l]))
      if( i == N){ i <- (N-K) }
      dr<-rep(0,K-1)
      dl<-rep(0,K-1)
      for(j in 1:(K-1))
      { dr[j]<-tKnots[i+j]-xmat[l]
        dl[j]<-xmat[l] - tKnots[i+1-j]
        saved<-0
        for(rx in 1:j)
        { term<-0
          term<-b[rx]/(dr[rx]+dl[j+1-rx])
          b[rx]<- saved + dr[rx]*term
          saved<- dl[j+1-rx]*term
        }
        b[j+1]<-saved
      }
        y[l,(i-K+1):i]<-b
        #for (o in 1:j)
        #{
        # y[l,(i-K+o):i]<-b[o]
        #}
    }
    print(y)
    return(y)
  }
  else {
    tt<-c(repmat(tKnots[1],1,K-2), tKnots, repmat(tKnots[N], 1, K-2))
    msp<- matrix(0, M, N+K-3)
  #msp-<matrix(0,M,(N+2*(K-2))-K))
   # y<- matrix(0, M, N+K-2)
    On<-t(rep(1,M))
    Aux<-(K-1)/(t(On) %*% (tt[K:(N+2*(K-2))]-tt[1:(N+K-3)]))
    msp<- Aux * bspl(xmat, K-1, tKnots, R-1)
    y[,1]<- -msp[,1]
    y[,2:(N+K-3)]<-msp[,1:(N+K-4)] - msp[, 2:(N+K-3)]
    y[,(N+K-2)]<-msp[, N+K-3]
  }
  
}

repmat <- function(a,n,m) 
{ 
  kronecker(matrix(1,n,m),a)
}
