pcafd<- function ( fdaobj, nharm, plotting=FALSE) 
{
  
  if (!(inherits(fdaobj, "FdaClass"))) 
    stop("Argument FD  not a functional data object.")
  
  data<- fdaobj$data
  argvals<- fdaobj$argvals
  rangevals<- fdaobj$rangeval
  Data2FD <- Data2fd( argvals, data)
  PCA <- pca.fd(Data2FD, nharm, centerfns =FALSE)
  Harmnonics<-PCA$harmonics$coefs
  Scores<- PCA$scores
  values<- PCA$values
  varprop<- PCA$varprop
  if(plotting== TRUE)
  { plot(PCA)}
  return(list(Harmonics = Harmnonics, 
              Scores= Scores, Values= values, VarProp=varprop ))
  
}
