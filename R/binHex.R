binHex <- function(x,y,nBins){
  
  n <- length(x)
  
  #Scaling Constants
  
  con1 <- .25       #double
  con2 <- 1/3       #double
  c1 <- nBins - 1   #double
  c2 <- c1/sqrt(3)  #double
  jinc <- trunc(nBins)     #int
  iinc <- trunc(2*nBins)   #int
  nBin <- trunc((nBins+20)^2)      #int
  
  count <- numeric(nBin)
  xbin <- numeric(nBin)
  ybin <- numeric(nBin)
  
  #Fill Bins
  
  for(i in 1:n){
    
    xi <- x[i]
    yi <- y[i]
    
    if(is.na(xi)){
      next
    }
    if(is.na(yi)){
      next
    }
    
    sx <- c1*xi
    sy <- c2*yi
    i1 <- trunc(sy + .5)
    j1 <- trunc(sx + .5)
    dy <- sy - i1
    dx <- sx - j1
    
    dist1 <- dx^2 + 3*dy^2
    
    if(dist1 < con1){
      m <- trunc(i1*iinc+j1)
    }else if(dist1 > con1){
      m <- trunc(trunc(sy)*iinc + trunc(sx) + jinc)
    }else{
      i2 <- trunc(sy)
      j2 <- trunc(sx)
      dy <- sy - i2 - .5
      dx <- sx - j2 - .5
      dist2 <- dx^2 + 3*dy^2
      
      if(dist1 <= dist2){
        m <- trunc(i1*iinc+j1)
      }else{
        m <- trunc(i2*iinc+j2+jinc)
      }
      
    }
    count[m+1] <- count[m+1] + 1
    xbin[m+1] <- xbin[m+1] + (x[i] - xbin[m+1])/count[m+1]
    ybin[m+1] <- ybin[m+1] + (y[i] - ybin[m+1])/count[m+1]
  }
  
  return(list(xbin,ybin,count))
  
}