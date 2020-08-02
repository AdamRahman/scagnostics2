Binner <- function(Data,nBins,maxBins){
  x <- Data[,1]
  y <- Data[,2]
  
  Out <- binHex(x,y,nBins)
  
  count <- Out[[3]]
  xbin <- Out[[1]]
  ybin <- Out[[2]]
  
  Out2 <- deleteEmptyBins(count,xbin,ybin)
  nBin <- Out2[[1]]
  count <- Out2[[2]]
  xbin <- Out2[[3]]
  ybin <- Out2[[4]]
  
  while(nBin > maxBins){
    nBins <- 2*nBins/3
    
    Out <- binHex(xbin,ybin,nBins)
    count <- Out[[3]]
    xbin <- Out[[1]]
    ybin <- Out[[2]]
    
    Out2 <- deleteEmptyBins(count,xbin,ybin)
    nBin <- Out2[[1]]
    count <- Out2[[2]]
    xbin <- Out2[[3]]
    ybin <- Out2[[4]]
  }
  
  return(list(xbin=xbin,ybin=ybin,count=count))
  
}