deleteEmptyBins <- function(count,xbin,ybin){
  k = 0
  
  for(i in 1:length(count)){
    if(count[i] > 0){
      k <- k+1
      count[k] <- count[i]
      xbin[k] <- xbin[i]
      ybin[k] <- ybin[i]
    }
  }
  return(list(k,count[1:k],xbin[1:k],ybin[1:k]))
}