computeSkewed <- function(Weights,correction){
  SortedWeights <- sort(Weights)
  
  n <- length(SortedWeights)
  n50 <- trunc(n/2)
  n10 <- trunc(n/10)
  n90 <- trunc(9*n/10)
  
  Skewness <- (SortedWeights[n90+1] - SortedWeights[n50+1])/(SortedWeights[n90+1]-SortedWeights[n10+1])
  
  return(1-correction*(1-Skewness))
  
}