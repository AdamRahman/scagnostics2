computeAlphaValue <- function(Weights){
  SortedWeights <- sort(Weights)
  n <- length(SortedWeights)
  
  n90 <- trunc(9*n/10)
  
  alpha <- SortedWeights[n90+1]
  
  return(min(alpha,100))
  
}