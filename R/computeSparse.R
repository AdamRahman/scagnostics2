computeSparse <- function(Weights,correction, Bins){
  SortedWeights <- sort(Weights)
  n <- length(Weights)
  n90 <- trunc(9*n/10)
  
  #Scale by Max Frame Size
  #R <- max(apply(Bins,2,max) - apply(Bins,2,min))
  R <- 1000
  
  sparse <- min(SortedWeights[n90+1]/R,1)
  return(correction*sparse)
}