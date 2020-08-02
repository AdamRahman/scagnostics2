#2D Replication of original
computeMonotonic2 <- function(Bins, Counts, isOutlier){
  s <- computeSpearman(Bins,Counts, isOutlier)
  
  return(s^2)
}