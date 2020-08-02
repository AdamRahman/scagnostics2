computeMonotonic <- function(Bins, Counts){
  
  #Compute the correlation matrix
  #CorrMat
  n <- ncol(Bins)
  
  CorrMat <- matrix(rep(0,n^2), ncol=n)
  diag(CorrMat) <- 1
  
  #Fill in the lower triangular entries
  for(i in 2:n){
    for(j in 1:(i-1)){
      X <- Bins[,i]
      Y <- Bins[,j]
      XY <- cbind(X,Y)
      
      CorrMat[i,j] <- computeSpearman(XY,Counts, rep(0, nrow(XY)))
      CorrMat[j,i] <- CorrMat[i,j]
      
    }
  }
  
  #Find the determinant
  #If completely uncorrelated, determinant will be 1, otherwise < 1
  #Returns same as original computeMonotonic for 2d ==> 
  #Determ = 1 - rho^2, return 1 - (1-rho^2) = rho^2 = original monotonic 
  Eigs <- eigen(CorrMat,symmetric=TRUE, only.values=TRUE)$values
  Determ <- prod(Eigs)
  
  return(1-Determ)
}