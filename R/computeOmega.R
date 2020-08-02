computeOmega <- function(Weights){
  Weights <- sort(Weights)
  n <- length(Weights)
  n50 <- trunc(n/2) 
  n25 <- trunc(n50/2) 
  n75 <- n50 + n25
  
  omega <- Weights[n75+1] + 1.5*(Weights[n75+1] - Weights[n25+1])
  return(omega)
}