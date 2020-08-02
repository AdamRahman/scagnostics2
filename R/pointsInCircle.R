pointsInCircle <- function(Node, xc,yc, alpha, Edges, Bins){
  
  #Find the neighbours to our node
  alpha <- alpha*.999
  E1 <- Edges[which(Edges[,1] == Node),c(1,2),drop=FALSE]
  E2 <- Edges[which(Edges[,2] == Node),c(1,2),drop=FALSE]
  
  if(length(E1) > 0){
    
    #E1 <- matrix(E1, ncol=2)
    
    for(i in 1:nrow(E1)){
    
      x1 <- Bins[E1[i,2],1]
      y1 <- Bins[E1[i,2],2]
    
      DToNode <- sqrt((x1-xc)^2 + (y1-yc)^2)
    
      if(DToNode < alpha){
        return(TRUE)
      }
    }
  }
  
  if(length(E2) > 0){
    
    #E2 <- matrix(E2, ncol=2)
    
    for(i in 1:nrow(E2)){
    
      x1 <- Bins[E2[i,1],1]
      y1 <- Bins[E2[i,1],2]
    
      DToNode <- sqrt((x1-xc)^2 + (y1-yc)^2)
    
      if(DToNode < alpha){
        return(TRUE)
      }
    }
  }
  return(FALSE)
  
}