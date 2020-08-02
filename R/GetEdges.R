GetEdges <- function(Delaunay){
  EdgeList <- c()
  
  Delaunay <- matrix(Delaunay, ncol=3)
  
  for(i in 1:nrow(Delaunay)){
    
    DT <- Delaunay[i,]
    
    E1 <- c(DT[1], DT[2])
    E2 <- c(DT[1], DT[3])
    E3 <- c(DT[2], DT[3])
    
    NewEdges <- rbind(E1,E2,E3)
    NewEdges <- cbind(NewEdges,rep(i,3))
    
    EdgeList <- rbind(EdgeList,NewEdges)
  }
  
  return(EdgeList)
  
}