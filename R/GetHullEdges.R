GetHullEdges <- function(Edges, TrianglesOnComplex){
  
  n <- nrow(Edges)
  OnHull <- rep(FALSE,n)
  
  for(i in 1:n){
    #See if an inverse edge exists
    #Find Inverse Edge
    tempEdges <- Edges[-i,]
    
    ## Same Orientiation
    Candidate1 <- which(tempEdges[,1] == Edges[i,1])
    Candidate1 <- Candidate1[which(tempEdges[Candidate1,2] == Edges[i,2])]
    
    ##Opposite Orientation
    Candidate2 <- which(tempEdges[,2] == Edges[i,1])
    Candidate2 <- Candidate2[which(tempEdges[Candidate2,1] == Edges[i,2])]
    
    if(length(Candidate1) > 0){
      InverseEdgeExists <- TRUE
      InverseEdge <- Candidate1
    }else if(length(Candidate2) > 0){
      InverseEdgeExists <- TRUE
      InverseEdge <- Candidate2
    }else{
      InverseEdgeExists <- FALSE
    }
    
    if(!InverseEdgeExists){
      OnHull[i] <- TRUE
    }else if(TrianglesOnComplex[tempEdges[InverseEdge,3]] == 0){
      OnHull[i] <- TRUE
    }
  }
  
  return(OnHull)
    
}