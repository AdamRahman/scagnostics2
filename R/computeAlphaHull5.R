computeAlphaHull5 <- function(Bins, Alpha){
  
  Del <- delaunayn(Bins, full=TRUE)
  
  DelTri <- Del$tri
  NumTri <- nrow(DelTri)
  
  Edges <- GetEdges(DelTri)
  colnames(Edges) <- c("N1", "N2", "Tri")
  
  Triangles <- seq(1,nrow(DelTri),1)
  TrianglesOnComplex <- rep(1,nrow(DelTri))
  
  deleted <- TRUE
  
  while(deleted){
    deleted <- FALSE
    
    for(i in 1:nrow(Edges)){
      if(TrianglesOnComplex[Edges[i,3]] == 1){
        #Compute Edge Weight
        n1 <- Bins[Edges[i,1],]
        n2 <- Bins[Edges[i,2],]
        Weight <- sqrt(sum((n1-n2)^2))
        
        if(Alpha < Weight/2){
          TrianglesOnComplex[Edges[i,3]] <- 0
          deleted <- TRUE
          #print("Alpha < Weight/2")
        }else{
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
          
          if(InverseEdgeExists){
            if(TrianglesOnComplex[tempEdges[InverseEdge,3]] == 1){
              next
            }
          }
          
          if(!edgeIsExposed(Alpha, Edges[i,c(1,2)],Bins,Edges)){
            TrianglesOnComplex[Edges[i,3]] <- 0
            deleted <- TRUE
            #print("Alpha Circle Non-Empty")
          }
        }
      }
    }
  }
  
  onTriangles <- Triangles[which(TrianglesOnComplex == 1)]
  
  if(length(onTriangles) > 0){
    #Grab only the edges that are part of the alpha hull
    onEdges <- GetEdges(DelTri[onTriangles,])

    #Grab the edges that are on the hull
    onHull <- GetHullEdges(onEdges, TrianglesOnComplex)
    HullEdges <- cbind(onEdges,onHull)
    
    #Compute the Area of the Alpha Hull
    AlphaArea <- computeAlphaArea2(DelTri[onTriangles,], Bins)
    
    #Compute the Perimeter of the Alpha Hull
    AlphaPerimeter <- computeAlphaPerimeter(HullEdges, Bins)
  }else{
    AlphaArea <- 0
    AlphaPerimeter <- 0
  }
  
  return(list(Area = AlphaArea, Perimeter = AlphaPerimeter))
  
}