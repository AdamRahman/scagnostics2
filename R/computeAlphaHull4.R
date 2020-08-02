#Peeling the convex hull to obtain the alpha hull
#method used in original scagnostics package

computeAlphaHull4 <- function(Bins,Alpha){
  
  #Compute the Delaunay Triangulation
  
  Del <- delaunayn(Bins, full=TRUE)
  
  DelTri <- Del$tri
  NumTri <- nrow(DelTri)
  
  Edges <- GetEdges(DelTri)
  Triangles <- seq(1,nrow(DelTri),1)
  
  Edges <- cbind(Edges,rep(1,nrow(Edges)))
  colnames(Edges) <- c("N1", "N2", "Tri", "InHull")
  HullEdges <- UniqueEdges2(Edges[which(Edges[,4] == 1),])
  
  Triangles <- cbind(Triangles, rep(1,length(Triangles)))
  colnames(Triangles) <- c("Tri", "InHull")
  
  deleted <- TRUE
  
  while(deleted){
    deleted <- FALSE
    
    for(i in 1:nrow(HullEdges)){
      if(HullEdges[i,4] == 1){ #Current edge is marked as being on the hull
        
        #Compute the edge weight
        N1 <- HullEdges[i,1]
        N2 <- HullEdges[i,2]
        
        Weight <- sqrt((Bins[N1,1] - Bins[N2,1])^2 + (Bins[N1,2] - Bins[N2,2])^2)
        
        if(Weight > 2*Alpha){ #If we can't draw a circle of radius alpha around the edge delete it
          deleted <- TRUE
          
          Triangles[HullEdges[i,3],2] <- 0 #Remove the triangle in which that edge belongs
          Edges[which(Edges[,3] == HullEdges[i,3]),4] <- 0 #Remove all edges in that triangle
        }else{
          if(!edgeIsExposed(Alpha,HullEdges[i,c(1,2)],Bins,Edges)){ #If edge is not alpha exposed delete it
            deleted <- TRUE
            Triangles[HullEdges[i,3],2] <- 0 #Remove the triangle in which that edge belongs
            Edges[which(Edges[,3] == HullEdges[i,3] ),4] <- 0 #Remove all edges in that triangle
          }
        }
      }
    }
    
    HullEdges <- UniqueEdges2(Edges[which(Edges[,4] == 1),])
    
  }
  
  TrianglesInHull <- Triangles[which(Triangles[,2] ==1),]
  
  TrianglesInHull2 <- DelTri[TrianglesInHull[,1],]
  
  #Temporarily Plot
  
  #par(pty="s")
  plot(Bins, main="Edges on Alpha Hull")
  for(i in 1:nrow(HullEdges)){
    segments(Bins[HullEdges[i,1],1],Bins[HullEdges[i,1],2],Bins[HullEdges[i,2],1],Bins[HullEdges[i,2],2], col="blue")
  }
  
  #Compute the area of the alpha hull
  AlphaArea <- computeAlphaArea(TrianglesInHull, Edges, Bins)
  
  #Compute the Perimeter of the Alpha Hull
  AlphaPerimeter <- computeAlphaPerimeter(HullEdges, Bins)
  
  return(list(area=AlphaArea, perimeter=AlphaPerimeter))
  
}