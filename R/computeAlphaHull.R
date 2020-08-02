computeAlphaHull <- function(Bins, Alpha){
  
  #Compute the Delaunay Triangulation
  
  Del <- delaunayn(Bins, full=TRUE)
  
  DelTri <- Del$tri
  NumTri <- nrow(DelTri)
  
  Edges <- GetEdges(DelTri)
  Triangles <- seq(1,nrow(DelTri),1)
  
  Edges <- cbind(Edges,rep(1,nrow(Edges)))
  colnames(Edges) <- c("N1", "N2", "Tri", "InHull")
  Edges <- UniqueEdges(Edges)
  
  Triangles <- cbind(Triangles, rep(1,length(Triangles)))
  colnames(Triangles) <- c("Tri", "InHull")
  
  deleted <- TRUE
  
  while(deleted){
    deleted <- FALSE
    
    for(i in 1:nrow(Edges)){
      #if(Triangles[Edges[i,3],2]){
      if(Edges[i,4] == 1){ #Current edge is marked as being on the hull
        
        #Compute the edge weight
        N1 <- Edges[i,1]
        N2 <- Edges[i,2]
      
        Weight <- sqrt((Bins[N1,1] - Bins[N2,1])^2 + (Bins[N1,2] - Bins[N2,2])^2)
      
        if(Weight > 2*Alpha){ #If we can't draw a circle of radius alpha around the edge delete it
          deleted <- TRUE
          
          #Triangles[Edges[i,3],2] <- 0
          
          Edges[i,4] <- 0
        }else{
          if(!edgeIsExposed(Alpha,Edges[i,c(1,2)],Bins,Edges)){ #If edge is not alpha exposed delete it
            deleted <- TRUE
            #Triangles[Edges[i,3],2] <- 0
            Edges[i,4] <- 0
          }
        }
      }
    }
  }
  
  #for(i in 1:nrow(Edges)){
  #  if(!Triangles[Edges[i,3],2]){
  #    Edges[i,4] <- 0
  #  }
  #}

  EdgesOnHull <- Edges[which(Edges[,4] == 1),]
  #EdgesOnHull <- UniqueEdges(EdgesOnHull)
  
  #Temporarily Plot
  
  par(pty="s")
  plot(Bins, main="Edges on Alpha Hull")
  for(i in 1:nrow(EdgesOnHull)){
    segments(Bins[EdgesOnHull[i,1],1],Bins[EdgesOnHull[i,1],2],Bins[EdgesOnHull[i,2],1],Bins[EdgesOnHull[i,2],2], col="blue")
  }
  
  #Compute the area of the alpha hull
  #AlphaArea <- computeAlphaArea(Triangles, Edges, Bins)
  
  #Compute the Perimeter of the Alpha Hull
  #AlphaPerimeter <- computeAlphaPerimeter(EdgesOnHull, Bins)
  
  #return(list(area=AlphaArea, perimeter=AlphaPerimeter))
  
}

