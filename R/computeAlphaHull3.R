#Regularized alpha shape with any interior holes filled

computeAlphaHull3 <- function(Bins, Alpha){
  
  Del <- delaunayn(Bins, full=TRUE)
  
  DelTri <- Del$tri
  NumTri <- nrow(DelTri)
  Edges <- GetEdges(DelTri)
  
  Edges <- cbind(Edges,rep(1,nrow(Edges)))
  DelTri <- cbind(DelTri,rep(1,NumTri))
  
  TrianglesInHull <- c()
  
  SortedEdges <- Edges
  
  for(i in 1:nrow(Edges)){
    SortedEdges[i,c(1,2)] <- sort(Edges[i,c(1,2)])
  }
  
  FirstOrder <- order(SortedEdges[,1])
  Edges <- SortedEdges[FirstOrder,]

  deleted <- TRUE
  
  while(deleted){
    
    deleted <- FALSE
    ToRemove <- c()
    
    for(i in 1:nrow(DelTri)){
      if(DelTri[i,4]){
      a <- sqrt((Bins[DelTri[i,1],][1] - Bins[DelTri[i,2],][1])^2 + (Bins[DelTri[i,1],][2] - Bins[DelTri[i,2],][2])^2)
      b <- sqrt((Bins[DelTri[i,1],][1] - Bins[DelTri[i,3],][1])^2 + (Bins[DelTri[i,1],][2] - Bins[DelTri[i,3],][2])^2)
      c <- sqrt((Bins[DelTri[i,2],][1] - Bins[DelTri[i,3],][1])^2 + (Bins[DelTri[i,2],][2] - Bins[DelTri[i,3],][2])^2)
    
      R <- a*b*c/sqrt((a+b+c)*(b+c-a)*(c+a-b)*(a+b-c))
      
      Free <- 0
      CandidateEdge <- Edges[which(Edges[,3] == i),c(1,2)]
      
      for(j in 1:3){
        NewCE <- CandidateEdge[j,]
        
        CandidateMatch <- which(Edges[,1] == NewCE[1])
        NotInHull <- which(Edges[CandidateMatch,4] == 0)
        if(length(NotInHull) > 0){
          CandidateMatch <- CandidateMatch[-which(Edges[CandidateMatch,4] == 0)]
        }
        ExactMatch <- which(Edges[CandidateMatch,2] == NewCE[2])

        if(length(ExactMatch) == 1){
          Free <- Free + 1
        }
      }

      if(Free == 1 | Free == 2){
        Exterior <- TRUE
      }else{
        Exterior <- FALSE
      }
    
      if(R > Alpha && Exterior){
        #TrianglesInHull <- rbind(TrianglesInHull,DelTri[i,])
      #}else{
        ToRemove <- c(ToRemove,i)
        Edges[which(Edges[,3] == i),4] <- 0
        DelTri[i,4] <- 0
        deleted <- TRUE
      }
    }
    }
  }
  
  TrianglesInHull <- DelTri[which(DelTri[,4] == 1),]
  
  #Find the Edges Referenced by Only one triangle
  
  EdgesInHull <- GetEdges(TrianglesInHull)
  EdgesInHull <- cbind(EdgesInHull,rep(1,nrow(EdgesInHull)))
  
  EdgesInHull <- UniqueEdges2(EdgesInHull)
  
  #Temporary Plot
  #plot(Bins,pch=20,xlab="",ylab="")
  #for(i in 1:nrow(EdgesInHull)){
  #  segments(Bins[EdgesInHull[i,1],1],Bins[EdgesInHull[i,1],2],Bins[EdgesInHull[i,2],1],Bins[EdgesInHull[i,2],2],col="blue")
  #}
  
  #Compute the Area of the Alpha Hull
  AlphaArea <- computeAlphaArea2(TrianglesInHull, Bins)
  
  #Compute the Perimeter of the Alpha Hull
  AlphaPerimeter <- computeAlphaPerimeter(EdgesInHull, Bins)
  
  return(list(AlphaArea,AlphaPerimeter))
  
}