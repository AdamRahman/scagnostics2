computeAlphaArea <- function(Triangles, Edges, Bins){
  
  area <- 0
  
  for(i in 1:nrow(Triangles)){
    if(Triangles[i,2]){
      E <- Edges[which(Edges[,3] == i),c(1,2)]
      
      E1 <- E[1,]
      E2 <- E[2,]
      E3 <- E[3,]
      
      Points <- unique(c(E1,E2,E3))
      
      p1 <- Bins[Points[1],]
      p2 <- Bins[Points[2],]
      p3 <- Bins[Points[3],]
      
      area <- area + abs(p1[1]*p2[2] + p1[2]*p3[1] + p2[1]*p3[2] - p3[1]*p2[2] - p3[2]*p1[1] - p1[2]*p2[1])
    }
  }
  
  return(area/2)
  
}