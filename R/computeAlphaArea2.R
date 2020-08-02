computeAlphaArea2 <- function(Tri, Bins){
  
  area <- 0
  Tri <- matrix(Tri,ncol=3)
  
  for(i in 1:nrow(Tri)){
      p1 <- Bins[Tri[i,1],]
      p2 <- Bins[Tri[i,2],]
      p3 <- Bins[Tri[i,3],]
      
      area <- area + abs(p1[1]*p2[2] + p1[2]*p3[1] + p2[1]*p3[2] - p3[1]*p2[2] - p3[2]*p1[1] - p1[2]*p2[1])
    }
  
  return(area/2)
  
}