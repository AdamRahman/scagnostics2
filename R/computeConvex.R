computeConvex <- function(AlphaArea, ConvexArea, correction){
  if(ConvexArea == 0){
    Convex <- 1
  }else{
    Convex <- correction * (AlphaArea/ConvexArea)
  }
  
  return(Convex)
}