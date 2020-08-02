computeAlphaPerimeter <- function(Edges, Bins){
  
  perimeter <- 0
  
  for(i in 1:nrow(Edges)){
    if(Edges[i,4]){
      
      Edge <- Edges[i,c(1,2)]
      
      perimeter <- perimeter + sqrt((Bins[Edge[1],1] - Bins[Edge[2],1])^2 + (Bins[Edge[1],2] - Bins[Edge[2],2])^2)
    }
  }
  
  return(perimeter)
  
}