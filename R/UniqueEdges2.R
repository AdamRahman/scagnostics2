UniqueEdges2 <- function(Edges){
  
  EdgesOn <- Edges[which(Edges[,4] == 1),]
  
  SortedEdgesOn <- EdgesOn
  
  for(i in 1:nrow(EdgesOn)){
    SortedEdgesOn[i,c(1,2)] <- sort(EdgesOn[i,c(1,2)])
  }
  
  FirstOrder <- order(SortedEdgesOn[,1])
  SortedEdgesOn <- SortedEdgesOn[FirstOrder,]
  
  UniqueEdges <- unique(SortedEdgesOn[,1])
  
  for(i in 1:length(UniqueEdges)){
    ToConsider <- which(SortedEdgesOn[,1] == UniqueEdges[i])
    EdgesToConsider <- SortedEdgesOn[ToConsider,]
    if(length(ToConsider) > 1){
      for(j in 1:length(ToConsider)){
        Num <- which(EdgesToConsider[,2] == EdgesToConsider[j,2])
        if(length(Num) > 1){
          SortedEdgesOn[ToConsider[Num],4] <- 0
        }
      }
    }
  }
  
  return(SortedEdgesOn[which(SortedEdgesOn[,4] == 1),])
}