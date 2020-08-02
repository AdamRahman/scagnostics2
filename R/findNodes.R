findNodes <- function(TrimmedMST, StartNode){
  
  CurrentNodes <- StartNode
  Weights <- c()
  Added <- TRUE
  
  GetRuntEdges <- function(Node, MST){
    NewEdges1 <- which(MST[,1] == Node)
    NewEdges2 <- which(MST[,2] == Node)
    
    return(c(NewEdges1,NewEdges2))
  }
  
  while(Added & nrow(TrimmedMST) > 0){
    NewEdges <- unlist(sapply(CurrentNodes,GetRuntEdges, MST=TrimmedMST))
    
    if(length(NewEdges) > 0){
      Weights <- c(Weights,TrimmedMST$Weight[NewEdges])
      CurrentNodes <- unique(c(CurrentNodes,TrimmedMST[NewEdges,1],TrimmedMST[NewEdges,2]))
      TrimmedMST <- TrimmedMST[-NewEdges, ,drop=FALSE]
    }else{
      Added <- FALSE
    }
  }
  
  return(list(nodes=CurrentNodes,weights=Weights))
  
}