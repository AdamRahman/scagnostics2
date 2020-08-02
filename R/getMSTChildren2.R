#MST = Entire minimum spanning tree
#i = current edge being worked on
#cutoff = weight of current edge
#ind = which node are we working on (the parent node (1) or the child node (2) of the edge)
#counts = binning counts

getMSTChildren2 <- function(MST, i, cutoff, ind, counts){
  
  #Starting Node for the runt graph (either the parent or child node of the edge under consideration)
  StartNode <- MST[i,ind]
  
  #Break MST at edge i
  TrimmedMST <- MST[-i,,drop=FALSE]
  
  #Break MST at all edges > cutoff
  Trim <- which(TrimmedMST$Weight > cutoff)
  TrimmedTree <- TrimmedMST[-Trim,,drop=FALSE]
  
  runtNodes <- findNodes(TrimmedTree,StartNode)
  nodes <- runtNodes$nodes
  weights <- runtNodes$weights
  
  if(length(nodes) > 1){
    runtCount <- sum(counts[nodes])
    maxLength <- max(weights)
  }else{
    runtCount <- 0
    maxLength <- 0
  }
  
  #Make a distance matrix
  #n <- nrow(MST) + 1
  
  #tempDist <- matrix(rep(Inf,n^2),nrow=n)
  #diag(tempDist) <- 0
  
  #for(j in 1:nrow(TrimmedTree)){
  #  tempDist[TrimmedTree$Parent[j],TrimmedTree$Child[j]] <- TrimmedTree$Weight[j]
  #  tempDist[TrimmedTree$Child[j],TrimmedTree$Parent[j]] <- TrimmedTree$Weight[j]
  #}
  
  #ThisRow <- tempDist[StartNode,]
  #ThisRow <- ThisRow[-StartNode]
  
  #if(!all(is.infinite(ThisRow))){
    #Find the minimum spanning tree path starting at the starting node
  #  NewMST <- primPath(tempDist,StartNode)
  
    #Cut Path at First Infinite Value or repetition
  #  CutHere <- min(which(is.infinite(NewMST[3,])))
  #  TrimmedPath <- NewMST[,seq(1,CutHere-1,1),drop=FALSE]
  
    #Get Unique Nodes Along Path
  #  UniqueNodes <- unique(c(TrimmedPath[1,],TrimmedPath[2,]))
  
    #Compute the runt count
  #  runtCount <- sum(counts[UniqueNodes])
  #  maxLength <- max(TrimmedPath[3,])
  #}else{
  #  runtCount <- 0
  #  maxLength <- 0
  #}
  return(list(runtCount, maxLength))
}