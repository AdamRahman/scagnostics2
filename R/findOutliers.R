findOutliers <- function(Bins, counts){

  n <- nrow(Bins)
  
  mstOut <- spantree(dist(Bins))
  MST <- data.frame(Parent=seq(2,nrow(Bins),1), Child=mstOut$kid, Weight=mstOut$dist)
  
  Parent <- MST$Parent
  Child <- MST$Child
  Weights <- MST$Weight

  BIndex <- seq(1,nrow(Bins),1) 
  
  cutoff <- computeOmega(Weights)
  TotalOriginalMSTLengths <- sum(Weights)
  totalMSTOutlierLengths <- 0
  
  #computeMSTOutliers Function
  
  found <- FALSE
  AlreadyCounted <- rep(FALSE,length(Weights))
  isOutlier <- rep(FALSE,nrow(Bins))
  
  for(i in 1:n){

    #Find All Neighbours of Node i in the MST
    Neighbours <- c(which(Parent == i),which(Child==i))
    
    delete <- TRUE
    
    if(any(Weights[Neighbours] < cutoff)){
      delete <- FALSE
    }
    
    if(delete){
      for(j in 1:length(Neighbours)){
        if(!AlreadyCounted[Neighbours[j]]){
          totalMSTOutlierLengths <- totalMSTOutlierLengths + Weights[Neighbours[j]]
          AlreadyCounted[Neighbours[j]] <- TRUE
        }
      }
      isOutlier[i] <- TRUE
      found <- TRUE
    }
  }
  
  #############################
  BIndex <- BIndex[!isOutlier]
  PeeledBins <- Bins
  PeeledMST <- MST
  
  while(found){
    PeeledBins <- PeeledBins[!isOutlier,]
    
    isOutlier <- rep(FALSE,nrow(PeeledBins))
    mstOut <- spantree(dist(PeeledBins))
    PeeledMST <- data.frame(Parent=seq(2,nrow(PeeledBins),1), Child=mstOut$kid, Weight=mstOut$dist)
    
    #PeeledMST <- Prim(PeeledBins)
    
    Parent <- PeeledMST$Parent
    Child <- PeeledMST$Child
    Weights <- PeeledMST$Weight
    
    cutoff <- computeOmega(Weights)
    
    #computeMSTOutliers Function
    
    found <- FALSE
    AlreadyCounted <- rep(FALSE,length(Weights))
    n <- nrow(PeeledBins)
    
    for(i in 1:n){
      
      #Find All Neighbours of Node i in the MST
      Neighbours <- c(which(Parent == i),which(Child==i))
      
      delete <- TRUE
      
      if(any(Weights[Neighbours] < cutoff)){
        delete <- FALSE
      }
      
      if(delete){
        for(j in 1:length(Neighbours)){
          if(!AlreadyCounted[Neighbours[j]]){
            totalMSTOutlierLengths <- totalMSTOutlierLengths + Weights[Neighbours[j]]
            AlreadyCounted[Neighbours[j]] <- TRUE
          }
        }
        isOutlier[i] <- TRUE
        found <- TRUE
      }
    }
    
    BIndex <- BIndex[!isOutlier]
    
  }
  
  return(list(BIndex,PeeledMST,totalMSTOutlierLengths))
  
}