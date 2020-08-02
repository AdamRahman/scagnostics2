setClass("scagnosticObject", representation(ScaledData = "matrix",
                                            BinnedData = "list",
                                            OriginalMST = "data.frame",
                                            Outliers = "list",
                                            ConvexHull = "list",
                                            AlphaHull = "list",
                                            Scagnostics = "matrix"))


scagnostics2 <- function(Data,
                         scagObj = NULL,
                         compute = "All"){
  
  
  if(is.null(scagObj)){
    scagObj <- new("scagnosticObject")
    Scagnostics <- matrix(rep(NA,9),nrow=1)
    colnames(Scagnostics) <- c("Outlying", "Skewed", "Clumpy", "Sparse", "Striated", "Convex", "Skinny", "Stringy", "Monotonic")
  }else{
    Scagnostics <- scagObj@Scagnostics
  }
  
  if(compute == "All"){
    compute <- c("Outlying", "Skewed", "Clumpy", "Sparse", "Striated", "Convex", "Skinny", "Stringy", "Monotonic")
  }
  
  #Scaling
  if(nrow(scagObj@ScaledData) == 0){  #If the scagnostic object does not include scaled data, scale the data and store it
    ScaledData <- Scale(Data)
    scagObj@ScaledData <- ScaledData
  }else{                              #Otherwise, transfer the scaled data
    ScaledData <- scagObj@ScaledData
  }
  
  #Binning
  if(length(scagObj@BinnedData) == 0){  #If the scagnostic object does not included binned data, bin the data and store it
  Out <- Binner(ScaledData,50,1000)
  BinnedData <- as.matrix(cbind(Out$xbin,Out$ybin))
  counts <- Out$count
  
  scagObj@BinnedData <- Out
  }else{                                #Otherwise, transfer the binned data
    Out <- scagObj@BinnedData
    BinnedData <- as.matrix(cbind(Out$xbin,Out$ybin))
    counts <- Out$count
  }
  
  #Integerize Data
  Resolution <- 1000
  BinnedData <- Resolution * BinnedData
  BinnedData <- cbind(as.integer(BinnedData[,1]),as.integer(BinnedData[,2]))
  
  totalCount <- sum(counts)
  
  #Until Proper Documentation is created, "Random" is just an array of 0s, indicating where 
  #the random number generation was used in the original implementation
  set.seed(1337)
  Random <- matrix(trunc(8*(runif(nrow(BinnedData)*ncol(BinnedData),0,1)-.5)), nrow=nrow(BinnedData))
  
  #MST
  if(nrow(scagObj@OriginalMST) == 0  &&                                                  #If no MST info in scagObj
     any(compute %in% c("Outlying","Skewed","Clumpy","Sparse","Striated","Stringy", "Skinny"))){   #and MST is needed for desired scagnostics
    mstOut <- spantree(dist(BinnedData + Random))
    MST <- data.frame(Parent=seq(2,nrow(BinnedData),1), Child=mstOut$kid, Weight=mstOut$dist)
  
    scagObj@OriginalMST <- MST
  }else if(nrow(scagObj@OriginalMST) != 0){ #If we have MST info, extract it, even if we don't necessarily need it
    MST <- scagObj@OriginalMST
  }
    
    
  #Find Outliers
  if(length(scagObj@Outliers) == 0){
    IsOutlier <- findOutliers(BinnedData + Random, counts)
    scagObj@Outliers <- IsOutlier
  }else{
    IsOutlier <- scagObj@Outliers
  }
  
  PeeledBins <- BinnedData[IsOutlier[[1]],] + Random[IsOutlier[[1]],]
  PeeledCounts <- counts[IsOutlier[[1]]]
  PeeledMST <- IsOutlier[[2]]
  totalMSTOutlierLengths <- IsOutlier[[3]]
  
  #Convex Hull
  #Returns m x dim matrix, each row represents a dim-dimensional "triangle" making up the surface of the convex hull
  #i.e. edges in 2d, triangles in 3d, etc.
  
  if(length(scagObj@ConvexHull) == 0 &&
     any("Convex" %in% compute)){
    
    ConvexHull <- convhulln(as.matrix(PeeledBins), "FA")
    scagObj@ConvexHull <- ConvexHull
  
    ConvexArea <- ConvexHull$vol
    ConvexPerimeter <- ConvexHull$area
  }else if(length(scagObj@ConvexHull) != 0){
    ConvexHull <- scagObj@ConvexHull
    
    ConvexArea <- ConvexHull$vol
    ConvexPerimeter <- ConvexHull$area
  }
  
  #Alpha Hull
  
  if(length(scagObj@AlphaHull) == 0 &&
     any(compute %in% c("Convex","Skinny"))){
    
    AlphaHull <- computeAlphaHull2(PeeledBins,Alpha=computeAlphaValue(MST$Weight))
    scagObj@AlphaHull <- AlphaHull
  
    AlphaArea <- AlphaHull[[1]]
    AlphaPerimeter <- AlphaHull[[2]]
    
  }else if(length(scagObj@AlphaHull) != 0){
    AlphaHull <- scagObj@AlphaHull
    
    AlphaArea <- AlphaHull[[1]]
    AlphaPerimeter <- AlphaHull[[2]]
  }
  
  #Correction Calculation
  t <- totalCount/500 
  correction <- .7 + .3/(1+t^2)
  
  #Compute Scagnostics
  
  #Outlying
  if("Outlying" %in% compute){
    Outlying <- computeOutlying(MST$Weight,totalMSTOutlierLengths) 
    Scagnostics[1] <- Outlying
  }
  
  #Skewed
  if("Skewed" %in% compute){
    Skewed <- computeSkewed(MST$Weight,correction)
    Scagnostics[2] <- Skewed
  }
  
  #Clumpy
  if("Clumpy" %in% compute){
    Clumpy <- computeClumpy(PeeledMST, PeeledCounts)
    Scagnostics[3] <- Clumpy
  }
  
  #Sparse
  if("Sparse" %in% compute){
    Sparse <- computeSparse(MST$Weight,correction,BinnedData)
    Scagnostics[4] <- Sparse
  }
  
  #Striated
  if("Striated" %in% compute){
    #Striated <- computeStriated(MSTNoOutliers, BinnedData) #Paper Version
    Striated <- computeStriated2(PeeledMST, PeeledBins) #Java Version
    Scagnostics[5] <- Striated
  }
  
  #Convex
  if("Convex" %in% compute){
    Convex <- computeConvex(AlphaArea, ConvexArea, correction)
    Scagnostics[6] <- Convex
  }
  
  #Skinny
  if("Skinny" %in% compute){
    Skinny <- computeSkinny2(AlphaArea, AlphaPerimeter)
    Scagnostics[7] <- Skinny
  }
  
  #Stringy
  if("Stringy" %in% compute){
    Stringy <- computeStringy(PeeledMST)
    Scagnostics[8] <- Stringy
  }
  
  #Monotonic
  if("Monotonic" %in% compute){
    #Monotonic <- computeMonotonic2(PeeledBins,PeeledCounts)
    Nodes <- seq(1,nrow(BinnedData),1)
    isOutlying <- !(Nodes %in% IsOutlier[[1]])
    Monotonic <- computeMonotonic2(BinnedData,counts, isOutlying)
    Scagnostics[9] <- Monotonic
  }
  
  scagObj@Scagnostics <- Scagnostics
  
  #Scags <- matrix(c(Outlying, Skewed, Clumpy, Sparse, Striated, Convex, Skinny, Stringy, Monotonic),nrow=1)
  #colnames(Scags) <- c("Outlying", "Skewed", "Clumpy", "Sparse", "Striated", "Convex", "Skinny", "Stringy", "Monotonic")
  
  return(list(Scagnostics,scagObj))

}