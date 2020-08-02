GetFaces <- function(Simplex, IsUnique=TRUE){
  
  #Find all faces of each n-simplex in delaunay triangulation 
  #i.e. simplices of dimension n-1

    
  FaceDim <- ncol(Simplex) - 1
  Faces <- c()
  
  for(i in 1:nrow(Simplex)){
    temp <- combn(Simplex[i,],FaceDim,simplify=TRUE)
    Faces <- cbind(Faces,temp)
  }
  
  Faces <- t(Faces)
  
  if(IsUnique){ #Find all faces only referenced once (i.e. making up the surface of the alpha shape)
    FaceFrame <- Faces
    
    #for(i in 1:nrow(FaceFrame)){
    sortFrame <- function(i, FaceFrame){
      out <- sort(FaceFrame[i,])
    }
    
    sortedFaceFrame <- t(sapply(1:nrow(FaceFrame), sortFrame, FaceFrame=FaceFrame))
    
    index1 <- duplicated(sortedFaceFrame)
    index2 <- duplicated(sortedFaceFrame, fromLast=TRUE)
    index <- (index1 | index2)
    Simplices <- sortedFaceFrame[!index,]
    
    #Simplices <- sortedFaceFrame[!duplicated(sortedFaceFrame),]
    
    #FirstOrder <- order(FaceFrame[,1])
    #FaceFrame <- FaceFrame[FirstOrder,]
    
    #while(any(duplicated(FaceFrame))){
      
    #  FirstDuplicate <- which(duplicated(FaceFrame))[1]
      
    #  TempFace <- FaceFrame[FirstDuplicate,]
    #  TempFaceFrame <- FaceFrame[-FirstDuplicate,]
    #  TempFaceFrame <- rbind(TempFace,TempFaceFrame)
      
    #  FirstDuplicate <- which(duplicated(TempFaceFrame))[1]
      
    #  FaceFrame <- TempFaceFrame[-c(1,FirstDuplicate),]
      
    #}
    
     #Simplices <- FaceFrame
  }
  
  return(Simplices)
  
}
  
  