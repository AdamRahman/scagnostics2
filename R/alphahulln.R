alphahulln <- function(Bins, Alpha){

  #delaunay triangulation 
  
  Del <- delaunayn(Bins, full=TRUE)
  
  DelTri <- Del$tri
  SimAreas <- Del$areas
  NumTri <- nrow(DelTri)
  
  #Include only Triangles with circumradius < alpha
  
  SimplexInHull <- c()
  InSims <- c()
  
  #for(i in 1:NumTri){
  GetAlphaSimplex <- function(i, Bins, DelTri, Alpha){
    NodesInSim <- Bins[DelTri[i,],]
    
    DM1 <- as.matrix(dist(NodesInSim)^2)
    
    DM2 <- cbind(rep(1,nrow(DM1)),DM1)
    DM2 <- rbind(c(0,rep(1,ncol(DM1))),DM2)
    
    #Eigenvalues
    #EigDM1 <- eigen(DM1,symmetric=TRUE, only.values=TRUE)$values
    #EigDM2 <- eigen(DM2, symmetric=TRUE, only.values=TRUE)$values
    
    #EigRatio <- c(EigDM1,1)/EigDM2
    
    #R <- sqrt(-.5*prod(EigRatio))
    
    R <- sqrt(-.5*det(DM1)/det(DM2))
    
    if(R < Alpha){
      #SimplexInHull <- rbind(SimplexInHull,DelTri[i,])
      #InSims <- c(InSims,i)
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  out <- sapply(1:NumTri, GetAlphaSimplex, Bins=Bins, DelTri=DelTri, Alpha=Alpha)  
  
  InSims <- which(out == TRUE)
  if(length(InSims) > 0){
  SimplexInHull <- DelTri[InSims,,drop=FALSE]
    
  #The n-dimensional volume of the n-dim alpha hull is
  AlphaNVol <- sum(SimAreas[InSims])
  }else{
    SimplexInHull <- NULL
    AlphaNVol <- 0
  }
  
  #The n-dimensional surface area is equivalent to the (n-1) dimensional volume of the simplices
  #making up the "surface" of the alpha hull
  
  #Find all simplices only referenced once
  if(!is.null(SimplexInHull)){
    SimplexFaces <- GetFaces(SimplexInHull, IsUnique=TRUE)
  
    #Compute the Surface Volume of the Alpha Hull
  
    j <- ncol(Bins)-1 #Dimension of simplices making up surface
    #SurfaceVolume <- 0
  
    #for(i in 1:nrow(SimplexFaces)){
    ComputeVolume <- function(i, Bins, SimplexFaces, j){
    
      NodesInSim <- Bins[SimplexFaces[i,],]
    
      DM <- as.matrix(dist(NodesInSim)^2)
    
      DM <- cbind(rep(1,nrow(DM)),DM)
      DM <- rbind(c(0,rep(1,(ncol(DM)-1))),DM)
    
      #Eigenvalues
      EigDM <- eigen(DM,symmetric=TRUE, only.values=TRUE)$values
    
      #SurfaceVolume <- SurfaceVolume + sqrt((-1)^(j+1)/(2^j*factorial(j)^2)*prod(EigDM)) #Cayley-Menger Volume
      SurfaceVolume <- sqrt((-1)^(j+1)/(2^j*factorial(j)^2)*prod(EigDM)) #Cayley-Menger Volume
      return(SurfaceVolume)
    }
      
    out <- sapply(1:nrow(SimplexFaces), ComputeVolume, Bins=Bins, SimplexFaces=SimplexFaces, j=j)
    SurfaceVolume <- sum(out)
      
  }else{
    SurfaceVolume <- 0
  }
  
  return(list(NVolume = AlphaNVol, NSurfaceVolume = SurfaceVolume))
  
}