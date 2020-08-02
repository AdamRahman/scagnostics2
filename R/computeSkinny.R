computeSkinny <- function(AlphaArea, AlphaPerimeter,Bins){
  
  #in general, the volume of an n-sphere with radius R is given by:
  #V_{n} = pi^{n/2}*r^{n}/(gamma(n/2 + 1))
  #The surface area is given by:
  #S_{n} = 2*pi^{(n+1)/2}r^{n}/gamma((n+1)/2)
  
  #Take  V_{n}^{1/n}/S_{n}^{1/(n-1)}
  
  #We want to find a constant such that S_{n-1}/sqrt(V_{n}) = 1
  
  n <- ncol(Bins) #Dimension of hypersphere
  R <- max(apply(Bins,2,max) - apply(Bins,2,min)) #If points were on the sphere, this is the maximum possible radius for that sphere
  
  SA <- 2*pi^(n/2)*R^{n-1}/gamma(n/2)
  V <- pi^{n/2}*R^{n}/gamma(n/2+1)
  
  Scale <- SA/sqrt(V)
  
  if(AlphaPerimeter > 0){
    
    Skinny <- 1 - Scale*sqrt(AlphaArea)/AlphaPerimeter
  }else{
    Skinny <- 1
  }
  
  return(Skinny)
}