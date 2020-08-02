computeSkinny2 <- function(AlphaArea, AlphaPerimeter){
  
  if(AlphaPerimeter > 0){
    Skinny <- 1 - sqrt(4*pi*AlphaArea)/AlphaPerimeter
  }else{
    Skinny <- 1
  }
  return(Skinny)
}