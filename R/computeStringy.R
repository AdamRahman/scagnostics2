computeStringy <- function(MST){
  Parent <- MST$Parent
  Child <- MST$Child
  
  NumNodes <- length(Parent) + 1 
  
  Counts <- numeric(NumNodes)
  
  Ident <- matrix(rep(0,NumNodes^2),nrow=NumNodes)
  
  for(i in 1:length(Parent)){
    Ident[Parent[i],Child[i]] <- 1
    Ident[Child[i],Parent[i]] <- 1
  }
  
  Counts <- rowSums(Ident)
  
  OneCounts <- length(which(Counts == 1))
  TwoCounts <- length(which(Counts == 2))
  
  Stringy <- TwoCounts/((NumNodes) - OneCounts)
  
  return(Stringy^3)
  
}