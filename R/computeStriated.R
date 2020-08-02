#Definition of Striated consistent with Paper "Scagnostic Distributions"

computeStriated <- function(MST, Bins){
  
  Parent <- MST$Parent
  Child <- MST$Child
  
  n <- length(Parent) + 1 #Number of Nodes
  CosInd <- 0
  
  for(i in 1:n){
    
    P <- which(Parent == i)
    C <- which(Child == i)
    
    C1 <- length(P) + length(C)
    
    if(C1 == 2){
      #In each calculation below, A represents the common node, while B and C are the nodes of the adjacent edges
      
      if(length(P) == 0){
        
        #(i.e. the child node is common)
        
        Axy <- Bins[Child[C[1]],] #= Bins[Child[C[2]],]
        Bxy <- Bins[Parent[C[1]],]
        Cxy <- Bins[Parent[C[2]],]
        
        Ax <- Axy[1]
        Ay <- Axy[2]
        Bx <- Bxy[1]
        By <- Bxy[2]
        Cx <- Cxy[1]
        Cy <- Cxy[2]
        
        AB <- sqrt((Ax-Bx)^2 + (Ay - By)^2)
        AC <- sqrt((Ax-Cx)^2 + (Ay - Cy)^2)
        BC <- sqrt((Bx-Cx)^2 + (By - Cy)^2)
        
      }else if(length(P) == 1){
        
        #(i.e. the child node in the first point is the same as the parent node in the second)
        
        Axy <- Bins[Child[C[1]],] #=Bins[Parent[P[1]],]
        Bxy <- Bins[Parent[C[1]],]
        Cxy <- Bins[Child[P[1]],]
        
        Ax <- Axy[1]
        Ay <- Axy[2]
        Bx <- Bxy[1]
        By <- Bxy[2]
        Cx <- Cxy[1]
        Cy <- Cxy[2]
        
        AB <- sqrt((Ax-Bx)^2 + (Ay - By)^2)
        AC <- sqrt((Ax-Cx)^2 + (Ay - Cy)^2)
        BC <- sqrt((Bx-Cx)^2 + (By - Cy)^2)
        
      }else{
        
        #(i.e. the parent node is common)
        Axy <- Bins[Parent[P[1]],] #=Bins[Parent[P[2]],]
        Bxy <- Bins[Child[P[1]],]
        Cxy <- Bins[Child[P[2]],]
        
        Ax <- Axy[1]
        Ay <- Axy[2]
        Bx <- Bxy[1]
        By <- Bxy[2]
        Cx <- Cxy[1]
        Cy <- Cxy[2]
        
        AB <- sqrt((Ax-Bx)^2 + (Ay - By)^2)
        AC <- sqrt((Ax-Cx)^2 + (Ay - Cy)^2)
        BC <- sqrt((Bx-Cx)^2 + (By - Cy)^2)
        
      }
      
      CosTheta <- (AB^2 + AC^2 - BC^2)/(2*AB*AC)
      
      if(CosTheta <= -.75){
        CosInd <- CosInd + 1
      }
    }
  }
  
  Striated <- CosInd/n
  
  return(Striated)
  
}