#Definition of Striated consistent with Java Implementation

computeStriated2 <- function(MST, Bins){
  
  Parent <- MST$Parent
  Child <- MST$Child
  
  n <- length(Parent) #Number of Edges
  CosInd <- 0
  
  for(i in 1:n){
    
    PNode <- Parent[i]
    CNode <- Child[i]
    
    P1 <- which(Parent == PNode)
    C1 <- which(Child == PNode)
    
    Degree1 <- length(P1) + length(C1)
    
    P2 <- which(Parent == CNode)
    C2 <- which(Child == CNode)
    
    Degree2 <- length(P2) + length(C2)
    
    if(Degree1 == 2 & Degree2 == 2){
      
      #Compute the angle for the parent node
        
      A <- Bins[PNode,] #Common Node
      B <- Bins[CNode,] #Child on known Edge
      C <- Bins[Parent[C1],] #Parent node for which the common node is the child
        
      Ax <- A[1]
      Ay <- A[2]
      Bx <- B[1]
      By <- B[2]
      Cx <- C[1]
      Cy <- C[2]
        
      AB <- sqrt((Ax-Bx)^2 + (Ay - By)^2)
      AC <- sqrt((Ax-Cx)^2 + (Ay - Cy)^2)
      BC <- sqrt((Bx-Cx)^2 + (By - Cy)^2)
      
      CosTheta1 <- (AB^2 + AC^2 - BC^2)/(2*AB*AC)
      
      #Compute the angle for the child node
      
      A <- Bins[CNode,] #Common Node
      B <- Bins[PNode,] #Parent on known edge
      if(length(P2) == 1){
        C <- Bins[Child[P2],] #Child node for which the common node is the Parent
      }else if(length(C2) == 2){
        C2 <- C2[-which(Parent[C2] == PNode)]
        C <- Bins[Parent[C2],] #Parent node for which the common node is the Child
      }else{
        stop("Error")
      }
      
      Ax <- A[1]
      Ay <- A[2]
      Bx <- B[1]
      By <- B[2]
      Cx <- C[1]
      Cy <- C[2]
      
      AB <- sqrt((Ax-Bx)^2 + (Ay - By)^2)
      AC <- sqrt((Ax-Cx)^2 + (Ay - Cy)^2)
      BC <- sqrt((Bx-Cx)^2 + (By - Cy)^2)
      
      CosTheta2 <- (AB^2 + AC^2 - BC^2)/(2*AB*AC)
      
      if(CosTheta1 <= -.7 & CosTheta2 <= -.7){
        CosInd <- CosInd + 1
      }
    }
  }
  
  Striated <- CosInd/n
  
  return(Striated)
  
}