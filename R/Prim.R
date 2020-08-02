Prim <- function(Data){
  
  #Make a Distance Matrix of "Edges" - we are always dealing with a complete graph
  DM <- as.matrix(dist(Data))
  diag(DM) <- Inf
  
  #Some initial parameters
  n <- nrow(DM)
  
  #Initialize Vectors of size n-1
  
  Parent <- numeric(n-1)
  Child <- numeric(n-1)
  Edge <- numeric(n-1)
  InMST <- c()
  NotInMST <- seq(1,n,1)
  
  #Initialize at Node 1
  
  Parent[1] <- 1
  Child[1] <- which.min(DM[1,])
  Edge[1] <- DM[Parent[1],Child[1]]
  InMST <- c(InMST,Parent[1],Child[1])
  NotInMST <- NotInMST[-c(Parent[1],Child[1])]
  
  #Fill in the Remaining Nodes
  
  for(i in 1:(n-2)){
    #Extract the rows of all nodes currently in the MST
    InNodes <- DM[InMST,]
    
    #Extract the columns that are not currently in the MST
    AvailableNodes <- InNodes[,NotInMST]
    
    #Find the Max Value in each row
    RowMin <- sapply(seq_len(nrow(as.matrix(AvailableNodes))), function(x)min(as.matrix(AvailableNodes)[x,]))
    
    #Find the Min Min
    MinMin <- which.min(RowMin)
    
    #Find Out where that max was
    NewParent <- InMST[MinMin]
    NewChild <- as.numeric(which(DM[InMST[MinMin],] == min(RowMin))[1])
    NewEdge <- DM[NewParent,NewChild]
    
    #Add Them to the current vectors
    
    Parent[i+1] <- NewParent
    Child[i+1] <- NewChild
    Edge[i+1] <- NewEdge
    
    #Update All Pointers
    
    InMST <- c(InMST,NewChild)
    NotInMST <- NotInMST[-which(NotInMST == NewChild)]
  }
  
  Ordered <- order(Child)
  temp <- Parent
  Parent <- Child[Ordered]
  Child <- temp[Ordered]
  Weight <- Edge[Ordered]
  
  MST <- data.frame(Parent=Parent,Child=Child,Weight=Weight)
  return(MST)
}