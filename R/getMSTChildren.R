getMSTChildren <- function(MST, i, cutoff, ind="P", counts){
  
  maxLength <- 0
  count <- 0
  
  Parent <- MST$Parent
  OParent <- Parent
  Child <- MST$Child
  OChild <- Child
  ChildInd <- seq(1,length(Child),1)
  ParentInd <- seq(1,length(Parent),1)
  Weight <- MST$Weight
  
  if(ind == "P"){
    NewNode <- Parent[i]
    Parent <- Parent[-i]
    Child <- Child[-i]
    ChildInd <- ChildInd[-i]
  }else{
    NewNode <- Child[i]
    Parent <- Parent[-i]
    Child <- Child[-i]
    ChildInd <- ChildInd[-i]
  }
  
  NextNode <- TRUE
  InTree <- NewNode
  
  while(NextNode){
    NextNode <- FALSE
    for(i in 1:length(InTree)){
      if(length(Parent) > 0 & any(Parent == InTree[i])){
        NewNodeInd <- which(Parent == InTree[i])
        for(j in NewNodeInd){
          if(Weight[ParentInd[j]] < cutoff){
            NewNode <- OChild[ParentInd[j]]
            NewWeight <- Weight[ParentInd[j]]
            count <- count + counts[NewNode]
        
            InTree <- c(InTree,NewNode)
            NextNode <- TRUE
            if(NewWeight > maxLength){
              maxLength <- NewWeight
            }
          }
        }
        
        Parent <- Parent[-NewNodeInd]
        Child <- Child[-NewNodeInd]
        ChildInd <- ChildInd[-NewNodeInd]
        ParentInd <- ParentInd[-NewNodeInd]
        
      }
      
      if(length(Child) > 0 & any(Child == InTree[i])){
        NewNodeInd <- which(Child == InTree[i])
        for(j in NewNodeInd){
          if(Weight[ChildInd[j]] < cutoff){
            NewNode <- OParent[ChildInd[j]]
            NewWeight <- Weight[ChildInd[j]]
            count <- count + counts[NewNode]
            
            InTree <- c(InTree,NewNode)
            NextNode <- TRUE
            if(NewWeight > maxLength){
              maxLength <- NewWeight
            }
          }
        }
        
        Child <- Child[-NewNodeInd]
        Parent <- Parent[-NewNodeInd]
        ChildInd <- ChildInd[-NewNodeInd]
        ParentInd <- ParentInd[-NewNodeInd]
        
      }
    }
  }
  return(list(count,maxLength))
}