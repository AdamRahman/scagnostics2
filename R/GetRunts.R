GetRunts <- function(MST,i,counts){
  
  Parent <- MST$Parent
  Child <- MST$Child
  Weight <- MST$Weight
  
  cutoff <- Weight[i]
  
  Out1 <- getMSTChildren2(MST, i, cutoff, ind=1, counts)
  count1 <- Out1[[1]]
  maxLength1 <- Out1[[2]]
  
  Out2 <- getMSTChildren2(MST, i, cutoff, ind=2, counts)
  count2 <- Out2[[1]]
  maxLength2 <- Out2[[2]]
  
  if(count1 < count2){
     maxLength <- maxLength1
    return(list(count1,maxLength))
  }else if(count1 == count2){
    if(maxLength1 < maxLength2){
      maxLength <- maxLength1
    }else{
      maxLength <- maxLength2
    }
    return(list(count1,maxLength))
  }else{
    maxLength <- maxLength2
    return(list(count2,maxLength))
  }
}