computeClumpy <- function(MST, counts){
  
  Parent <- MST$Parent
  Child <- MST$Child
  Weight <- MST$Weight
  
  outFn <- function(i, MST, counts,Weight){
    Out <- GetRunts(MST,i, counts)
    runts <- Out[[1]]
    maxLength <- Out[[2]]
    if(maxLength > 0){
      value <- runts*(1-maxLength/Weight[i])
    }else{
      value <- 0
    }
    return(value)
  }
  
  values <- sapply(seq(1,nrow(MST),1),outFn, MST=MST,counts=counts,Weight=Weight)
  maxValue <- max(values)
  
  #maxValue <- 0
  
  #for(i in 1:length(Parent)){
  #  Out <- GetRunts(MST,i, counts)
  #  runts <- Out[[1]]
  #  maxLength <- Out[[2]]
  #  
  #  if(maxLength > 0){
  #    value <- runts*(1-maxLength/Weight[i])
  #    if(value > maxValue){
  #      maxValue <- value
  #    }
  #  }
  #}
  
  Clumpy <- 2*maxValue/(sum(counts))
  return(Clumpy)
  
}