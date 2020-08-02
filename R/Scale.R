Scale <- function(Data){
  
  #Find the Max of Each column
  MaxVec <- sapply(seq_len(ncol(Data)), function(x,Data)max(Data[,x]), Data=Data)
  
  #Find the Min of each Column
  MinVec <- sapply(seq_len(ncol(Data)), function(x,Data)min(Data[,x]), Data=Data)
  
  #Do Max-Min Scaling
  
  ScaledData <- (t(Data) - MinVec)/(MaxVec - MinVec)
  
  return(t(ScaledData))
}