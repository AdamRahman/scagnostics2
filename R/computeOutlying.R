computeOutlying <- function(OriginalWeights,MSTOutliers){
  outlying <- MSTOutliers/sum(OriginalWeights)
  return(min(outlying,1))
}