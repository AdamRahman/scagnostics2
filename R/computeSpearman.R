#Compute Spearman correlation of two random variables

computeSpearman <- function(Points, Weight, isOutlier){
  x <- rank(Points[,1]) 
  y <- rank(Points[,2])
  
  xmean <- 0
  ymean <- 0
  xx <- 0
  yy <- 0
  xy <- 0
  sumwt <- 0
  
  for(i in 1:length(Weight)){
    wt <- Weight[i]
    if(wt > 0 & !isOutlier[i]){
      sumwt <- sumwt + wt
      xx <- xx + (x[i]-xmean)*wt*(x[i]-xmean)
      yy <- yy + (y[i]-ymean)*wt*(y[i]-ymean)
      xy <- xy + (x[i]-xmean)*wt*(y[i]-ymean)
      xmean <- xmean + (x[i]-xmean)*wt/sumwt
      ymean <- ymean + (y[i]-ymean)*wt/sumwt
    }
  }
  
  xy <- xy/sqrt(xx*yy)
  
  return(xy)
}