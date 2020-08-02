edgeIsExposed <- function(alpha,edge, Bins,Edges){
  x1 <- Bins[edge[1],1]
  x2 <- Bins[edge[2],1]
  y1 <- Bins[edge[1],2]
  y2 <- Bins[edge[2],2]
  
  Weight <- sqrt((x1-x2)^2 + (y1-y2)^2)
  
  xe <- (x1+x2)/2
  ye <- (y1+y2)/2
  d <- sqrt(alpha^2 - (Weight/2)^2)
  xt <- d*(y2-y1)/Weight
  yt <- d*(x2-x1)/Weight
  xc1 <- xe+xt
  yc1 <- ye-yt
  xc2 <- xe-xt
  yc2 <- ye+yt
  
  PIC1 <- (pointsInCircle(edge[1],xc1,yc1,alpha,Edges, Bins) || pointsInCircle(edge[2],xc1,yc1,alpha,Edges,Bins)) #are there any points in the first circle?
  PIC2 <- (pointsInCircle(edge[1],xc2,yc2,alpha,Edges, Bins) || pointsInCircle(edge[2],xc2,yc2,alpha,Edges,Bins)) #are there any points in the second circle?
  return(!(PIC1 && PIC2)) #If there are points in both circles, return that the edge is not exposed
                           #If there are no points in at least one circle, return that the edge is exposed
  
  #CriticalR <- alpha*.999 #The distance all points must be from the center of the circumcircle
                          #Since the distance to the circumcenter from the two edge points is equal
                          #to alpha, we scale the critical radius down slightly to avoid flagging these two nodes
                         
  #InCirc1 <- FALSE
  #InCirc2 <- FALSE
  
  #for(i in 1:nrow(Bins)){
    
    #Compute the distance from Bin i to the circumcenter of each of the circles
    
  #  x <- Bins[i,1]
  #  y <- Bins[i,2]
    
  #  D1 <- sqrt((x - xc1)^2 + (y - yc1)^2)
  #  D2 <- sqrt((x - xc2)^2 + (y - yc2)^2)
    
  #  if(D1 < CriticalR){
  #    InCirc1 <- TRUE
  #  }
    
  #  if(D2 < CriticalR){
  #    InCirc2 <- TRUE
  #  }
    
  #  if(InCirc1 & InCirc2){ #Once we have found points in both circles, return that the edge is not exposed
  #    return(FALSE)
  #  }
    
  #}
  
  #return(TRUE) #If at least one of the circles does not have points, then return that the edge is exposed
  
}