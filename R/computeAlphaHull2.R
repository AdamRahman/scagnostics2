#Equivalent to the MATLAB implementation.

#NOTE: Varying the alpha radius can sometimes result in an alpha shape with multiple regions,
#which might or might not contain holes. However, the alphaShape function in MATLAB® 
#always returns regularized alpha shapes, which prevents isolated or dangling points, edges, or faces.

#FROM: http://www.mathworks.com/examples/matlab/mw/matlab-ex67295599-alpha-shapes

computeAlphaHull2 <- function(Bins,Alpha){
  
  #Compute the Delaunay Triangulation
  
  Del <- delaunayn(Bins, full=TRUE)
  
  DelTri <- Del$tri
  NumTri <- nrow(DelTri)
  
  #Include only Triangles with circumradius < alpha
  
  TrianglesInHull <- c()
  
  for(i in 1:nrow(DelTri)){
    
    a <- sqrt((Bins[DelTri[i,1],][1] - Bins[DelTri[i,2],][1])^2 + (Bins[DelTri[i,1],][2] - Bins[DelTri[i,2],][2])^2)
    b <- sqrt((Bins[DelTri[i,1],][1] - Bins[DelTri[i,3],][1])^2 + (Bins[DelTri[i,1],][2] - Bins[DelTri[i,3],][2])^2)
    c <- sqrt((Bins[DelTri[i,2],][1] - Bins[DelTri[i,3],][1])^2 + (Bins[DelTri[i,2],][2] - Bins[DelTri[i,3],][2])^2)
    
    R <- a*b*c/sqrt((a+b+c)*(b+c-a)*(c+a-b)*(a+b-c))
    
    if(R < Alpha){
      TrianglesInHull <- rbind(TrianglesInHull,DelTri[i,])
    }
  }
  
  #Find the Edges Referenced by Only one triangle
  if(length(TrianglesInHull) > 0){
    EdgesInHull <- GetEdges(TrianglesInHull)
    EdgesInHull <- cbind(EdgesInHull,rep(1,nrow(EdgesInHull)))
    
    EdgesInHull <- UniqueEdges2(EdgesInHull)
    
    #Compute the Area of the Alpha Hull
    AlphaArea <- computeAlphaArea2(TrianglesInHull, Bins)
    
    #Compute the Perimeter of the Alpha Hull
    AlphaPerimeter <- computeAlphaPerimeter(EdgesInHull, Bins)
  }else{
    AlphaArea <- 0
    AlphaPerimeter <- 0
    EdgesInHull <- c()
  }
  
  return(list(Area = AlphaArea, Perimeter = AlphaPerimeter, Edges = EdgesInHull))
  
}