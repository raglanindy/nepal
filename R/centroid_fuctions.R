############cent

require(rgeos)
require(sp)

centroid <- function(pol,ultimate=TRUE,iterations=5,initial_width_step=10){
  if (ultimate){
    new_pol <- pol
    # For every polygon do this:
    for (i in 1:length(pol)){
      width <- -initial_width_step
      area <- gArea(pol[i,])
      centr <- pol[i,]
      wasNull <- FALSE
      for (j in 1:iterations){
        if (!wasNull){ # stop when buffer polygon was alread too small
          centr_new <- gBuffer(centr,width=width)
          # if the buffer has a negative size:
          substract_width <- width/20
          while (is.null(centr_new)){ #gradually decrease the buffer size until it has positive area
            width <- width-substract_width
            centr_new <- gBuffer(centr,width=width)
            wasNull <- TRUE
          }
          # if (!(is.null(centr_new))){
          #   plot(centr_new,add=T)
          # }
          new_area <- gArea(centr_new)
          #linear regression:
          slope <- (new_area-area)/width
          #aiming at quarter of the area for the new polygon
          width <- (area/4-area)/slope
          #preparing for next step:
          area <- new_area
          centr<- centr_new
        }
      }
      #take the biggest polygon in case of multiple polygons:
      d <- disaggregate(centr)
      if (length(d)>1){
        biggest_area <- gArea(d[1,])
        which_pol <- 1                             
        for (k in 2:length(d)){
          if (gArea(d[k,]) > biggest_area){
            biggest_area <- gArea(d[k,])
            which_pol <- k
          }
        }
        centr <- d[which_pol,]
      }
      #add to class polygons:
      new_pol@polygons[[i]] <- remove.holes(new_pol@polygons[[i]])
      new_pol@polygons[[i]]@Polygons[[1]]@coords <- centr@polygons[[1]]@Polygons[[1]]@coords
    }
    centroids <- gCentroid(new_pol,byid=TRUE)
  }else{
    centroids <- gCentroid(pol,byid=TRUE)  
  }  
  return(centroids)
}

#Given an object of class Polygons, returns
#a similar object with no holes


remove.holes <- function(Poly){
  # remove holes
  is.hole <- lapply(Poly@Polygons,function(P)P@hole)
  is.hole <- unlist(is.hole)
  polys <- Poly@Polygons[!is.hole]
  Poly <- Polygons(polys,ID=Poly@ID)
  # remove 'islands'
  max_area <- largest_area(Poly)
  is.sub <- lapply(Poly@Polygons,function(P)P@area<max_area)  
  is.sub <- unlist(is.sub)
  polys <- Poly@Polygons[!is.sub]
  Poly <- Polygons(polys,ID=Poly@ID)
  Poly
}
largest_area <- function(Poly){
  total_polygons <- length(Poly@Polygons)
  max_area <- 0
  for (i in 1:total_polygons){
    max_area <- max(max_area,Poly@Polygons[[i]]@area)
  }
  max_area
}

gCentroidWithin <- function(pol) {
  require(rgeos)
  
  pol$.tmpID <- 1:length(pol)
  # initially create centroid points with gCentroid
  initialCents <- gCentroid(pol, byid = T)
  
  # add data of the polygons to the centroids
  centsDF <- SpatialPointsDataFrame(initialCents, pol@data)
  centsDF$isCentroid <- TRUE
  
  # check whether the centroids are actually INSIDE their polygon
  centsInOwnPoly <- sapply(1:length(pol), function(x) {
    gIntersects(pol[x,], centsDF[x, ])
  })
  
  if(all(centsInOwnPoly) == TRUE){
    return(centsDF)
  }
  
  else {
    # substitue outside centroids with points INSIDE the polygon
    newPoints <- SpatialPointsDataFrame(gPointOnSurface(pol[!centsInOwnPoly, ], 
                                                        byid = T), 
                                        pol@data[!centsInOwnPoly,])
    newPoints$isCentroid <- FALSE
    centsDF <- rbind(centsDF[centsInOwnPoly,], newPoints)
    
    # order the points like their polygon counterpart based on `.tmpID`
    centsDF <- centsDF[order(centsDF$.tmpID),]
    
    # remove `.tmpID` column
    centsDF@data <- centsDF@data[, - which(names(centsDF@data) == ".tmpID")]
    
    cat(paste(length(pol), "polygons;", sum(centsInOwnPoly), "actual centroids;", 
              sum(!centsInOwnPoly), "Points corrected \n"))
    
    return(centsDF)
  }
}