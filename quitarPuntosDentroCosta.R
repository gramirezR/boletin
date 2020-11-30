quitarPuntosDentroCosta <- function(malla){
  load('costa_05-20.RData')
  for (ii in 1:length(shore)){
    shore@polygons[[ii]]@Polygons[[1]]@coords <- rbind(shore@polygons[[ii]]@Polygons[[1]]@coords,
                                                       shore@polygons[[ii]]@Polygons[[1]]@coords[1,])
    
  }
  
  indcCosta <- vector(mode='list',length=length(shore))
   longitud <- array(dim = c(length(shore),1))
  for (ii in 1:length(shore)){
    indcCosta[[ii]] <- which(point.in.polygon( malla$lon,malla$lat,shore@polygons[[ii]]@Polygons[[1]]@coords[,1]+360,
                                               shore@polygons[[ii]]@Polygons[[1]]@coords[,2] )==1)
    longitud[ii] <- length(indcCosta[[ii]])
  }
  
   indcLon <- which(longitud >0)
   
   return(do.call(what = cbind,args=indcCosta))
   
}