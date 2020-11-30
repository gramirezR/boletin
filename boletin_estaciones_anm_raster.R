boletin_estaciones_anm_raster <- function(lista,puntos,fecha.anterior,fecha.actual){

  require('raster')
  
  narchs <- length(lista)
  
  
  region.lat <- c(-10,5)
  region.lon <- c(-100,-75) + 360
  
  #### DDELIMITAR LA ZONA
  
  ncin <- open.nc(lista[1])
  
  lat <- var.get.nc(ncin,'latitude')
  lon <- var.get.nc(ncin,'longitude')+360
  
  close.nc(ncin)
  
  # indc.lat <- which((lat >= region.lat[1]) & (lat <= region.lat[2]) )
  # indc.lon <- which((lon >= region.lon[1]) & (lon <= region.lon[2]) )
  # 
  #######
  
  anm <- vector(mode='list',length=narchs)
  tie <- array(dim=narchs)
  for (ii in 1:narchs){
    tryCatch({ncConn <- open.nc(lista[ii])
    longitud <- var.get.nc(ncConn,variable='longitude')
    latitud <- var.get.nc(ncConn,variable='latitude')
    indc.lat <- which(  latitud>=region.lat[1] &  latitud<=region.lat[2])
    datos <- var.get.nc(ncConn,'sla')*1.0e-2
    tie[ii] <- 24*3600*var.get.nc(ncin,'time')
    if (!any(longitud<0)) {
      cuales.puntos <- longitud>=region.lon[1] & longitud<=region.lon[2]
      indc.lon <- which( cuales.puntos)
    }else{
      indc.180 <- which(longitud<0)
      longitud[indc.180] <- longitud[indc.180] +360
      cuales.puntos <- longitud>=(region.lon[1]) & longitud<=(region.lon[2])
      indc.lon <- which( cuales.puntos)
      indc.lon <- indc.lon[order(longitud[indc.lon])]
    }
    mapa <- pasar.aRaster(datos[indc.lon,indc.lat],longitud[indc.lon],latitud[indc.lat])
    close.nc(ncConn)
    anm[[ii]] <- mapa
    rm(list=c('mapa','datos'))},
    error=function(e)e,finally = function(ii) 1)
  }
  
  
  for (ii in 1:length(anm)){
    if (is.null(anm[[ii]])){
      anm[[ii]] <- 0.5*( anm[[ii-1]] + anm[[ii+1]] )
      tie[ii] <- 0.5*(tie[ii-1]+tie[ii+1])
      print(ii)
    }
  }
  
  x.raster <- do.call(what=stack,args=anm)
  crs(x.raster) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  
  # windows()
  # plot(x.raster)
  # 
  
balta <-  raster::extract(x.raster,t(as.matrix(puntos$balta)),layer=1,nl=nlayers(x.raster))
chimbote <-  raster::extract(x.raster,t(as.matrix(puntos$chimbote)),layer=1,nl=nlayers(x.raster),method='bilinear')
talara <-  raster::extract(x.raster,t(as.matrix(puntos$talara)),layer=1,nl=nlayers(x.raster))
###########################################################
  
anm <- rbind( t(balta) , t(chimbote) , t(talara)  )
est <- t(cbind( t(rep('Galapagos',nlayers(x.raster))),
              t(rep('Chimbote',nlayers(x.raster))),
              t(rep('Talara',nlayers(x.raster)))))

#################################


grafica <- data.frame(    T = as.POSIXct(tie,origin='1950-01-01'), 
                        anm = anm,
                        EST = as.factor(est))   

 pp <- ggplot(data=grafica,aes(x=T,y=anm,colour=EST))
 pp <- pp + geom_path(size=1.5)
 pp <- pp + scale_color_manual(name='ESTACIÓN',values=c('Galapagos'='red','Talara'='blue','Chimbote'='green'))
 pp <- pp + theme_bw() + scale_x_datetime(date_breaks = '1 week',labels = waiver())
 pp <- pp + labs(x= 'Fecha',y='Anomalía nivel del mar (cm)',
                 title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                              'Dpto. de Oceanografía - Div. Oceanografía'),
                 subtitle=paste0('Nivel del mar'),
                 caption=paste0('Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0)',
                                ' Climatología: 1993-2012'))
 pp <- pp + theme( axis.title.x = element_text( size=24,hjust=0.5  ),
                   axis.title.y = element_text( size=24,hjust=0.5  ),
                   axis.text = element_text(size=20),
                   axis.text.x = element_text(angle=90),
                   title=element_text(size=26),
                   plot.subtitle=element_text(size=24),
                   plot.caption = element_text(size = 20,hjust = 0),
                   legend.text = element_text(size = 20,hjust = 0),
                   legend.title = element_text(size = 21,hjust = 0)
                   )
 
 return(pp)
 
 }