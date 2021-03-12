boletin_hovmoller_ecuatorial_anm_raster <- function(lista ,
                                        limite.lon,
                                        limite.lat,
                                        fecha.anterior,
                                        fecha.actual){
  
  require('RNetCDF')
  require('tidyverse')
  require('maptools')
  require('reshape2')
  require('directlabels')
  require('RColorBrewer')
  require('plotly')
  require('scales')
  require('mgcv')
  require('metR')
  library('raster')
  source('herramientas.R')
  
  
  ###### DEFINICIÓN DE LA REGIÓN
  
  #extract(velocidad,costa)
  
  region.lat <- limite.lat
  region.lon <- limite.lon
  
  
  ##### LISTA DE ARCHIVOS#####
  
  
  narchs <- length(lista)
  
  #### DDELIMITAR LA ZONA
  
  ncin <- open.nc(lista[1])
  
  lat <- var.get.nc(ncin,'latitude')
  lon <- var.get.nc(ncin,'longitude')
  
  close.nc(ncin)
  
  indc.lat <- which((lat >= region.lat[1]) & (lat <= region.lat[2]) )
  indc.lon <- which((lon >= region.lon[1]) & (lon <= region.lon[2]) )
  
  #######
  
  anm <- vector(mode='list',length=narchs)
  tie <- array(dim=narchs)
  for (ii in 1:narchs){
    tryCatch({ncConn <- open.nc(lista[ii])
             longitud <- var.get.nc(ncConn,variable='longitude')
             latitud <- var.get.nc(ncConn,variable='latitude')
             indc.lat <- which(  latitud>=limite.lat[1] &  latitud<=limite.lat[2])
             datos <- var.get.nc(ncConn,'sla')*1.0e-2
             tie[ii] <- 24*3600*var.get.nc(ncin,'time')
             if (!any(longitud<0)) {
               cuales.puntos <- longitud>=limite.lon[1] & longitud<=limite.lon[2]
               indc.lon <- which( cuales.puntos)
             }else{
               indc.180 <- which(longitud<0)
               longitud[indc.180] <- longitud[indc.180] + 360 
               cuales.puntos <- longitud>=(limite.lon[1]) & longitud<=(limite.lon[2])
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
    if (!is.null(anm[[ii]])){
    anm[[ii]] <- raster::t(anm[[ii]])
    }else{
      anm[[ii]] <- 0.5*( anm[[ii-1]] + raster::t(anm[[ii+1]]) )
      tie[ii] <- 0.5*(tie[ii-1]+tie[ii+1])
      print(ii)
    }
  }

  x.raster <- do.call(what=stack,args=anm)
  crs(x.raster) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
 

    salida <- raster( ncol=narchs,nrow=nrow(x.raster) )
      
    out <- raster(ncol=1,nrow=narchs)
    salida <- writeStart(salida, 'prueba2', overwrite=TRUE)
    
    for (jj in 1:nrow(x.raster)){
      out <- writeStart(out, 'prueba1', overwrite=TRUE)  
    for (ii in 1:narchs) {
        v <- getValues(raster(x.raster,layer=ii), jj)
        v <- mean(v,na.rm=TRUE)
        out <- writeValues(out, v, ii)
    }
      out <- writeStop(out)
      
      salida <- writeValues(salida,getValues(t(out)), jj)
      }
    salida <- writeStop(salida)  

     # windows(width=1200,height=800)
     # plot(salida)
 
###### ARMADO DEL DATA.FRAME PARA GGPLOT#####
  
       malla <- expand.grid(lon=longitud[indc.lon],T=tie)
 malla.inter <- malla
    
    Z <-  getValues(t(salida) )
    
    indcNan <- which( is.na(Z) )
    
    if ( length(indcNan) > 0 ){
      Z <- Z[-indcNan]
      malla1 <- malla[-indcNan,]
    }else{
      malla1 <- malla
    }
    
    hovmoller <- data.frame(    T = malla1$T,
                              lon = malla1$lon,
                              anm = Z)
    suave <- gam(anm ~ te(T,lon,k=c(15,10)),data = hovmoller)
    pred <- predict(suave,newdata = as.data.frame(malla.inter))
     Z <- as.matrix( pred )
     hovmoller <- data.frame(     T = malla.inter$T,
                                  lon = malla.inter$lon,
                                  anm = Z)
  ############################################# 
  
  
  paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
  if (exists('pp')){
    rm(pp)
  }
  
  niveles <- seq( from=-40,to=40,by=5 )

  indcT <- grep( '[0-9]*-[0-9]*-01',as.Date(tie/86400,origin='1950-01-01')  )

  marcas.fecha<- c(tie[1],tie[indcT])

  etqt.fecha <- as.Date(  marcas.fecha/86400,origin='1981-01-01') %>% months(abbreviate =TRUE)

  etiquetas <- seq(from=region.lon[1],to=region.lon[2],by=20)
  etktas <- unlist(lapply( -(etiquetas - 360) ,
                           function(x){ 
                             if (x > 180){
                               paste0(as.vector( as.character(360-x)),'E') 
                             }else if( x< 180) {
                               paste0( x ,'W')
                             }else{
                               as.character(x)
                             }
                           }) )
  ######
  contornos <- seq( from=-30,to=30,by=2 )
  ######
  pp <- ggplot(data=hovmoller,aes(y=lon,x=T,fill=anm)) 
  pp <- pp + geom_raster(aes(fill = anm),interpolate=TRUE,show.legend = TRUE  )
  pp <- pp + scale_fill_gradientn(colours = paleta_color,breaks = niveles,limits =range(niveles))
  
  pp <- pp + stat_contour(  data = hovmoller, aes(y=lon,x=T,z=anm),
                            breaks = contornos,
                            col ='black' ,
                            inherit.aes = FALSE )
  pp <- pp + geom_text_contour(data=hovmoller,aes(y=lon,x=T,z=anm),
                               stroke = 0.15,
                               size=18,rotate = FALSE,check_overlap = TRUE)
  
  pp <- pp + labs(       y = 'Longitud',
                         x = 'Fecha 2020',
                         title = paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                                        'Dpto. de Oceanografía - Div. Oceanografía'),
                         subtitle = expression( Zona~Ecuatorial~2*degree~N~a~2*degree~S),
                         caption = 'Fuente: COPERNICUS MARINE\n             ENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 1981-2009')
  pp <- pp + theme_bw()
  
  # pp <- pp + scale_x_continuous(expand = c(0.02,0.02),
  #                               breaks = etiquetas,
  #                               labels = etktas,
  #                               limits = (range(region.lon)))
  # 
  # pp <- pp + scale_y_reverse(breaks = marcas.fecha,
  #                             label = etqt.fecha,
  #                            expand = c(0.02,0.02))
  
  pp <- pp + scale_x_continuous( breaks = marcas.fecha,
                                 label = etqt.fecha,
                                 expand = c(0.01,0.01) )
  
  pp <- pp + scale_y_reverse( expand = c(0,0),
                              breaks = etiquetas,
                              labels =  etktas)
  
  pp <- pp + theme( axis.text.x = element_text( size = 34,color='black' ),
                    axis.text.y = element_text( size = 34,color='black' ),
                    axis.title.x = element_text( size = 40 ),
                    axis.title.y = element_text( size = 40 ),
                    title = element_text( size = 26 ),
                    plot.caption = element_text( size = 28,hjust = 0))
  
  pp <- pp + guides( fill = guide_colorbar(barheight = unit(25, "cm"),
                                           barwidth = unit(2,'cm'),
                                           label.theme = element_text(size=30),
                                           title='ANM\ncm',
                                           title.theme = element_text(size=30)) )
  
  return(pp)
}