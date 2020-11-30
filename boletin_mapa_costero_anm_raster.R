boletin_mapa_costero_anm_raster <- function(lista = archivos,
                                     limites.lon ,
                                     limites.lat ,
                                     fecha.anterior,
                                     fecha.actual){
  
  require('raster')
  require('RNetCDF')
  source('herramientas.R')
  narchs <- length(lista)
  graphics.off()
  
  ##################################################
  
  anm <- vector(mode='list',length=narchs)
  
  for (ii in 1:narchs){
    
      tryCatch({
      ncConn <- open.nc(lista[ii])
    longitud <- var.get.nc(ncConn,variable='longitude')
     latitud <- var.get.nc(ncConn,variable='latitude')
    indc.lat <- which(  latitud>=limites.lat[1] &  latitud<=limites.lat[2])
    datos <- var.get.nc(ncConn,'sla')*1.0e-2
         if (!any(longitud<0)) {
            cuales.puntos <- longitud>=limites.lon[1] & longitud<=limites.lon[2]
                 indc.lon <- which( cuales.puntos)

     }else{
                 indc.180 <- which(longitud<0)
       longitud[indc.180] <- longitud[indc.180] + 360 
            cuales.puntos <- longitud>=(limites.lon[1]) & longitud<=(limites.lon[2])
                 indc.lon <- which( cuales.puntos)
                 indc.lon <- indc.lon[order(longitud[indc.lon])]
     }
    mapa <- pasar.aRaster(datos[indc.lon,indc.lat],longitud[indc.lon],latitud[indc.lat])
    close.nc(ncConn)
   
    anm[[ii]] <- mapa
    rm(list=c('mapa','datos'))
},error=function(e) e,finally=print(ii))
  }
  
  x.raster <- do.call(what=stack,args=anm)
  crs(x.raster) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  
  promedio <- stackApply(x.raster,indices = rep(1,nlayers(x.raster)),fun='mean',na.rm = TRUE)
  
  # promedio <- calc(x.raster,fun = mean,na.rm=TRUE)
  
  
  # rm('anm')
  # windows()
  # plot(promedio)
  # # 
  ################################################# 
  
  mapa <- raster2ggplot(promedio)
  
  etqt.fecha <- paste0(format(fecha.anterior,'%d-%B-%Y'),' al ',format(fecha.actual,'%d-%B-%Y'))
  ###############################

  ###### LINEA DE COSTA-----------------
  
  rango.lon <- abs(diff(limites.lon))
  if (rango.lon>20){
    load('E:/programasR/boletin/costa_ecuatorial.RDat')
    marcas_y      <- seq(limites.lat[1],limites.lat[2],by=5)
    niveles       <- seq(-40,40,by=5)
    niveles.barra <- seq(-40,40,by=5)
  }else{
    load('costa_Peru_202.RDat') 
    marcas_y      <- seq(-20,5,by=5)
    niveles       <- seq(-30,30,by=1)
    niveles.barra <- seq(-30,30,by=5)
  }
  paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
  
  marcas_x <- seq(limites.lon[1],limites.lon[2],by=10)
  
  etiquetas_x <- unlist(lapply( (marcas_x ) ,
                                function(x){ 
                                  if (x > 180){
                                    paste0(as.vector( as.character(360-x)),'W') 
                                  }else if( x< 180) {
                                    paste0( x ,'E')
                                  }else{
                                    as.character(x)
                                  }
                                }) )
  etiquetas_y <- unlist(lapply( as.vector(marcas_y),
                                function(x) if( x<0 ){
                                  paste0(-x,'S')
                                }else if(x>0){
                                  paste0(x,'N')
                                }else{x} )   )
  
  if (exists('pp')){
    rm(pp)}
  
  pp <- ggplot( data=mapa,aes(x=lon,y=lat,fill=z) )
  pp <- pp + geom_raster(aes(fill = z),interpolate=TRUE,show.legend = TRUE  )
  pp <- pp + scale_fill_gradientn(colours= paleta_color,breaks=niveles.barra,limits =range(niveles))
  pp <- pp + geom_contour(data=mapa, aes(x=lon,y=lat,z=z),breaks=niveles,
                          col='grey45' ,inherit.aes=FALSE )
  
  pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),
                           color = 'black', fill = 'grey80',inherit.aes=FALSE  )
  
  for (kk in 1:length(fronteras)){
    f <- as.data.frame(fronteras[[kk]])
    
    pp <- pp + geom_point(data=f,aes( x=X1,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
  }
  
  pp <- pp + scale_x_continuous(limits = limites.lon,
                                expand = c(0.01,0),
                                breaks = marcas_x,
                                labels = etiquetas_x)
  pp <- pp + scale_y_continuous(limits = limites.lat,
                                expand = c(0.01,0),
                                breaks = marcas_y,
                                labels = etiquetas_y)
  pp <- pp + metR::geom_text_contour(  data = mapa, aes(x=lon,y=lat,z=z),
                                     stroke = 0.15,skip=0,min.size =5,size=12,
                                     breaks = niveles,rotate = FALSE,
                                     check_overlap = TRUE,inherit.aes=FALSE)
  pp <- pp + labs(x='Longitud',y='Latitud',
                  title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                  subtitle=paste0('Anomalia del nivel del Mar: ',etqt.fecha),
                  caption=paste0('Fuente: COPERNICUS MARINE\n             ENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 1981-2009'))
  pp <- pp + theme_bw(   )
  pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                    axis.title.y = element_text( size=28,hjust=0.5  ),
                    axis.text = element_text(size=28,colour = 'black'),
                    title=element_text(size=28),
                    plot.subtitle=element_text(size=24),
                    plot.caption = element_text(size = 22,hjust = 0))
  pp <- pp + guides( fill = guide_colorbar(  barheight = unit(22, "cm"),
                                             barwidth = unit(1.5,'cm'),
                                             label.theme = element_text(size=26),
                                             title = 'ANM\n(cm)',
                                             title.theme = element_text(size=26)))
  return(pp) 
}