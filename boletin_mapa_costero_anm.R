boletin_mapa_costero_anm <- function(lista = archivos,
                                     limites.lon ,
                                     limites.lat ,
                                     fecha.anterior,
                                     fecha.actual){
  
  narchs <- length(lista)
   
    ncin <- open.nc(lista[narchs])
   lon.p <- var.get.nc(ncin,'longitude')
   lat.p <- var.get.nc(ncin,'latitude')
   
   anmPositivo <- 0.0*var.get.nc(ncin,'sla')
   anmNegativo <- anmPositivo
   close.nc(ncin)
  
  if (narchs>1){
    for (ii in 1:narchs){
      arch <- lista[ii]
      ncin <- open.nc(arch)
      lon.p <- var.get.nc(ncin,'longitude')
      if(any(range(lon.p)<0)){
        lonNegativo <- var.get.nc(ncin,'longitude') 
        if (exists('anmNegativo')){
          anmNegativo <- 0.01*var.get.nc(ncin,'sla') + anmNegativo  
        }else{
          anmNegativo <- 0.01*var.get.nc(ncin,'sla') 
        }
        
      }else{
        lonPositivo <- var.get.nc(ncin,'longitude')
        if (exists('anmPositivo')){
          anmPositivo <- 0.01*var.get.nc(ncin,'sla') + anmPositivo
        }else{
          anmPositivo <- 0.01*var.get.nc(ncin,'sla')
        }
      }
      close.nc(ncin)
    }
    
    
    indcLonNegativo <- which(lon.p<0)
    
    lonNegativo[indcLonNegativo] <-  lonNegativo[indcLonNegativo] + 360
    
    lonPositivo <- lonNegativo
    
    anmNegativo <- anmNegativo[order(lonNegativo),]
    
    anm <- ( anmPositivo + anmNegativo) / narchs
  }else{
    arch <- lista
    ncin <- open.nc(arch)
    lon.p <- var.get.nc(ncin,'longitude')
    if(any(range(lon.p)<0)){
      anmNegativo <- 0.01*var.get.nc(ncin,'sla') 
      anm <- anmNegativo
      lonNegativo <- lon.p
      indcLonNegativo <- which(lon.p<0)
      lonNegativo[indcLonNegativo] <-  lonNegativo[indcLonNegativo] + 360
      lonPositivo <- lonNegativo[order(lonNegativo)]
    }else{
      lonPositivo <- lon.p
      anmPositivo <- 0.01*var.get.nc(ncin,'sla')
      anm <- anmPositivo
    }
    close.nc(ncin)
    etqt.fecha <- format(fecha.actual,'%d-%B-%Y')
  }
  
 
   
   indc.lon <- which( lonPositivo>=limites.lon[1] & lonPositivo<=limites.lon[2] )
   indc.lat <- which( lat.p>=limites.lat[1] & lat.p<=limites.lat[2] )
   
   lon <- lonPositivo[indc.lon]
   lat <- lat.p[indc.lat]
   
  malla <- expand.grid(lon=lon,lat=lat)
  p <- anm[indc.lon,indc.lat]
  Z <- stack( as.data.frame(p) )
  
  mapa <- data.frame( lon = as.matrix(malla$lon),
                      lat = as.matrix(malla$lat),
                        z = as.matrix(Z$values)  )
  
  ###### LINEA DE COSTA-----------------
  
  rango.lon <- abs(diff(limites.lon))
  if (rango.lon>20){
    load('D:/programasR/boletin/costa_ecuatorial.RDat')
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