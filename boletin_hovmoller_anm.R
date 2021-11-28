
boletin_hovmoller_anm <- function( lista, poligono,limite.lon,
                                   limite.lat,fecha.anterior,fecha.actual,franja ){

########
# cat("\014") 
# graphics.off()
# rm(list=ls())
require('RNetCDF')
require('stringr')
require('tidyverse')
require('scales')
require('directlabels')
require('maptools')
require('reshape2')
require('metR')

  poligono <- as.data.frame(poligono)
  
  names(poligono) <- c('lon','lat')
  
  # poligono$lon <- 360 + poligono$lon 
  
  dias.atras <- as.numeric(fecha.actual-fecha.anterior)
  
  ####### LECTURA DE ARCHIVOS ###########
  
  arch <- lista[1]
  ncin <- open.nc(arch)
  Sla <- var.get.nc(ncin,'sla')
  lon <- var.get.nc(ncin,'longitude')
  lat <- var.get.nc(ncin,'latitude')
  
  
  indc.lon <- lon<0
  
  lon[indc.lon] <- lon[indc.lon] + 360 
  
  malla <- expand.grid(lon=lon,lat=lat)
  
  Z <- stack( as.data.frame(Sla) )
  
  mapa <- data.frame( lon = as.matrix(malla$lon),
                      lat = as.matrix(malla$lat),
                      z = as.matrix(Z$values)  )
  
  ####### ARMADO DE LA MATRIZ DE DATOS QUE EST?N DENTRO DEL POL?GONO ########
  
  close.nc(ncin)
  
  if (length(lista)>dias.atras){
    cortar.lista <- length(lista) - dias.atras
    
    lista <- lista[-seq(1:cortar.lista)]}
  
    indc <- which(point.in.polygon( malla$lon,malla$lat,
                                  poligono$lon,poligono$lat )==1)
  
    X <- malla$lon[indc]
    Y <- malla$lat[indc]
  
    latitudes <- unique(malla$lat[indc])
  
    datos <- matrix( nrow =  length(latitudes),
                   ncol = (length(lista)-4) ) 
    tiempo <- array(  dim = (length(lista)-4) )
  
  for ( ii in 3:(length( lista)-2) ) {
    Sla <- 0*Sla
    for ( kk in (ii-2):(ii+2)){
      arch <- lista[kk]
      tryCatch({
      ncin <- open.nc(arch)
      if (kk==ii){
        tiempo[kk-2] <- var.get.nc(ncin,'time') 
      }
      Sla <-  0.01*var.get.nc(ncin,'sla') + Sla
      close.nc(ncin)},
      error=function(e)e,finaly=e)
    }
    
    Sla <- Sla/5
    Z <- Sla[indc]
    
    for ( jj in 1:length(latitudes)){
      indc2 <- Y==latitudes[jj]
      datos[jj,ii-2] = mean( Z[indc2],na.rm=TRUE  )
    }
  }
  
  ######### MALLA #########
  
  malla <- expand.grid(lat=latitudes,tiempo=tiempo)
  
  Z <- stack( as.data.frame(datos) )
  
  
  hovmoller <- data.frame( tiempo = malla$tiempo,
                           lat = as.matrix(malla$lat),
                           anm = as.matrix(Z$values)  )
  
  # suave <- gam(anm ~ te(lat,tiempo,k=c(20,20)),data = hovmoller)
  #  pred <- predict(suave,newdata = as.data.frame(malla))
  #     Z <- as.matrix( pred )
  # 
  # hovmoller <- data.frame(  tiempo = malla$tiempo,
  #                              lat = as.matrix(malla$lat),
  #                              anm = as.matrix(Z))

  
  indcT <- grep( '[0-9]+-[0-9]+-01',as.Date(tiempo,origin='1950-01-01')  )
  
  marcas.fecha<- c(tiempo[1],tiempo[indcT])
  
  etqt.fecha <- as.Date( marcas.fecha, origin='1950-01-01') %>% months(abbreviate =TRUE)
  
  #########################
  paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
  niveles <- seq(-30,30,by=4)
  niveles_contornos <- seq(-30,30,by=2)
  marcas_y <- seq(from=limite.lat[1],to=limite.lat[2],by=2)
  etiquetas_y <- unlist(lapply( as.vector(marcas_y),
                                function(x) if( x<0 ) {paste0(-x,'S')}else if(x>0){paste0(x,'N')}else{x} )   )
  pp <- ggplot( data=hovmoller,aes(x=tiempo,y=lat,fill=anm) )
  pp <- pp + geom_raster(aes(fill = anm),interpolate=TRUE,show.legend = TRUE  )
  pp <- pp + scale_fill_gradientn(colours = paleta_color,
                                  limits =range(niveles),
                                  breaks = niveles_contornos)
  pp <- pp + stat_contour(data=hovmoller, aes(z=anm),
                          breaks = niveles_contornos,col='black' )
  
  pp <- pp + geom_text_contour(data=hovmoller,aes(x=tiempo,y=lat,z=anm,label=..level..),
                               rotate=FALSE,size=18,stroke=0.15,
                               check_overlap = TRUE)
  
  pp <- pp + scale_x_continuous( breaks = marcas.fecha,
                                 labels = etqt.fecha,
                                 expand = c(0,0),
                                 limits = range(tiempo) )
  pp <- pp + scale_y_continuous( limits = range(marcas_y),
                                 breaks = marcas_y  ,
                                 expand = c(0,0),
                                 labels = etiquetas_y)
  
  pp <- pp + labs(x='Fecha',y='Latitud',
                  title=paste0('DIRECCI?N DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                  subtitle=paste0('Anomalía del nivel del Mar: del ',
                                  format(fecha.anterior,'%B-%d-%Y'),' a ',
                                  format(fecha.actual,'%B-%d-%Y'),'\nFranja de ',franja,' millas'),
                  caption=paste0('Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0)','\nClimatolog?a: 1993-2012'))
  
  
  pp <- pp +  guides( fill = guide_colorbar(barheight = unit(25, "cm"),
                                            barwidth = unit(3,'cm'),
                                            label.theme = element_text(size=34),
                                            title='ANM\ncm',
                                            title.theme = element_text(size=30)) )
  pp <- pp + theme_bw()
  
  pp <- pp + theme( axis.text.x = element_text( size = 38,color='black',angle=90 ),
                    axis.text.y = element_text( size = 38,color='black' ),
                    axis.title.x = element_text( size = 40 ),
                    axis.title.y = element_text( size = 40 ),
                    title = element_text( size = 40 ),
                    plot.caption = element_text( size = 32,hjust = 0))
  return(pp)
}