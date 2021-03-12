diferencias_semanales_temp <- function(lista_mapas){
  
  n <- length(lista_mapas) - 1
  diferencias <- vector(mode='list', length = n)
  if ( n>1){
  for (ii in 1:n){
    diferencias[[ii]] <- data.frame(lon = lista_mapas[[ii]]$lon, lat = lista_mapas[[ii]]$lat, z = 0*lista_mapas[[ii]]$z)
    diferencias[[ii]]$z <- lista_mapas[[ii+1]]$z - lista_mapas[[ii]]$z 
  }
  }else{
    diferencias[[1]] <- data.frame(lon = lista_mapas[[1]]$lon, lat = lista_mapas[[1]]$lat, z = 0*lista_mapas[[1]]$z)
    }
  
  return( diferencias)
  
}

##################################################

mapa_diferencias <- function( mapa, fecha.inicial, fecha.final, limites.lon, limites.lat ){
  
  niveles <- seq(-5,5,by=0.5)
  subtitulo.graf <- 'Diferencias semanales de la Temperatura Superficial del Mar'
  titulo.barra <- 'Temp.\n(°C)'
  subtitulo.grafico <- paste0(subtitulo.graf,':\ndel ',
                              format(fecha.inicial,format='%d-%B'),' al ',
                              format(fecha.final,format='%d-%B-%Y'))
  paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
  
  load('costa_Peru_202.RDat')
  indc_shore <- which(shore$group==3.1)
  indc <- which(point.in.polygon( mapa$lon,mapa$lat,
                                  shore$lon[indc_shore],shore$lat[indc_shore])==1)
  if (length(indc)>0){
    mapa <- mapa[-indc,]
  }
  Dt <- 0.5
  npts <- 10
  tamanio <- 12
  
  
  
  if ( exists('pp')){
    rm(pp)  
  }
  
  
  marcas_x <- seq(limites.lon[1],limites.lon[2],by=5)
  
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
  
  marcas_y <- seq(from=limites.lat[1],to=limites.lat[2],by=5)
  etiquetas_y <- unlist(lapply( as.vector(marcas_y),
                                function(x) if( x<0 ) {paste0(-x,'S')}else if(x>0){paste0(x,'N')}else{x} )   )
  pp <- ggplot( data=mapa,aes(x=lon,y=lat,fill=z) )
  pp <- pp + geom_raster(aes(fill = z),interpolate=TRUE,show.legend = TRUE  )
  pp <- pp + scale_fill_gradientn( colours = paleta_color,
                                   limits = range(niveles))
  pp <- pp + geom_contour(data=mapa, aes(x=lon,y=lat,z=z),
                          linetype = 1 ,
                          col = 'black' ,
                          inherit.aes = FALSE,
                          breaks = niveles)
  pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),color = 'black', fill = 'grey80',inherit.aes=FALSE  )

  pp <- pp + geom_text_contour(data=mapa, aes(x=lon,y=lat,z=z),
                               stroke = 0.15,skip=0,min.size =npts,size=tamanio,rotate = FALSE,
                               check_overlap=TRUE,breaks = niveles)
  
  for (kk in 1:length(fronteras)){
    f <- as.data.frame(fronteras[[kk]])
    pp <- pp + geom_point(data=f,aes( x=X1,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
  }
  
    # pp <- pp + geom_point(data=boyas,aes(x=x,y=y),color='black',size=5,inherit.aes=FALSE)  
    # pp <- pp + geom_text(data=boyas,aes(x=x,y=y-0.4,label=c('B1','B2')),
    #                      color='black',size=12,inherit.aes=FALSE)
    
  pp <- pp + scale_x_continuous(limits = range(marcas_x),
                                expand = c(0.01,0),
                                breaks = marcas_x,
                                labels = etiquetas_x)
  pp <- pp + scale_y_continuous(limits = limites.lat,
                                expand = c(0.01,0),
                                breaks = marcas_y,
                                labels = etiquetas_y)
  pp <- pp + labs(x='Longitud',y='Latitud',
                  title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                  subtitle = subtitulo.grafico,
                  caption = 'Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 1981-2009')
  #pp <- pp + coord_cartesian(xlim=c(282,283),ylim=c(-13,-10))
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
                                             title = titulo.barra,
                                             title.theme = element_text(size=26)))
  
  return(pp)
  
  
  
}