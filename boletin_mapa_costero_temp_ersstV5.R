mapa_costero_temp_ersstV5 <- function(lista,variable,
                              limites.lon,limites.lat,
                              fecha.anterior,fecha.actual, poner_boyas ){
  
  require('RNetCDF')
  require('tidyverse')
  require('metR')
  
  boyas <- data.frame(x=c(-85,-85)+360,y=c(-5,-8))
  nino_12 <- data.frame(lon=c(270,280), lat = c(-10,0))
  nino_3 <- data.frame(lon=c(210,270), lat = c(-5,5))
  nino_4 <- data.frame(lon=c(160,210), lat = c(-5,5))
  nino_3.4 <- data.frame(lon=c(190,240), lat = c(-4.6,4.6))
  
  lwd <- 2
  
  narchs <- length(lista)
  #nodoSpline <- c(17,17)
  nodoSpline <- c(15,15)
  # nc = ncdf4::nc_open(arch)
  # variables = names(nc[['var']])
  arch <- lista[1]
  ncin <- open.nc(arch)
  # print.nc(ncin)
  lon.p <- var.get.nc(ncin,'lon')
  lat.p <- var.get.nc(ncin,'lat')
  
  indc.lon <- lon.p >= limites.lon[1] & lon.p <= limites.lon[2]
  indc.lat <- lat.p >= limites.lat[1] & lat.p <= limites.lat[2]
  
  lon <- lon.p[indc.lon]
  lat <- lat.p[indc.lat]
  
  rm('lon.p')
  rm('lat.p')
  
  A <- var.get.nc(ncin,variable)
  ############### PROMEDIO DE LOS ARCHIVOS #######################  
  close.nc(ncin)

      arch <- lista
      ncin <- open.nc(arch)
      A <- var.get.nc(ncin,variable, unpack = TRUE)
      close.nc(ncin)

  temp <- A[indc.lon,indc.lat]
  
  rm('A')
  
  malla <- expand.grid(lon=lon,lat=lat)
  
  Z <- stack( as.data.frame(temp) )
  
  mapa <- data.frame( lon = as.matrix(malla$lon),
                      lat = as.matrix(malla$lat),
                      z = as.matrix(Z$values)  )
  
  ###### SUAVIZADO #######
  rm(list=c('Z','lon','lat'))
  rango.lon <- diff(limites.lon)
  
    load('E:/programasR/boletin/costa_ecuatorial.RDat')
    Dt <- 1
    npts <- 30
    tamanio <- 10

  
  saveRDS(object = mapa,file = 'mapa.RDS')
  
  nfronteras <- length(fronteras)
  
  

    niveles <- seq(-6,6,by=0.5)

    subtitulo.graf <- 'Anomalía de la Temperatura Superficial del Mar\n'
    titulo.barra <- 'ATSM\n(°C)'

    cadena <- paste0(substring(lista, 40, 45),'01')
    cadena <- as.Date(cadena, format='%Y%m%d')

    subtitulo.grafico <- paste0(subtitulo.graf,
                                lubridate::month(cadena,
                                                 label=TRUE,
                                                 abbr=FALSE),' ',
                                lubridate::year(cadena))

  
  
  paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
  
  ##################################
  # if ( exists('pp') ){
  #   rm(list=c('pp'))  
  # }
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
  # pp <- pp + geom_dl(data=mapa,aes(x=lon,y=lat,z=z,label=..level..),col='black',
  #                    method = list('bottom.pieces', cex=2.0), stat="contour",
  #                    breaks = niveles)
  # 
  
  
  pp <- pp + geom_text_contour(data=mapa, aes(x=lon,y=lat,z=z),
                               stroke = 0.15,
                               skip=0,
                               min.size =npts,
                               size=tamanio,
                               rotate = FALSE,
                               check_overlap=TRUE,
                               breaks = niveles)
  
  pp <- pp + geom_rect(data = nino_12, aes(xmin = lon[1], xmax = lon[2],
                                           ymin = lat[1], ymax = lat[2]), inherit.aes = FALSE,
                       fill=NA, colour='red', lwd=lwd)
  
  # pp <- pp + geom_rect(data = nino_3, aes(xmin = lon[1], xmax = lon[2],
  #                                          ymin = lat[1], ymax = lat[2]), inherit.aes = FALSE,
  #                      fill=NA, colour='black', lwd=lwd)
  # 
  
  pp <- pp + geom_rect(data = nino_3.4, aes(xmin = lon[1], xmax = lon[2],
                                           ymin = lat[1], ymax = lat[2]), inherit.aes = FALSE,
                       fill=NA, colour='black', lwd=lwd)
  
  # pp <- pp + geom_rect(data = nino_4, aes(xmin = lon[1], xmax = lon[2],
  #                                          ymin = lat[1], ymax = lat[2]), inherit.aes = FALSE,
  #                      fill=NA, colour='black', lwd=lwd)
  # 
  # pp <- pp + geom_text( data = data.frame(lon=165, lat=6.3, lab='NIÑO 4'),
  #                       aes(x=lon,y=lat, label=lab), inherit.aes = FALSE, size=10 )
  # 
  pp <- pp + geom_text( data = data.frame(lon=275, lat=1.3, lab='NIÑO 1+2'),
                        aes(x=lon,y=lat, label=lab), inherit.aes = FALSE, size=8 )
  
  # pp <- pp + geom_text( data = data.frame(lon=265, lat=6.3, lab='NIÑO 3'),
  #                       aes(x=lon,y=lat, label=lab), inherit.aes = FALSE, size=8 )
  # 
  pp <- pp + geom_text( data = data.frame(lon=215, lat=6.3, lab='NIÑO 3.4'),
                        aes(x=lon,y=lat, label=lab), inherit.aes = FALSE, size=8 )
  
  for (kk in 1:length(fronteras)){
    f <- as.data.frame(fronteras[[kk]])
    pp <- pp + geom_point(data=f,aes( x=X1,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
  }
  
  
  
  if (rango.lon<=20 & poner_boyas){
    pp <- pp + geom_point(data=boyas,aes(x=x,y=y),color='black',size=5,inherit.aes=FALSE)  
    pp <- pp + geom_text(data=boyas,aes(x=x,y=y-0.4,label=c('B1','B2')),
                         color='black',size=12,inherit.aes=FALSE)
    
  }
  pp <- pp + scale_x_continuous(limits = range(marcas_x),
                                expand = c(0.01,0),
                                breaks = marcas_x,
                                labels = etiquetas_x)
  pp <- pp + scale_y_continuous(limits = limites.lat,
                                expand = c(0.01,0),
                                breaks = marcas_y,
                                labels = etiquetas_y)
  pp <- pp + labs(x='Longitud',y='Latitud',
                  title = subtitulo.grafico,
                  caption = 'Fuente: NOAA National Centers for Environmental Information
https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/')
  #pp <- pp + coord_cartesian(xlim=c(282,283),ylim=c(-13,-10))
  pp <- pp + theme_bw(   )
    pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                      axis.title.y = element_text( size=28,hjust=0.5  ),
                      axis.text.x = element_text(size=32,colour = 'black', angle=90),
                      axis.text.y = element_text(size=32,colour = 'black'),
                      plot.title = element_text(size=48, hjust = 0.5),
                      plot.subtitle = element_text(size=38, hjust=0.5),
                      plot.caption = element_text(size = 22,hjust = 0))
  
  pp <- pp + guides( fill = guide_colorbar(  barheight = unit(22, "cm"),
                                             barwidth = unit(1.5,'cm'),
                                             label.theme = element_text(size=26),
                                             title = titulo.barra,
                                             title.theme = element_text(size=26)))
  
  return(pp)
  
}