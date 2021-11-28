
boletin_hovmoller_anm_raster <- function( lista, poligono,limite.lon,
                                          limite.lat,fecha.anterior,fecha.actual,franja, filtro ){
  
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
  require('raster')
  # require('tswge')
  require('WaveletComp')
  
  poligono <- as.data.frame(poligono)
  
  names(poligono) <- c('lon','lat')
  
  indc2 <- which(poligono$lon<0)
  if(length(indc2)>0){
    poligono$lon <- poligono$lon + 360
  }
  
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

  close.nc(ncin)  
    
  ####### ARMADO DE LA MATRIZ DE DATOS QUE ESTÁN DENTRO DEL POLÍGONO ########
  
  if (length(lista)>dias.atras){
    cortar.lista <- length(lista) - dias.atras
    
    lista <- lista[-seq(1:cortar.lista)]}
  
  indc <- which(point.in.polygon( malla$lon,malla$lat,
                                  poligono$lon,poligono$lat )==1)
  
  X <- malla$lon[indc]
  Y <- malla$lat[indc]
  
  latitudes <- unique(malla$lat[indc])
  
  datos <- matrix( nrow =  length(latitudes),
                   ncol = length(lista) ) 
  tiempo <- array(  dim = length(lista) )
  
  for ( ii in 1:length(lista) ) {
    # Sla <- 0*Sla
    arch <- lista[[ii]]
    ncin <- open.nc(arch)
    tiempo[ii] <- as.POSIXct(var.get.nc(ncin,'time')*24*3600, origin='1950-01-01')
    Sla <- var.get.nc(ncin,'sla', unpack = TRUE)
    Z <- stack(as.data.frame(Sla))$values[indc]
    close.nc(ncin)
    
    for ( jj in 1:length(latitudes)){
      indc2 <- Y==latitudes[jj]
      datos[jj,ii] = 100*mean( Z[indc2], na.rm=TRUE  )
    }
    
  }
  
  
  # df <- data.frame(x=malla$lon[indc], y=malla$lat, z= datos[indc,])
  
  
  #saveRDS(object = datos, file = 'matriz_hovmoller_anm.RDS')
  
  #tiempo <- tiempo + 8
  
  # print(as.Date(tiempo, origin='1970-01-01'))
  ######### MALLA #########
  
  indcTiempoNA <- which(is.na(tiempo))
  
  if(length(indcTiempoNA)>0){
    tiempo[indcTiempoNA] <- 0.5*(tiempo[indcTiempoNA-1] + tiempo[indcTiempoNA+1] )
  }
  
  # indc <- which(complete.cases(datos))
  # 
  # 
  # if (length(indc)>0){
  #   
  #   df <- datos[indc,]
  #   
  #   #modelo <- mgcv::gam(data=df, )
  #   
  #   
  # }
  
  
  
   saveRDS(object = list(datos, latitudes, tiempo),
           file = 'E:/programasR/hov_anm.RDS')
  
  ############FILTRO PASA BANDA DE 10 A 120 DIAS##################
   
   filtro_wavelet <- function(x) {
     if (all(is.na(x))){
       return(x)
     }
     N <- length(x)
     
     hay_nan <- any(is.na(x))
     df <- data.frame(x = x)  
     y_wavelet <-   analyze.wavelet(my.data = df,
                                    loess.span = 1,
                                    dt = 1, dj = 1/250,
                                    lowerPeriod = 10,
                                    upperPeriod = 120,
                                    make.pval = TRUE, n.sim=10,
                                    verbose = FALSE)
     
     y_rec <- reconstruct(y_wavelet, plot.waves = FALSE,
                          verbose = FALSE )               
     graphics.off()
     resultado <- y_rec$series$x.r
     # print(y_rec)
     return( resultado )
   }
   
###################################################################   
  if(filtro){
    datos <- apply(datos, MARGIN = 1,
                   FUN=function(x) {filtro_wavelet(x)}
    )
    if(franja=='50') franja <- '60'
    datos <- t(datos)
    niveles <- seq(-12,12,by=1)
    niveles2 <- seq(-12,12,by=2)
    paleta_color <- cptcity::cpt('ncl_BlueWhiteOrangeRed', n=19)
    
    paleta_color <- c(paleta_color[1:9],
                      paleta_color[10],paleta_color[10],
                      paleta_color[10],paleta_color[10],
                      paleta_color[11:19])
    subtitulo <- paste0('Anomalía del nivel del Mar: del ',
                        format(fecha.anterior,'%B-%d-%Y'),' a ',
                        format(fecha.actual,'%B-%d-%Y'),
                        '\nFranja de ',franja,' millas\n',
                        'Filtro pasabanda de 10 a 120 días')
  }else{
    if(franja=='50') franja <- '60'
    paleta_color <- cptcity::cpt('ncl_BlueWhiteOrangeRed', n=19)
    
    # paleta_color <- c(paleta_color[1:9],
    #                   paleta_color[10],paleta_color[10],paleta_color[10],paleta_color[10],
    #                   paleta_color[11:19])
    paleta_color <- c(paleta_color[c(1,3,6,9)],
                      paleta_color[10:19])
    niveles <- c(-10,-5,0,5,10,15,20,25,30)#seq(-6,6,by=2)
    niveles2 <- seq(-10,30,by=5)
    subtitulo <- paste0('Anomalía del nivel del Mar: del ',
                        format(fecha.anterior,'%B-%d-%Y'),' a ',
                        format(fecha.actual,'%B-%d-%Y'),
                        '\nFranja de ',franja,' millas')
  }
  
  
  #####################
  tiempo <- as.POSIXct(tiempo, origin='1970-01-01')
  malla <- expand.grid(lat=latitudes,tiempo=tiempo)
  
  Z <- stack( as.data.frame(datos) )
  
  
  hovmoller <- data.frame( tiempo = malla$tiempo,
                           lat = as.matrix(malla$lat),
                           anm = as.matrix(Z$values)  )
  
  ######################################
  
  indc <- is.na( hovmoller$anm  )
  
  
  df <- hovmoller[which(!indc),]
  
  indc <- which(indc)
  
  df$tiempo <- as.numeric(df$tiempo)
  
  if (length(indc)>0){
    
    modelo <- mgcv::gam( data = df,
                        formula =  anm ~ te(tiempo,lat,k = c(10,10)) )
    df2 <- hovmoller[indc,c('tiempo','lat')]
    df2$tiempo <- as.numeric(df2$tiempo)
    z <- predict(object = modelo, newdata = df2 )
    
    hovmoller$anm[indc] <- z
    
  }
  
  
  ######################################
  nuevoRaster <- hovmoller[,c('tiempo','lat')]
  cualNa <- which(is.na(hovmoller$anm))
  ##

  saveRDS(object = 'promedios', file = 'E:/programasR/promedioANM_2019-2020.RDS' )
  saveRDS(object = 'desv_sd', file = 'E:/programasR/promedioANM_2019-2020.RDS' )
  
  indcT <- grep( '[0-9]+-[0-9]+-01',as.Date(tiempo,origin='1950-01-01')  )
  
  marcas.fecha<- as.POSIXct(c(tiempo[1],tiempo[indcT]), origin='1950-01-01')
  
  etqt.fecha <- as.Date( marcas.fecha, origin='1950-01-01') %>% months(abbreviate =TRUE)
  
  #########################
   
  marcas_y <- seq(from=limite.lat[1],to=limite.lat[2],by=2)
  etiquetas_y <- unlist(lapply( as.vector(marcas_y),
           function(x) if( x<0 ) {paste0(-x,'S')}else if(x>0){paste0(x,'N')}else{x} )   )
  pp <- ggplot( data=hovmoller,aes(x=tiempo,y=lat,fill=anm) )
  
  pp <- pp + geom_raster(aes(fill = anm),
                         interpolate=TRUE,
                         show.legend = TRUE  )
  
  pp <- pp + scale_fill_gradientn(colours = paleta_color,
                                  limits  = range(niveles),
                                  breaks  = niveles, 
  )
  
  pp <- pp + geom_contour(aes(z=anm),
                          breaks = niveles,
                          col='black'  )
#library(directlabels)  
  # pp <- pp + geom_text_contour( aes(label=..levels..) )
#  pp <-  direct.label(pp, method="bottom.pieces", debug=TRUE)
  pp <- pp + scale_x_datetime( breaks = marcas.fecha,
                                 labels = etqt.fecha,
                                 expand = c(0,0),
                                 limits = range(tiempo) )
  pp <- pp + scale_y_continuous( limits = range(marcas_y),
                                 breaks = marcas_y  ,
                                 expand = c(0,0),
                                 labels = etiquetas_y)
  
  pp <- pp + labs(x='Fecha',y='Latitud',
                  title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                  subtitle=subtitulo,
                  caption=paste0('Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0)','\nClimatología: 1993-2012'))
  
  pp <- pp +  guides( fill = guide_colorbar(barheight = unit(25, "cm"),
                                            barwidth = unit(3,'cm'),
                                            label.theme = element_text(size=34),
                                            title='ANM',
                                            title.theme = element_text(size=30)) )
  pp <- pp + theme_bw()
  
  pp <- pp + theme( axis.text.x = element_text( size = 40,color='black',angle=90 ),
                    axis.text.y = element_text( size = 40,color='black' ),
                    axis.title.x = element_text( size = 40 ),
                    axis.title.y = element_text( size = 40 ),
                    plot.title = element_text( size = 38 ),
                    plot.subtitle = element_text( size = 42 ),
                    plot.caption = element_text( size = 32,hjust = 0))
  return(pp)
}