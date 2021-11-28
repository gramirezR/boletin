boletin_hovmoller_ecuatorial_anm_raster_discreto <- function(lista ,
                                                    limite.lon,
                                                    limite.lat,
                                                    fecha.anterior,
                                                    fecha.actual,
                                                    filtro){
  
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
  require('WaveletComp')  
  # require('tswge')
  require('raster')
  source('herramientas.R')
  
  
  ###### DEFINICI?N DE LA REGI?N
  
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
    tryCatch({
      #print(lista[ii])
      ncConn <- open.nc(lista[ii])
      longitud <- var.get.nc(ncConn,variable='longitude')
      latitud <- var.get.nc(ncConn,variable='latitude')
      indc.lat <- which(  latitud>=limite.lat[1] &  latitud<=limite.lat[2])
      datos <- var.get.nc(ncConn,'sla', unpack = TRUE)
      tie[ii] <- 24*3600*var.get.nc(ncin,'time')
      #print(as.Date(tie[ii]/86400,origin='1950-01-01'))
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
      # print(ii)
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
  
  
  ############FILTRO PASA BANDA DE 10 A 120 DIAS##################
  
  Z <- matrix(Z, nrow = length(longitud[indc.lon]), byrow = FALSE)
  
  Z <- apply(Z, MARGIN = 1,
             FUN=function(x) {
               # N <- length(x)
               # y <- c(rev(x),x,rev(x))
               # 
               # rr <- butterworth.wge(x = y,
               #                       order = 4,
               #                       type = 'pass',
               #                       cutoff = c(1/120, 1/10),
               #                       plot=FALSE)
               # resultado <- rr$x.filt
               # resultado <- resultado[(N+1):(2*N)]
               y_wavelet <-   analyze.wavelet(my.data = data.frame(x=x),
                                              loess.span = 1,
                                              dt = 1, dj = 1/250,
                                              lowerPeriod = 10,
                                              upperPeriod = 120,
                                              make.pval = TRUE,
                                              n.sim=10,
                                              verbose = FALSE)
               
               y_rec <- reconstruct(y_wavelet, plot.waves = FALSE,
                                    verbose = FALSE )               
               graphics.off()
               resultado <- y_rec$series$x.r                     
               return( resultado )
             }
  )
  
  Z <- 100*stack(as.data.frame(t(Z)))$values
  
  niveles <- seq(-12,12,by=2)
  niveles2 <- seq(-12,12,by=2)
  
  #####################
  
  hovmoller <- data.frame(    T = malla1$T,
                              lon = malla1$lon,
                              anm = Z)
  
  suave <- gam(anm ~ te(T,lon,k=c(30,30)),data = hovmoller)
  pred <- predict(suave,newdata = as.data.frame(malla.inter))

  Z <- as.matrix( pred )
  hovmoller <- data.frame(     T = malla.inter$T,
                               lon = malla.inter$lon,
                               anm = Z)
  ############################################# 
  
  niveles3 <- seq(-12, 10,by = 2)
  temp <- cut( hovmoller$anm,
               breaks = niveles3,
               labels = niveles3[-1])
  
  hovmoller$anm_d <- temp
  
  
  ############################################
  paleta_color <- cptcity::cpt('ncl_BlueWhiteOrangeRed', n=10)
  
  paleta_color <- c(paleta_color[1:5],
                    '#FFFFFF',
                    '#FFFFFF',
                    paleta_color[6:10])
  if (exists('pp')){
    rm(pp)
  }
  
  # if (filtro){
  #   promedios <- mean(hovmoller$anm, na.rm=TRUE)
  #   desv_sd <- sd(hovmoller$anm, na.rm=TRUE)
  #   hovmoller$anm <- (hovmoller$anm - promedios)/ desv_sd
  #   niveles <- seq(-4,4,by=0.25)
  #   niveles2 <- seq(-4,4,by=0.5)
  #   arch_fin = '_filtro'
  #   contornos <-  niveles
  # }
  # else{
  # niveles <- seq(-10,10,by=5)
  # niveles2 <- seq(-10,10,by=5)
  arch_fin = ''
  contornos <- seq( from=-12,to=12,by=4 )
  # contornos <- contornos[-7]
  # }
  
  
  indcT <- grep( '[0-9]*-[0-9]*-01',as.Date(tie/86400,origin='1950-01-01')  )
  
  # marcas.fecha<- c(tie[1],tie[indcT])
  
  marcas.fecha<- tie[indcT]
  
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
  pp <- ggplot(data=hovmoller,
               aes(y=lon,
                   x=T,
                   fill=anm_d)) 
  pp <- pp + geom_raster(interpolate=TRUE,
                         show.legend = TRUE  )
  
  pp <- pp + scale_fill_manual(values = rev(paleta_color),
                               breaks = rev(levels(temp)),
                               limits = levels(temp))
  
  pp <- pp + geom_contour(data = hovmoller,
                          aes(x = T,
                              y = lon,
                              z = anm),
                          breaks = seq(-12, 10, by = 1),
                          col = 'black',
                          inherit.aes = FALSE  )
  
  indc <- which(hovmoller$anm>-2)
  
  hovmoller$anm[indc] <- hovmoller$anm[indc] + 2
  
  pp <- pp + geom_text_contour(data = hovmoller,
                               aes(y = lon,
                                   x = T,
                                   z = anm),
                               min.size = 20, 
                               size = 14,
                               rotate = FALSE,
                               inherit.aes = FALSE)
  
  pp <- pp + labs(       y = 'Longitud',
                         x = paste0('Fecha ',
                                    lubridate::year(fecha.anterior), '-',
                                    lubridate::year(fecha.actual)),
                         title = paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                                        'Dpto. de Oceanografía - Div. Oceanografía'),
                         subtitle = expression( Zona~Ecuatorial~2*degree~N~a~2*degree~S),
                         caption = 'Fuente: COPERNICUS MARINE\nENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 1981-2009')
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
  
  pp <- pp + theme( axis.text.x = element_text( size = 34, color='black' ),
                    axis.text.y = element_text( size = 34, color='black', hjust = 0.5 ),
                    axis.title.x = element_text( size = 40 ),
                    axis.title.y = element_text( size = 40 ),
                    title = element_text( size = 38 ),
                    plot.caption = element_text( size = 28, hjust = 0))
  
  pp <- pp + guides( fill = guide_legend(barheight = unit(25, "cm"),
                                           barwidth = unit(2,'cm'),
                                           label.theme = element_text(size=30),
                                           title='ANM\ncm',
                                           title.theme = element_text(size=30)) )
  
  return(pp)
}