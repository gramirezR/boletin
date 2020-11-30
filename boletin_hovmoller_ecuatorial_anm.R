boletin_hovmoller_ecuatorial_anm <- function(lista ,
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
  
  
  ###### DEFINICIÓN DE LA REGIÓN
  
  
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
  
  
  # c(140,280)
  
  indc <- which(lon < 0)
  
  if ( length(indc) > 0){
    indc.lon1 <- which((lon >= region.lon[1]) & (lon <= 180) )
    indc.lon2 <- which((lon > -180) & (lon <= (region.lon[2] - 360)  ) )
    lon1 <- unique( lon[indc.lon1]  ) 
    n.lon1 <- diff( range(indc.lon1)  ) + 1
    lon2 <- unique( lon[indc.lon2]  ) 
    n.lon2 <- diff( range(indc.lon2)  ) + 1
    n.lon <- n.lon1 +n.lon2
    lon <- c( lon[indc.lon1] , lon[indc.lon2] + 360)
    
  }else{
    indc.lon <- which((lon >= region.lon[1]) & (lon <= region.lon[2]) )
    lon <- unique( lon[indc.lon]  ) 
    n.lon <- diff( range(indc.lon)  ) + 1
  }
  
  
  #######

  lat <- unique( lat[indc.lat]  ) 
  n.lat <- diff( range(indc.lat)  ) + 1
  n.tie <- narchs
  Sla <- array( dim=c(n.lon,n.tie) )
  tie <- array(dim=n.tie)
  for (ii in 1:narchs){
    tryCatch({
      ncin <- open.nc(lista[ii])
      tie[ii] <- 24*3600*var.get.nc(ncin,'time')
      if (exists('indc.lon1')){
        M1 <- var.get.nc(ncin,'sla',start = c(min(indc.lon1),
                                             min(indc.lat), 1 ),
                        count = c(n.lon1,n.lat,1 ))
        M2 <- var.get.nc(ncin,'sla',start = c(min(indc.lon2),
                                              min(indc.lat), 1 ),
                         count = c(n.lon2,n.lat,1 ))
        M <- rbind(M1,M2)
      }else{
        M <- var.get.nc(ncin,'sla',start = c(min(indc.lon),
                                             min(indc.lat), 1 ),
                        count = c(n.lon,n.lat,1 ))
      }
      Sla[,ii]  <- (0.01*rowMeans(M,na.rm=TRUE))
      close.nc(ncin)
    },error=function(e) e, finally=print(ii)    )
    
  }
  

  ###### ARMADO DEL DATA.FRAME PARA GGPLOT#####
  indcNanX <- which(is.na( tie ))
  
  if ( length(indcNanX)>0 ){
    x <- tie[-indcNanX]
  } else{
    x <- tie
  }
  
  indcNanY <- which(is.na( lon ))
  if ( length(indcNanY)>0 ){
    y <- lon[-indcNanY]
  } else{
    y <- lon
  }
  
  tiempo <- seq( from=min(x),to=max(x),length.out = length(tie)  )
  longitud <- seq( from=min(y),to=max(y),length.out = length(lon)  )
  
  
  modelo.gam <-  TRUE
  if (modelo.gam){
    malla <- expand.grid(lon=y,T=x)
    
    malla_interpolacion <- expand.grid(lon=longitud,T=tiempo)
    
    Z <- stack( as.data.frame(Sla) )
    
    indcNaZ <- which(is.na(Z$values))
    
    Z <- Z[-indcNaZ,]
    
    hovmoller_pre <- data.frame(    T = malla$T,
                                lon = malla$lon,
                                anm = Z$values)
    
    suave <- gam(anm ~ te(T,lon,k=c(15,10)),data = hovmoller_pre)
    pred <- predict(suave,newdata = as.data.frame(malla_interpolacion) )
    Z <- as.matrix( pred )
    hovmoller <- data.frame(     T = malla_interpolacion$T,
                                 lon = malla_interpolacion$lon,
                                 anm = Z)
  }else{
    nimpar <- 3 
    sla2 <- imagine::convolution2D(Sla,
                                   kernel=as.matrix(replicate(nimpar^2,1),nrow=nimpar,ncol=nimpar)/nimpar^2,
                                   times=2,noNA=FALSE)
    indna <- is.na(sla2)
    
    sla2[indna] <- Sla[indna]
    
    Sla <- sla2
    
    rm('sla2')
    malla <- expand.grid(lon=lon,T=tie)
    Z <- stack( as.data.frame(Sla) )
    
    hovmoller <- data.frame(   T  = malla$T,
                               lon = malla$lon,
                               anm = Z$values)
    
  }
  
  
  paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
  if (exists('pp')){
    rm(pp)
  }
  
  niveles <- seq( from=-30,to=30,by=4 )
  
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
  #######      
  
  contornos <- seq( from=-30,to=30,by=2 )
  ######
  pp <- ggplot(data=hovmoller,aes(y=lon,x=T,fill=anm)) 
  pp <- pp + geom_raster(aes(fill = anm),interpolate=FALSE,show.legend = TRUE  )
  pp <- pp + scale_fill_gradientn(colours = paleta_color,breaks = niveles,limits =range(niveles))
  
  pp <- pp + stat_contour(  data = hovmoller, aes(y=lon,x=T,z=anm),
                            breaks = contornos,
                            col ='black' ,
                            inherit.aes = FALSE )
  pp <- pp + geom_text_contour(data=hovmoller,aes(y=lon,x=T,z=anm),
                               stroke = 0.15,
                               size=18,rotate = FALSE,check_overlap = TRUE)
  
  pp <- pp + labs(       y = 'Longitud',
                         x = 'Fecha 2018-2019',
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