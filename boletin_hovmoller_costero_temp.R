hovmoller_costero <- function(lista,poligono,variable,lim.lat,franja){
  require('metR')
  poligono <- as.data.frame(poligono)
  
  names(poligono) <- c('lon','lat')
  
  indcNeg <- which(poligono$lon<0)
  
  if (length(indcNeg)> 0 ){
     poligono$lon[indcNeg] <- 360+poligono$lon[indcNeg] }
  
  arch <- lista[1]
  ncin <- open.nc(arch)
  sst <- 0.01*var.get.nc(ncin,variable)
  lon <- var.get.nc(ncin,'lon')
  lat <- var.get.nc(ncin,'lat')
  
  malla <- expand.grid(lon=lon,lat=lat)
  
  ####### ARMADO DE LA MATRIZ DE DATOS QUE ESTÁN DENTRO DEL POLÍGONO ########
  
  close.nc(ncin)
  
  # cortar.lista <- length(lista) - dias.atras
  # 
  # lista <- lista[-seq(1:cortar.lista)]
  
  indc <- which(point.in.polygon( malla$lon,malla$lat,
                                  poligono$lon,poligono$lat )==1)
  
  # X <- malla$lon[indc]
  Y <- malla$lat[indc]
  latitudes <- unique(malla$lat[indc])
  # tiempo <- seq(fecha.anterior+1,fecha.actual,by='day'  )
  
  datos <- matrix( nrow=length(latitudes), ncol= (length(lista)-4) ) 
  tiempo <- array( dim=(length(lista)-4) )
  
  for ( ii in 3:(length( lista)-2) ) {
    sst <- 0*sst
    for ( kk in (ii-2):(ii+2)){
      arch <- lista[kk]
      ncin <- open.nc(arch)
      if (kk==ii){
        tiempo[kk-2] <- var.get.nc(ncin,'time')
      }
      sst <-  0.01*var.get.nc(ncin,variable) + sst
      close.nc(ncin)
    }
    sst <- sst/5
    Z <- sst[indc]
    for ( jj in 1:length(latitudes)){
      indc2 <- Y==latitudes[jj]
      datos[jj,ii-2] = mean( Z[indc2],na.rm=TRUE  )
    }   
    
  }
  
  
  
  ######### DIAGRAMA DE HOVMOLLER ################
  malla <- expand.grid(lat=latitudes,tiempo=tiempo)
  
  Z <- stack( as.data.frame(datos) )
  
  
  hovmoller <- data.frame( tiempo = as.matrix(malla$tiempo),
                           lat = as.matrix(malla$lat),
                           atsm = as.matrix(Z$values)  )
  
  suave <- mgcv::gam(atsm~te(tiempo,lat,k=c(20,15)),data=hovmoller )
  
  pred <- predict(suave,newdata = as.data.frame(malla))
  
  Z <- as.matrix(pred)
  hovmoller <- data.frame( tiempo = malla$tiempo,
                           lat = as.matrix(malla$lat),
                           atsm = Z  )
  
 
  indcT <- grep( '[0-9]*-[0-9]*-01',as.Date(tiempo/86400,origin='1981-01-01')  )
  
  marcas.fecha<- tiempo[indcT]
  
  etqt.fecha <- months(as.Date( marcas.fecha/86400,origin='1981-01-01'),abbreviate =TRUE)
  
  indcT <- grep( '[0-9]*-[0-9]*-15',as.Date(tiempo/86400,origin='1981-01-01')  )
  
  indcOrden <- sort(c(marcas.fecha,tiempo[indcT]),index.return=TRUE)
  
  etqt.fecha <- c( etqt.fecha,rep('.',length(indcOrden$ix) ) )
  
  marcas.fecha <- marcas.fecha[indcOrden$ix] 
  
  etqt.fecha <- etqt.fecha[indcOrden$ix]

  titulo.fig <- paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                       'Dpto. de Oceanografía - Div. Oceanografía')
  if (pracma::strcmp(variable,'sst_anomaly')){
    niveles <- seq( from=-6,to=6,by=0.5 )
    subtitulo <- 'Anomalía de la Temperatura Superficial del Mar: '
    titulo.barra <- 'ATSM\n°C'
  }else{
    niveles <-  seq( from=10,to=30,by=1)

    subtitulo <- 'Temperatura Superficial del Mar: '
    titulo.barra <- 'TSM\n°C'
  }
  
  paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')

  marcas_y <- seq(lim.lat[1],lim.lat[2],by=2)
  etiquetas_y <- unlist(lapply( as.vector(marcas_y),
                                function(x) if( x<0 ) {paste0(-x,'S')}else if(x>0){paste0(x,'N')}else{x} )   )
  
  pp <- ggplot( data=hovmoller,aes(x=tiempo,y=lat,fill=atsm) )
  pp <- pp + geom_raster(aes(fill = atsm),interpolate=TRUE,show.legend = TRUE  )
  pp <- pp + scale_fill_gradientn(colours= paleta_color,limits =range(niveles),breaks=niveles)
  pp <- pp + stat_contour(data=hovmoller, aes(x=tiempo,y=lat,z=atsm),
                          breaks = niveles,col='black' ,inherit.aes=FALSE )
  
  # pp <- pp + geom_text_contour(data=hovmoller, aes(x=tiempo,y=lat,z=atsm),
  #                              stroke = 0.15,skip=0,min.size =15,size=12,rotate = FALSE,
  #                              breaks = niveles)
  # 
  pp <- pp + geom_text_contour(data=hovmoller,aes(x=tiempo,y=lat,z=atsm,label=..level..),
                               rotate=FALSE,size=12,stroke=0.15,
                               check_overlap = TRUE)
  # pp <- pp + geom_dl(data=hovmoller,aes(x=tiempo,y=lat,z=atsm,label=..level..),
  #                    breaks = niveles,col='black', method=list('bottom.pieces', cex=1.3), stat="contour")
  # 

  pp <- pp + labs(x='Fecha',y='Latitud',
                  title=titulo.fig,
                  subtitle=paste0(subtitulo,
                                  format(fecha.anterior+1,'%B-%d-%Y'),' a ',
                                  format(fecha.actual,'%B-%d-%Y'),
                                  paste0('\nFranja de ',franja,' millas')),
                  caption='Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0)\nClimatología: 1981-2009')
 
  pp <- pp + scale_x_continuous( breaks = marcas.fecha,
                                 labels = etqt.fecha,
                                 expand = c(0,0),
                                 limits=c(tiempo[1],last(tiempo)))
  pp <- pp + scale_y_continuous(limits = range(marcas_y),
                                breaks = marcas_y  ,
                                expand = c(0,0),
                                labels = etiquetas_y)
  
   pp <- pp + guides( fill = guide_colorbar(  barheight = unit(20, "cm"),
                                             barwidth = unit(1,'cm'),
                                             label.theme = element_text(size=24),
                                             title=titulo.barra,
                                             title.theme = element_text(size=28)))
  
  pp <- pp + theme_bw()
  pp <- pp + theme(axis.text.x = element_text( size = 34,color='black',angle = 90,vjust = 0.5 ),
                   axis.text.y = element_text( size = 34,color='black' ),
                   axis.title.x = element_text( size = 40 ),
                   axis.title.y = element_text( size = 40 ),
                   title = element_text( size = 26 ),
                   plot.subtitle = element_text(size = 28),
                   plot.caption = element_text( size = 28,hjust = 0),
                   axis.ticks.length=unit(.25, "cm"))
  return(pp)
  
}