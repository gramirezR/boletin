hovmoller_costero_salinidad <- function(lista,poligono,lim.lat,franja){
  require('metR')
  poligono <- as.data.frame(poligono)
  variable <- 'so'
  names(poligono) <- c('lon','lat')
  
  indcNeg <- which(poligono$lon<0)
  
  # if (length(indcNeg)> 0 ){
  #   poligono$lon[indcNeg] <- 360+poligono$lon[indcNeg] }
  #

  arch <- lista[3]
  ncin <- open.nc(arch)
  sal <- var.get.nc(ncin,variable,start = c(1,1,1,1),
                               count = c(4320,2041,1,1),  unpack = TRUE)
  lon <- var.get.nc(ncin,'longitude')
  lat <- var.get.nc(ncin,'latitude')
  
  malla <- expand.grid(lon=lon,lat=lat)
  
  ####### ARMADO DE LA MATRIZ DE DATOS QUE ESaTN DENTRO DEL POLiGONO ########
  
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
  
  datos <- matrix( nrow = length(latitudes), ncol = length(lista) ) 
  
  tiempo <- array( dim = length(lista) )
  
  sal <- 0*sal
  for ( ii in 1:length(lista) ) {
    tryCatch({
    ncin <- open.nc(lista[ii])
    print(lista[ii])
    tiempo[ii] <- var.get.nc(ncin,'time')
    sal <- var.get.nc(ncin,variable,start = c(1,1,1,1),
                            count =c(4320,2041,1,1), collapse = TRUE, unpack = TRUE)
    close.nc(ncin)
    tempo <- utils::stack(as.data.frame(sal))$values[indc] # horas dede 1950-01-01
    for (jj in 1:length(latitudes)){
      indc2 <- Y==latitudes[jj]
      datos[jj,ii] <- mean(tempo[indc2], na.rm = TRUE)
    }},
    error = function(e) e,
    finally = print("Hello")
    )
    
  }
  
  
  Z <- stack(as.data.frame(datos)) 
  
  ######### DIAGRAMA DE HOVMOLLER ################
  malla <- expand.grid(lat=latitudes,tiempo=tiempo)
  
  hovmoller <- data.frame( tiempo = as.matrix(malla$tiempo),
                           lat = as.matrix(malla$lat),
                           sal = as.matrix(Z$values)  )
  
  suave <- mgcv::gam(sal~te(tiempo,lat,k=c(20,15)),data=hovmoller )
  # tiempo2 <- as.Date(tiempo/24,origin='1950-01-01')
  tiempo <- seq(from=min(tiempo,na.rm = TRUE), to= max(tiempo,na.rm = TRUE),by=24)
  malla <- expand.grid(lat = latitudes,tiempo = tiempo)
  
  pred <- predict(suave,newdata = as.data.frame(malla))
  
  # tiempo <- as.POSIXct(tiempo*3600, origin='1950-01-01') # segundos desde 1950-01-01
  
  Z <- as.matrix(pred)
  hovmoller <- data.frame( tiempo = malla$tiempo,
                           lat = as.matrix(malla$lat),
                           sal = Z  )
  
  fechas <- as.Date( tiempo/24,origin='1950-01-01')
  
  indcT <- grep( '[0-9]*-[0-9]*-01',fechas  )
  
  marcas.fecha<- tiempo[indcT]
  
  etqt.fecha <- months(fechas[indcT],abbreviate =TRUE)
  
  # indcT <- grep( '[0-9]*-[0-9]*-15',as.Date(tiempo/24,origin='1950-01-01')  )
  # 
  # indcOrden <- sort(c(marcas.fecha,tiempo[indcT]),index.return=TRUE)
  # 
  # etqt.fecha <- c( etqt.fecha,rep('.',length(indcOrden$ix) ) )
  # 
  # marcas.fecha <- marcas.fecha[indcOrden$ix] 
  # 
  # etqt.fecha <- etqt.fecha[indcOrden$ix]
  
  titulo.fig <- paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                       'Dpto. de OceanografÍa - Div. OceanografÍa')

    
    subtitulo <- 'Salinidad: '
    titulo.barra <- 'UPS'
  
  # paleta_color <- cptcity::cpt('ncl_StepSeq25')
    niveles <- c(seq(31,36.2,by=0.2))
    niveles_barra <- c(seq(from=31,to=34.7,by=0.8),c(34.8,35.1, by=0.75),seq(35.4,36.2,by=0.8))
    
    paleta01 <- colorRampPalette(colors = c("#FF0000","#FF0200","#FF0500","#FF0700","#FF0A00","#FF0C00","#FF0F00","#FF1200","#FF1400",
                                            "#FF1700","#FF1900","#FF1C00","#FF1E00","#FF2100","#FF2400","#FF2600","#FF2900","#FF2B00",
                                            "#FF2E00","#FF3000","#FF3300","#FF3600","#FF3800","#FF3B00","#FF3D00","#FF4000","#FF4200",
                                            "#FF4500","#FF4800","#FF4A00","#FF4D00","#FF4F00","#FF5200","#FF5500","#FF5700","#FF5A00",
                                            "#FF5C00","#FF5F00","#FF6100","#FF6400","#FF6700","#FF6900","#FF6C00","#FF6E00","#FF7100",
                                            "#FF7300","#FF7600","#FF7900","#FF7B00","#FF7E00","#FF8000","#FF8300","#FF8500","#FF8800",
                                            "#FF8B00","#FF8D00","#FF9000","#FF9200","#FF9500","#FF9700","#FF9A00","#FF9D00","#FF9F00",
                                            "#FFA200","#FFA400","#FFA700","#FFAA00","#FFAC00","#FFAF00","#FFB100","#FFB400","#FFB600",
                                            "#FFB900","#FFBC00","#FFBE00","#FFC100","#FFC300","#FFC600","#FFC800","#FFCB00","#FFCE00",
                                            "#FFD000","#FFD300","#FFD500","#FFD800","#FFDA00","#FFDD00","#FFE000","#FFE200","#FFE500",
                                            "#FFE700","#FFEA00","#FFEC00","#FFEF00","#FFF200","#FFF400","#FFF700","#FFF900","#FFFC00",
                                            "#FFFF00"))
    paleta02 <- colorRampPalette(colors = c('#00BFFF','#0000CD'))
    # paleta03 <- colorRampPalette(colors = c("#88A52F","#7FAC25","#B9E1AC"))(11)
    paleta03 <- colorRampPalette(colors = c("#007F66","#028066","#058166","#078266","#0A8366","#0C8566","#0F8666","#118766","#148866",
                                            "#168A66","#198B66","#1B8D66","#1E8E66","#208F66","#239166","#259266","#289366","#2B9566",
                                            "#2E9666","#309766","#339966","#369A66","#389B66","#3B9C66","#3D9E66","#409F66","#42A066",
                                            "#45A166","#47A366","#4AA466","#4CA566","#4FA766","#51A866","#54AA66","#56AB66","#59AC66",
                                            "#5CAD66","#5EAE66","#61B066","#64B166","#67B266","#69B366","#6CB466","#6EB666","#71B766",
                                            "#73B866","#76B966","#78BB66","#7BBC66","#7DBD66","#80BF66","#82C066","#85C166","#87C366",
                                            "#8AC466","#8DC666","#8FC766","#92C866","#95CA66","#97CB66","#9ACC66","#9DCD66","#9FCF66",
                                            "#A2D066","#A4D166","#A7D266","#AAD466","#ACD566","#AED666","#B1D866","#B3D966","#B6DA66",
                                            "#B8DC66","#BBDD66","#BDDE66","#C0DF66","#C3E166","#C6E266","#C8E366","#CBE466","#CEE566",
                                            "#D0E766","#D3E866","#D5E966","#D8EA66","#DAEC66","#DDED66","#DFEE66","#E2F066","#E4F166",
                                            "#E7F266","#E9F466","#ECF566","#EEF666","#F1F866","#F4F966","#F6FA66","#F9FC66","#FCFD66",
                                            "#FFFF66"))
    
    paleta_color <- c(rev(paleta03(38)), paleta02(4), paleta01(11))
  
  marcas_y <- seq(lim.lat[1],lim.lat[2],by=2)
  etiquetas_y <- unlist(lapply( as.vector(marcas_y),
                                function(x) if( x<0 ) {paste0(-x,'S')}else if(x>0){paste0(x,'N')}else{x} )   )
  png(width=1200, height=1200, filename = paste0(raiz,'figuras/hovmoller_salinidad.png'  ))
  pp <- ggplot( data=hovmoller,aes(x=tiempo,y=lat,fill=sal) )
  pp <- pp + geom_raster(aes(fill = sal),interpolate=TRUE,show.legend = TRUE  )
  pp <- pp + scale_fill_gradientn(colours= paleta_color,limits =range(niveles),breaks=niveles)
  pp <- pp + stat_contour(data=hovmoller, aes(x=tiempo,y=lat,z=sal),
                          breaks = niveles,col='black' ,inherit.aes=FALSE )
  
  # pp <- pp + geom_text_contour(data=hovmoller, aes(x=tiempo,y=lat,z=atsm),
  #                              stroke = 0.15,skip=0,min.size =15,size=12,rotate = FALSE,
  #                              breaks = niveles)
  # 
  pp <- pp + geom_text_contour(data=hovmoller,aes(x=tiempo,y=lat,z=sal,label=..level..),
                               rotate=FALSE,size=12,stroke=0.10,
                               check_overlap = TRUE)
  # pp <- pp + geom_dl(data=hovmoller,aes(x=tiempo,y=lat,z=atsm,label=..level..),
  #                    breaks = niveles,col='black', method=list('bottom.pieces', cex=1.3), stat="contour")
  # 
  
  pp <- pp + labs(x='Fecha',y='Latitud',
                  title=titulo.fig,
                  subtitle=paste0(subtitulo,
                                  format(as.Date(tiempo[1]/24,origin='1950-01-01'),'%B-%d-%Y'),' a ',
                                  format(as.Date(tiempo[length(tiempo)]/24,origin='1950-01-01'),'%B-%d-%Y'),
                                  paste0('\nFranja de ',franja,' millas')),
                  caption='Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0)\nClimatolog?a: 1981-2009')
  
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
                   title = element_text( size = 30 ),
                   plot.subtitle = element_text(size = 34),
                   plot.caption = element_text( size = 22,hjust = 0),
                   axis.ticks.length=unit(.25, "cm"))
  plot(pp)
  dev.off()
  
}