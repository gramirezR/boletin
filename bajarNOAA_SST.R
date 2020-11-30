cat("\014") 
graphics.off()
rm(list=ls())

# library('RNetCDF')
setwd('D:/programasR/boletin/')
library('stringr')
library('tidyverse')
library('scales')
library('directlabels')
library('maptools')
library('reshape2')
require('RNetCDF')
require('metR')
gshhs.dir <- 'D:/programasR/gshhs/'
carpetaFIg <- 'D:/DHN/NOAA/SST/figuras/'
carpetaanomalia <- 'D:/DHN/NOAA/SST/anomalia15/'
load('costa_Peru_205.RDat')

anioLista <- 1981:2019
nAnios <- length(anioLista)
load(file = 'D:/DHN/NOAA/climatologia_1982-2018.RDat')

puntoAmarillo <- data.frame(x=-c(95,95,95,95,85,85),y=c(5,2,-2,-5,-5,-8))
puntoVerde <- data.frame(x=-c(90,90,83,83),y=c(-3.5,-5,-3.5,-5))
figuraN <- 611########
for (ii in anioLista[27:nAnios]){
  
  archivo <- paste0('D:/DHN/NOAA/SST/sst.day.mean.',ii,'.nc')
  
  lim.lon <- c(180, 290)
  lim.lat <- c(-20, 5)
  
  ncId <- open.nc(archivo)
  
  lon <- var.get.nc(ncId,'lon')
  lat <- var.get.nc(ncId,'lat')
  
  tiempo <- as.POSIXct(var.get.nc(ncId,'time')*86400,origin='1800-01-01')
  
  meses <- unique(lubridate::month(tiempo))
  
  #contador = 12
  
  for (contador in meses){
    for (quincena in 1:2){
      indc <- which( lubridate::month(tiempo)==contador )
      if (length(indc)<15) next
      if (quincena==1){
        temp <- var.get.nc(ncId,'sst',
                           start = c(1,1,indc[1]),
                           count = c(NA,NA,15))
      }else{
        restoQuincena <- length(indc) - 15
        if (restoQuincena<0) next
        temp <- var.get.nc(ncId,'sst',
                           start = c(1,1,indc[1]+15),
                           count = c(NA,NA,restoQuincena))
      }
      SST <- apply(X = temp,MARGIN = c(1,2),FUN = mean,na.rm=TRUE)
      mes <- lubridate::month(contador,abbr=FALSE,label=TRUE)
      anio <- unique(lubridate::year(tiempo[indc]))
      subtitulo.grafico <- paste0(quincena,'a quincena del mes de ',mes,' de ',anio)
      indc.lon <-  which(lon >=lim.lon[1] & lon<=lim.lon[2] )
      indc.lat <-  which(lat >=lim.lat[1]& lat<=lim.lat[2])
      
      temp <- stack(as.data.frame(SST[indc.lon,indc.lat]))$values
      
      malla <- expand.grid(lon[indc.lon],lat[indc.lat])
      
      campoT <- data.frame(x=malla$Var1-360,y=malla$Var2,z=temp)
      
      anomaliaT <- campoT
      
      anomaliaT$z <- campoT$z - climatologiaSST$z
      
      save(file = paste0('D:/NOAA/SST/',mes,ii,'.RData'),
           list = c('campoT','anomaliaT'))
      
      rm(list=c('temp','malla','SST'))
      
      paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
      niveles <- seq(10,31,by=1)
      #######
      png(width=1200,height=800,filename = paste0(carpetaFIg,'PromedioSST','_',sprintf(figuraN,fmt = '%04d'),'.png'))
      pp <- ggplot(data=campoT,aes(x=x,y=y,z=z,fill=z))
      pp <- pp+geom_raster(aes(fill = z),interpolate=TRUE,show.legend = TRUE  )
      # pp <- pp + scale_fill_gradientn( colours = paleta_color,
      #                                  limits = range(niveles))
      pp <- pp + scale_fill_divergent(colours = paleta_color,
                                       limits = range(niveles),
                                      midpoint = 0.0)
      pp <- pp + geom_contour(linetype = 1 ,
                              col = 'black' ,
                              breaks = niveles)
      pp <- pp + geom_text_contour(  stroke = 0.15,skip=0,
                                     min.size = 10,
                                     size = 10,
                                     rotate = FALSE,
                                     check_overlap=TRUE,breaks = niveles)
      pp <- pp + geom_polygon( data=shore,aes(x=long-360,y=lat,group=group),color = 'black', fill = 'grey80',inherit.aes=FALSE  )
      for (kk in 1:length(fronteras)){
        f <- as.data.frame(fronteras[[kk]])
        pp <- pp + geom_point(data=f,aes( x=X1-360,y=X2 ),col='grey30',size=0.05,size=0.05,inherit.aes=FALSE)
      }
      
      pp <- pp + geom_point(data=puntoAmarillo,aes(x=x,y=y),inherit.aes = FALSE,shape=23, fill="lightgray", color="darkred",size=7)
      pp <- pp + geom_point(data=puntoVerde,aes(x=x,y=y),inherit.aes = FALSE,shape=21, fill="blue", color="#FC4E07",size=7)
      pp <- pp + labs(x='Longitud',y='Latitud',
                      title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                                   'Dpto. de Oceanografía - Div. Oceanografía'),
                      subtitle = subtitulo.grafico,
                      caption = 'Fuente: ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2.highres/')
      pp <- pp + theme_bw(   )
      pp <- pp + scale_x_continuous(limits = lim.lon-360,
                                    expand = c(0.01,0),
                                    breaks = seq(from=lim.lon[1],to=lim.lon[2],by=5)-360)
      pp <- pp + scale_y_continuous(limits = lim.lat,
                                    expand = c(0.01,0),
                                    breaks = seq(from=lim.lat[1],to=lim.lat[2],by=2.5))
      pp <- pp + guides( fill = guide_colorbar(  barheight = unit(14, "cm"),
                                                 barwidth = unit(0.85,'cm'),
                                                 label.theme = element_text(size=20),
                                                 title = 'TSM °C',
                                                 title.theme = element_text(size=22)))
      pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                        axis.title.y = element_text( size=28,hjust=0.5  ),
                        axis.text = element_text(size=20),
                        title=element_text(size=26),
                        plot.subtitle=element_text(size=24),
                        plot.caption = element_text(size = 18,hjust = 0))
      plot(pp)
      dev.off()
      #     
      # #######  
      niveles <- seq(-8,10,by=0.5)
      subtitulo.grafico <- paste0('Anomalía de la ',quincena,'a quincena del mes de ',mes,' de ',anio)
      png(width=1200,height=800,filename = paste0(carpetaanomalia,'anomaliaSST','_',sprintf(figuraN,fmt = '%04d'),'.png'))
      pp <- ggplot(data=anomaliaT,aes(x=x,y=y,z=z,fill=z))
      pp <- pp+geom_raster(aes(fill = z),interpolate=TRUE,show.legend = TRUE  )
      pp <- pp + scale_fill_gradientn( colours = paleta_color,
                                       limits = range(niveles))
      pp <- pp + geom_contour(linetype = 1 ,
                              col = 'black' ,
                              breaks = niveles)
      pp <- pp + geom_text_contour(stroke = 0.15,skip=0,
                                   min.size =10,size=10,rotate = FALSE,
                                   check_overlap=TRUE,breaks = niveles)
      pp <- pp + geom_polygon( data=shore,aes(x=long-360,y=lat,group=group),color = 'black', fill = 'grey80',inherit.aes=FALSE  )
      for (kk in 1:length(fronteras)){
        f <- as.data.frame(fronteras[[kk]])
        pp <- pp + geom_point(data=f,aes( x=X1-360,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
      }
      pp <- pp + geom_point(data=puntoAmarillo,aes(x=x,y=y),inherit.aes = FALSE,shape=23, fill="lightgray", color="darkred",size=7)
      pp <- pp + geom_point(data=puntoVerde,aes(x=x,y=y),inherit.aes = FALSE,shape=21, fill="blue", color="#FC4E07",size=7)
      pp <- pp + labs(x='Longitud',y='Latitud',
                      title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                                   'Dpto. de Oceanografía - Div. Oceanografía'),
                      subtitle = subtitulo.grafico,
                      caption = 'Fuente: ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2.highres/')
      pp <- pp + theme_bw(   )
      pp <- pp + scale_x_continuous(limits = lim.lon-360,
                                    expand = c(0.01,0),
                                    breaks = seq(from=lim.lon[1],to=lim.lon[2],by=5)-360)
      pp <- pp + scale_y_continuous(limits = lim.lat,
                                    expand = c(0.01,0),
                                    breaks = seq(from=lim.lat[1],to=lim.lat[2],by=2.5))
      pp <- pp + guides( fill = guide_colorbar(  barheight = unit(14, "cm"),
                                                 barwidth = unit(0.85,'cm'),
                                                 label.theme = element_text(size=20),
                                                 title = 'ATSM °C',
                                                 title.theme = element_text(size=22)))
      pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                        axis.title.y = element_text( size=28,hjust=0.5  ),
                        axis.text = element_text(size=20),
                        title=element_text(size=26),
                        plot.subtitle=element_text(size=24),
                        plot.caption = element_text(size = 18,hjust = 0))
      plot(pp)
      dev.off()
      #####
      figuraN <- figuraN + 1}
  }
  close.nc(ncId)
}
#########






