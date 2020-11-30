cat('\014') 
graphics.off()
rm(list=ls())

library('RNetCDF')
library('stringr')
library('tidyverse')
library('scales')
library('directlabels')
library('maptools')
library('reshape2')
library('ggquiver')
library('RCurl')
library('metR')
####

setwd("D:/programasR/boletin")

fecha.actual <- lubridate::today() -2

 # fecha.actual <- as.Date('2007-01-11')-2
 
dias.atras <- 29

fecha.anterior <- fecha.actual-dias.atras
#############
bajar <- TRUE
lista.dias <- seq(from=fecha.anterior,to=fecha.actual,by='day')
lista.dias <- gsub(lista.dias,pattern = '-',replacement = '/') 
fecha.titulo <- gsub(pattern = '/',replacement = '',x = lista.dias)
datos.dir <- paste0('D:/boletin/viento/ASCAT/datos/')

figuras <- paste0('D:/boletin/viento/ASCAT/figuras/')

if (!dir.exists(datos.dir )){
  dir.create(datos.dir ,recursive = TRUE)
}

ftp.dir <- paste0('ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/MWF/L3/ASCAT/Daily/Netcdf/')
                  #ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/MWF/L3/ASCAT/

   lista.ascat <- lapply(lista.dias,function(x) paste0(ftp.dir,x,'/'))
lista.archivos <- list.files(path = datos.dir,pattern = '*.nc')
    lista.dias <- gsub(lista.dias,pattern = '/',replacement = '') 
          indc <- vector('list',length = length(lista.dias))
          no.esta <- indc 
    for (ii in 1:length(lista.dias)){
      esta <- grep(pattern = paste0('^',lista.dias[[ii]],'00_*'),x = lista.archivos)
      if (length(esta)>0){
        indc[[ii]] <- esta
      }else{
        no.esta[[ii]] <- ii 
      }
    }
          
 indc <- unlist(indc)
 no.esta <- unlist(no.esta)
 
 lista.no.esta <- lista.dias[no.esta]

 fechas.ascat <- gsub(substring(text=lista.ascat,first=80,last=89),pattern = '/',replacement = '')
 
  ascat.bajar <- unlist(lapply(lista.no.esta,function(y) grep(pattern=y,x = fechas.ascat,fixed = TRUE)))
 
if (bajar){
lapply(lista.ascat[ascat.bajar] , function (x){
         tryCatch({bajar <- unlist(strsplit(getURL(x,dirlistonly = TRUE,verbose=TRUE),'\r\n'))
                   descomp <- paste0(datos.dir, bajar)
                  arch.sal <- str_extract(descomp,pattern='.*[^.bz2]')
                  closeAllConnections()
                  if (!file.exists(arch.sal)){
                  download.file( url=paste0(x, bajar), destfile=descomp,
                                 method = "auto",quiet = FALSE, mode="wb", cacheOK = TRUE  )
                  R.utils::bunzip2(descomp,
                                   destname=arch.sal,
                                   skip  =FALSE,
                                   remove=TRUE,
                                   overwrite=TRUE) }},error=function(e){ print('no existe este archivo') })
                  
               })}
###########################################
lista <- list.files(datos.dir)

lista <- unlist(lapply(lista,function(x) paste0(datos.dir,x)))

indc.lista <- lapply(fecha.titulo,
                     function (y) grep(pattern = paste0(y,'00_2'),x=lista,fixed = TRUE))

indc.lista <- unlist(indc.lista)
lista <- lista[indc.lista]
#############################
#
#   time -------> 1 :segundos desde 1900-01-01
#   depth ------> 1: a 10 metros de altura
#   latitude ---> 641
#   longitude --> 1440
#   wind_speed -> factor 0.01
#   eastward_wind -> factor 0.01
#   northward_wind -> factor 0.01
#
#

lon.lim <- c(-90,-70)
lat.lim <- c(-20,2)

factor.viento <- 0.01
######################
narchs <- length(lista)
if(exists('pp'))rm(pp)

# paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
paleta_color <- c('#f0f0f0','#b4b4b4','#7878f7','#3c3cfb','#6000ff','#0040ff',
                  '#0080ff','#00bfff','#00ffff','#2bffd4','#55ffaa','#80ff80',
                  '#aaff55','#d4ff2b','#ffff00','#ffd400','#ffaa00','#ff8000',
                  '#ff5500','#ff2b00','#ff0000','#d40000','#aa0000','#800000')
niveles <- seq(from=0,to=16,by=2)
########################################33
tituloprom <- paste0('promedio del ',fecha.anterior, ' al ',fecha.actual)
tituloprom <- paste0('promedio del ',fecha.anterior, ' al ',fecha.actual)
png(file=paste0(figuras,'mapa_viento_%03d.png'),width=1000,height=1200,type='windows')
for (ii in 1:narchs){
  fecha.titulo <- as.Date(substring(text =lista[ii],first = 31, last = 40 ),format='%Y%m%d')
  fecha.titulo <- format(fecha.titulo,format='%d %B %Y')
ncId <- open.nc(lista[ii])

lat <- var.get.nc(ncId,'latitude')
lon <- var.get.nc(ncId,'longitude') 

ind.lon <- (lon>=lon.lim[1])&(lon<=lon.lim[2])
ind.lat <- (lat>=lat.lim[1])&(lat<=lat.lim[2])

cuales.lon <- which(ind.lon)
cuales.lat <- which(ind.lat)

 u <- factor.viento*var.get.nc(ncId,'eastward_wind',
                               start=c(cuales.lon[1],cuales.lat[1],1,1),
                               count=c(length(cuales.lon),length(cuales.lat),1,1)) 
 v <- factor.viento*var.get.nc(ncId,'northward_wind',
                               start=c(cuales.lon[1],cuales.lat[1],1,1),
                               count=c(length(cuales.lon),length(cuales.lat),1,1)) 
 
close.nc(ncId)

malla <- expand.grid(lon=lon[cuales.lon],lat=lat[cuales.lat])

 magnitud <- sqrt(u^2 + v^2)

Z <- stack( as.data.frame(magnitud) )
u <- stack( as.data.frame(u) )
v <- stack( as.data.frame(v) )
mapa <- data.frame( lon = as.matrix(malla$lon),
                    lat = as.matrix(malla$lat),
                    mag = as.matrix(Z$values),
                    u = as.matrix(u$values/Z$values),
                    v = as.matrix(v$values/Z$values))


quitar <- floor(seq(from=1,to=dim(u)[1],length.out = 1000)) 
 vectores <- mapa[quitar,]
 if (ii==1){
   promedioV <- vectores
   promedioC <- mapa 
 }else{
   promedioC$mag <- promedioC$mag + mapa$mag
   promedioC$u <- promedioC$u + mapa$u
   promedioC$v <- promedioC$v + mapa$v
   promedioV$mag <- promedioV$mag + vectores$mag
   promedioV$u <- promedioV$u + vectores$u
   promedioV$v <- promedioV$v + vectores$v
 }
rm(list=c('magnitud','Z','u','v'))

load('costa_Peru_202.RDat') 
shore$long <- shore$long-360
###########################
marcas_x <- seq(lon.lim[1],lon.lim[2],by=10)

etiquetas_x <- unlist(lapply( (marcas_x+360 ) ,
                              function(x){ 
                                if (x > 180){
                                  paste0(as.vector( as.character(360-x)),'W') 
                                }else if( x< 180) {
                                  paste0( x ,'E')
                                }else{
                                  as.character(x)
                                }
                              }) )

marcas_y <- seq(from=lat.lim[1],to=lat.lim[2],by=5)
etiquetas_y <- unlist(lapply( as.vector(marcas_y),
                              function(x) if( x<0 ) {paste0(-x,'S')}else if(x>0){paste0(x,'N')}else{x} )   )

###########################
pp <- ggplot(data=mapa,aes(x=lon,y=lat,fill=mag))
pp <- pp + geom_raster(interpolate=TRUE)
pp <- pp + scale_fill_gradientn(colours = paleta_color,
                                 breaks = niveles,
                                 labels = as.character(niveles),
                                limits=range(niveles))
pp <- pp + stat_contour( data=mapa,aes(x=lon,y=lat,z=mag),
                         bins = 7,
                         col ='black'  )
pp <- pp + geom_quiver(data=vectores,
                       aes(x=lon,y=lat,u=u,v=v),
                       vecsize = 2,
                       size = 1,
                       inherit.aes=FALSE)
# pp <- pp + geom_text_contour( data=mapa, aes(x=lon,y=lat,z=mag),
#                               stroke = 0.15,skip=0,size=10,
#                               rotate = FALSE,check_overlap = TRUE,col='white',
#                               stroke.color = 'black')
pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),color = 'black',
                         fill = 'grey80',inherit.aes=FALSE  )
for (kk in 1:length(fronteras)){
  f <- as.data.frame(fronteras[[kk]])
  pp <- pp + geom_point(data=f,aes( x=X1-360,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
}
pp <- pp + scale_x_continuous(limits=range(lon.lim),
                              expand=c(0,0),
                              breaks = marcas_x,
                              labels = etiquetas_x)
pp <- pp + scale_y_continuous(limits=range(lat.lim),
                              expand=c(0,0),
                              breaks = marcas_y,
                              labels = etiquetas_y)
pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle=paste0('Campo de viento ',fecha.titulo ),
                caption='Fuente: Remote Sensing Systems ASCAT C-2015, Daily Ocean Vector Winds on 0.25 deg grid, Version 02.1')

pp <- pp + theme( axis.title.x = element_text( size=24,hjust=0.5  ),
                  axis.title.y = element_text( size=24,hjust=0.5  ),
                  axis.text = element_text(size=20),
                  title=element_text(size=26),
                  plot.subtitle=element_text(size=24),
                  plot.caption = element_text(size = 18,hjust = 0))
pp <- pp + guides( fill = guide_colorbar(  barheight = unit(11, "cm"),
                                           barwidth = unit(1,'cm'),
                                           label.theme = element_text(size=20),
                                           title = 'm/s',
                                           title.theme = element_text(size=20)
))
plot(pp)
}
dev.off()

################################# ARMAR GIF ANIMADO ####################

system(paste0('D:/ffmpeg/bin/ffmpeg.exe -y -framerate 1/0.5 -i ',
              figuras,'mapa_viento_%03d.png -r 30 ',figuras,'mapa_viento.gif'))
#################################
promedioV$mag <- promedioV$mag/narchs
promedioV$u <- promedioV$u/narchs
promedioV$v <- promedioV$v/narchs

promedioC$mag <- promedioC$mag/narchs
promedioC$u <- promedioC$u/narchs
promedioC$v <- promedioC$v/narchs


png(file=paste0(figuras,'mapa_viento_promedio.png'),width=1000,height=1200,type='windows')
pp <- ggplot(data=promedioC,aes(x=lon,y=lat,fill=mag))
pp <- pp + geom_raster(interpolate=TRUE)
pp <- pp + scale_fill_gradientn(colours = paleta_color,
                                breaks = niveles,
                                labels = as.character(niveles),
                                limits=range(niveles))
pp <- pp + stat_contour( data=promedioC ,aes(x=lon,y=lat,z=mag),
                         bins = 7,
                         col ='black'  )
pp <- pp + geom_quiver(data=promedioV,
                       aes(x=lon,y=lat,u=u,v=v),
                       vecsize = 2,
                       size = 1,
                       inherit.aes=FALSE)
# pp <- pp + geom_text_contour( data=mapa, aes(x=lon,y=lat,z=mag),
#                               stroke = 0.15,skip=0,size=10,
#                               rotate = FALSE,check_overlap = TRUE,col='white',
#                               stroke.color = 'black')
pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),color = 'black',
                         fill = 'grey80',inherit.aes=FALSE  )
for (kk in 1:length(fronteras)){
  f <- as.data.frame(fronteras[[kk]])
  pp <- pp + geom_point(data=f,aes( x=X1-360,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
}
pp <- pp + scale_x_continuous(limits=range(lon.lim),
                              expand=c(0,0),
                              breaks = marcas_x,
                              labels = etiquetas_x)
pp <- pp + scale_y_continuous(limits=range(lat.lim),
                              expand=c(0,0),
                              breaks = marcas_y,
                              labels = etiquetas_y)
pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle=paste0('Campo de viento ',tituloprom ),
                caption='Fuente: Remote Sensing Systems ASCAT C-2015, Daily Ocean Vector Winds on 0.25 deg grid, Version 02.1')

pp <- pp + theme( axis.title.x = element_text( size=24,hjust=0.5  ),
                  axis.title.y = element_text( size=24,hjust=0.5  ),
                  axis.text = element_text(size=20),
                  title=element_text(size=26),
                  plot.subtitle=element_text(size=24),
                  plot.caption = element_text(size = 18,hjust = 0))
pp <- pp + guides( fill = guide_colorbar(  barheight = unit(11, "cm"),
                                           barwidth = unit(1,'cm'),
                                           label.theme = element_text(size=20),
                                           title = 'm/s',
                                           title.theme = element_text(size=20)
))
plot(pp)
dev.off()
####################################################
load(file = 'viento_climat_2000-2014.RData')
paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
niveles <- seq(from=-8,to=8,by=2)
mag.prom <- sqrt(u_p^2+v_p^2)
narchs <- length(lista)
png(file=paste0(figuras,'mapa_anomalia_viento_%03d.png'),width=1000,height=1200,type='windows')
for (ii in 1:narchs){
  fecha.titulo <- as.Date(substring(text =lista[ii],first = 31, last = 40 ),format='%Y%m%d')
  fecha.titulo <- format(fecha.titulo,format='%d %B %Y')
  ncId <- open.nc(lista[ii])
  
   lat <- var.get.nc(ncId,'latitude')
   lon <- var.get.nc(ncId,'longitude') 
  # 
  # ind.lon <- (lon>=lon.lim[1])&(lon<=lon.lim[2])
  # ind.lat <- (lat>=lat.lim[1])&(lat<=lat.lim[2])
  # 
  # cuales.lon <- which(ind.lon)
  # cuales.lat <- which(ind.lat)
  
  u <- 0.01*var.get.nc(ncId,'eastward_wind',
                                start=c(cuales.lon[1],cuales.lat[1],1,1),
                                count=c(length(cuales.lon),length(cuales.lat),1,1)) 
  v <- 0.01*var.get.nc(ncId,'northward_wind',
                                start=c(cuales.lon[1],cuales.lat[1],1,1),
                                count=c(length(cuales.lon),length(cuales.lat),1,1)) 
  
  
  close.nc(ncId)
  
  malla <- expand.grid(lon=lon[cuales.lon],lat=lat[cuales.lat])
  
  anomalia <- sqrt(u^2 + v^2) - mag.prom
  
  
  u <- u - u_p
  v <- v - v_p
  
  A <- stack( as.data.frame(anomalia) )
  Z <- stack( as.data.frame(sqrt(u^2+v^2)) )
  u <- stack( as.data.frame(u) )
  v <- stack( as.data.frame(v) )
  mapa <- data.frame( lon = as.matrix(malla$lon),
                      lat = as.matrix(malla$lat),
                      mag = as.matrix(A$values),
                        u = as.matrix((u$values)/Z$values),
                        v = as.matrix((v$values)/Z$values))
  
  quitar <- floor(seq(from=1,to=dim(u)[1],length.out = 1000)) 
  vectores <- mapa[quitar,]
  rm(list=c('anomalia','Z','u','v','A'))
  if (ii==1){
    promedioV <- vectores
    promedioC <- mapa 
  }else{
    promedioC$mag <- promedioC$mag + mapa$mag
    promedioC$u <- promedioC$u + mapa$u
    promedioC$v <- promedioC$v + mapa$v
    promedioV$mag <- promedioV$mag + vectores$mag
    promedioV$u <- promedioV$u + vectores$u
    promedioV$v <- promedioV$v + vectores$v
  }
  ###
  load('costa_Peru_202.RDat') 
  shore$long <- shore$long-360
  
  
  pp <- ggplot(data=mapa,aes(x=lon,y=lat,fill=mag))
  pp <- pp + geom_raster(interpolate=TRUE)
  
  pp <- pp + scale_fill_gradientn(colours = paleta_color,
                                   breaks = niveles,
                                   labels = as.character(niveles),
                                   limits = range(niveles))
 
   pp <- pp + stat_contour( data=mapa,aes(x=lon,y=lat,z=mag),
                            bins = 7,
                            col ='black'  )
 
    pp <- pp + geom_quiver(data=vectores,
                         aes(x=lon,y=lat,u=u,v=v),
                         vecsize = 2,
                         size = 1,
                         inherit.aes=FALSE)
    pp <- pp + geom_text_contour(  data = mapa,aes(x=lon,y=lat,z=mag),
                                   stroke = 0.15,skip=0,size=10,
                                   rotate = FALSE,col='white',
                                   stroke.color = 'black')
    
     pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),color = 'black',
                           fill = 'grey80',inherit.aes=FALSE  )
  for (kk in 1:length(fronteras)){
    f <- as.data.frame(fronteras[[kk]])
    pp <- pp + geom_point(data=f,aes( x=X1-360,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
  }
  pp <- pp + scale_x_continuous(limits=range(lon.lim),
                                expand=c(0,0),
                                breaks = marcas_x,
                                labels = etiquetas_x)
  pp <- pp + scale_y_continuous(limits=range(lat.lim),
                                expand=c(0,0),
                                breaks = marcas_y,
                                labels = etiquetas_y)
  pp <- pp + labs(x='Longitud',y='Latitud',
                  title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                  subtitle=paste0('Anomalía del campo de viento ',fecha.titulo ),
                  caption='Fuente: Remote Sensing Systems ASCAT C-2015, Daily Ocean Vector Winds on 0.25 deg grid, Version 02.1
                  \n Climatología 2000-2014')
  
  pp <- pp +  theme( axis.title.x = element_text( size=24,hjust=0.5  ),
                     axis.title.y = element_text( size=24,hjust=0.5  ),
                     axis.text = element_text(size=20),
                     title=element_text(size=26),
                     plot.subtitle=element_text(size=24),
                     plot.caption = element_text(size = 18,hjust = 0))
  pp <- pp + guides( fill = guide_colorbar(  barheight = unit(11, "cm"),
                                             barwidth = unit(1,'cm'),
                                             label.theme = element_text(size=20),
                                             title = 'm/s',
                                             title.theme = element_text(size=20)
  ))
  plot(pp)
}
dev.off()


system(paste0('D:/ffmpeg/bin/ffmpeg.exe -y -framerate 1/0.5 -i ',
              figuras,'mapa_anomalia_viento_%03d.png -r 10 ',figuras,'mapa_anomalia_viento.gif'))

promedioV$mag <- promedioV$mag/narchs
  promedioV$u <- promedioV$u/narchs
  promedioV$v <- promedioV$v/narchs

promedioC$mag <- promedioC$mag/narchs
  promedioC$u <- promedioC$u/narchs
  promedioC$v <- promedioC$v/narchs

png(file=paste0(figuras,'mapa_anomalia_viento_promedio.png'),
      width=1000,height=1200,type='windows')
pp <- ggplot(data=promedioC,aes(x=lon,y=lat,fill=mag))
pp <- pp + geom_raster(interpolate=TRUE)
pp <- pp + scale_fill_gradientn(colours = paleta_color,
                                breaks = niveles,
                                labels = as.character(niveles),
                                limits=range(niveles))
pp <- pp + geom_contour( data=promedioC ,aes(x=lon,y=lat,z=mag),
                         bins = 7,
                         col ='black'  )
pp <- pp + geom_quiver(data=promedioV,
                       aes(x=lon,y=lat,u=u,v=v),
                       vecsize = 2,
                       size = 1,
                       inherit.aes=FALSE)
# pp <- pp + geom_text_contour( data=mapa, aes(x=lon,y=lat,z=mag),
#                               stroke = 0.15,skip=0,size=10,
#                               rotate = FALSE,check_overlap = TRUE,col='white',
#                               stroke.color = 'black')
pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),color = 'black',
                         fill = 'grey80',inherit.aes=FALSE  )
for (kk in 1:length(fronteras)){
  f <- as.data.frame(fronteras[[kk]])
  pp <- pp + geom_point(data=f,aes( x=X1-360,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
}
pp <- pp + scale_x_continuous(limits=range(lon.lim),
                              expand=c(0,0),
                              breaks = marcas_x,
                              labels = etiquetas_x)
pp <- pp + scale_y_continuous(limits=range(lat.lim),
                              expand=c(0,0),
                              breaks = marcas_y,
                              labels = etiquetas_y)
pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle=paste0('Anomalía del campo de viento\n',tituloprom ),
                caption='Fuente: Remote Sensing Systems ASCAT C-2015, Daily Ocean Vector Winds on 0.25 deg grid, Version 02.1')

pp <- pp + theme( axis.title.x = element_text( size=24,hjust=0.5  ),
                  axis.title.y = element_text( size=24,hjust=0.5  ),
                  axis.text = element_text(size=20),
                  title=element_text(size=26),
                  plot.subtitle=element_text(size=24),
                  plot.caption = element_text(size = 18,hjust = 0))
pp <- pp + guides( fill = guide_colorbar(  barheight = unit(11, "cm"),
                                           barwidth = unit(1,'cm'),
                                           label.theme = element_text(size=20),
                                           title = 'm/s',
                                           title.theme = element_text(size=20)
))
plot(pp)
dev.off()