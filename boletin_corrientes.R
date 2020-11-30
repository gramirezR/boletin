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
library('RCurl')
library('RNetCDF')
library('raster')
library('ggquiver')

setwd('D:/programasR/boletin/')
source('herramientas.R')
lugar <- 'Ecuador'
raiz <- 'D:/boletin/datos/mercator/'
figuras <- paste0(raiz,'figuras_Callao/')


if(!dir.exists(figuras)){
  dir.create(figuras)
}

gshhs.dir <- 'D:/programasR/gshhg-bin-2.3.7/'
t0.copernico <- as.Date('1950-01-01') # segundos desde esta fecha
# los archivos miden el tiempo en horas a partir de t0.Copernico 
bajar <- FALSE

fecha.inicial <- as.Date('2008-01-01')
fecha.final <- as.Date('2018-12-01') # en días

if (bajar){
  lista.archivos.ftp <- ruta.archivos(inicio=fecha.inicial,final=fecha.final,ruta=ruta.ftp)
  bajar.archivos(lista.archivos.ftp,raiz)
}


##########################


lista.archivos.local <- list.files(path=raiz,full.names = TRUE,pattern = '.+nc$')

# lista.archivos.local <- lista.archivos.local[-seq(1,6)]

narchs <- length(lista.archivos.local)

##########################

ncConn <- open.nc(lista.archivos.local[1])
print.nc(ncConn)
close.nc(ncConn)

#############################

limite.lon <- c(-77.7,-76.7)
limite.lat <- c(-12.3,-11.8)

ncConn <- open.nc(lista.archivos.local[1])
longitud <- var.get.nc(ncConn,variable='longitude')
latitud <- var.get.nc(ncConn,variable='latitude')
indc.lon <- which( longitud>=limite.lon[1] & longitud<=limite.lon[2])
indc.lat <- which(  latitud>=limite.lat[1] &  latitud<=limite.lat[2])
primer.lon <- indc.lon[1]
cuantos.lon <- length(indc.lon)
primer.lat <- indc.lat[1]
cuantos.lat <- length(indc.lat)

inicio <- c(primer.lon,primer.lat,1,1)
cuantos <- c(cuantos.lon,cuantos.lat,10,1)

U <- promedio(ncConn,'uo',inicio,cuantos,factor.var=0.000610370188951492)
V <- promedio(ncConn,'vo',inicio,cuantos,factor.var=0.000610370188951492)
temporalU <- U*0
temporalV <- V*0
close.nc(ncConn)

longitud <- longitud[indc.lon]
latitud <- latitud[indc.lat]

mapa <- pasar.aRaster(U,longitud,latitud)

windows()
plot(mapa)
rm('mapa')
################################


U <- vector(mode='list',length=(12))
V <- U

for (jj in 1:12){
    kk <- 0
    temporalU <- temporalU*0
    temporalV <- temporalU*0
  for (ii in seq(from=jj,to=narchs,by=12)){
    tryCatch({  
      ncConn <- open.nc(lista.archivos.local[ii])
      
      temporalU <- temporalU + promedio(ncConn,'uo',inicio,cuantos,factor.var=1)
      
      temporalV <- temporalV + promedio(ncConn,'vo',inicio,cuantos,factor.var=1)
      # temporalU <- temporalU + pasar.aRaster(,longitud,latitud)
      # 
      # temporalV <- temporalV + pasar.aRaster(promedio(ncConn,'vo',inicio,cuantos,factor.var=0.000610370188951492),longitud,latitud)
      kk <- kk+1
      close.nc(ncConn)},error=function(e) print(ii)
    )
  }
  
  U[[jj]] <- pasar.aRaster(temporalU*0.000610370188951492/kk,longitud,latitud)
  V[[jj]] <- pasar.aRaster(temporalV*0.000610370188951492/kk,longitud,latitud)
}
################################

# U.promedio <- vector(mode='list',length=12)
# V.promedio <- U.promedio
# 
# for (ii in seq(1,12)){
#   U.promedio[[ii]] <- promedio.mes(U,ii)
#   V.promedio[[ii]] <- promedio.mes(V,ii)
# }

# U.promedio <- do.call(what=stack,args=U.promedio)
# V.promedio <- do.call(what=stack,args=V.promedio)

# U.promedio <- do.call(what=stack,args=U)
# V.promedio <- do.call(what=stack,args=V)
# 
# names(U.promedio) <- paste0('u',names(U.promedio))
# names(V.promedio) <- paste0('v',names(V.promedio))
# 
 velocidad <- stack( c(U,V) )


########
campo.U <- lapply(U,FUN=function(x){
  if (!is.null(x)){
    raster2ggplot(x)}
}) 

for (ii in  1:12){
  names(campo.U[[ii]]) <- c('lat','lon','U')
}

campo.V <- lapply(V,FUN=function(x){
  if (!is.null(x)){
    raster2ggplot(x)}
})  

for (ii in  1:12){
  names(campo.V[[ii]]) <- c('lat','lon','V')
}

campo <- vector(mode='list',length=12)

for (ii in  1:12){
  
        mag <- sqrt(campo.U[[ii]]$U^2 + campo.V[[ii]]$V^2 )
         uu <-campo.U[[ii]]$U 
          u <- uu/mag
        
        vv <- campo.V[[ii]]$V
         v <- vv/mag
        
        campo[[ii]] <- data.frame(lat = campo.U[[ii]]$lat,
                                   lon = campo.V[[ii]]$lon,
                                     U = uu,
                                     V = vv,
                                  Unor = u,
                                  Vnor = v,
                                     Z = mag
                                   )
}


################
graphics.off()
cuales <- seq(from=1,to=length(campo[[1]]$lon),by=1)
# paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')

#load('D:/programasR/boletin/costa_ecuador.RData') 
#load('peru_costa_200mi.RData')
load('costa_05-20.RData')

if (exists('shore')){

poligono <- shore@polygons[[1]]@Polygons[[1]]@coords

poligono <- data.frame(lon=poligono[,1],lat=poligono[,2])
}else{
  poligono <- costa@polygons[[1]]@Polygons[[1]]@coords
  
  poligono <- data.frame(lon=poligono[,1],lat=poligono[,2])
}
# shore$long <- shore$long - 360
mes.nombre <- lubridate::month(as.Date(paste('2020',sprintf(1:12,fmt='%02d'),'01',sep='-')),
                               label=TRUE,abbr=FALSE)

#####################
cMin <- 0
cMax <- 1
graphics.off()
paleta_color <- cptcity::cpt('cmocean_speed')
for (ii in 1:12){
png(width=1200,height=1000,file=paste0(figuras,'corrientes',sprintf(ii,fmt='%02d'),'.png'))
pp <- ggplot(data=campo[[ii]],aes(x=lon,y=lat,fill=Z))
pp <- pp + geom_raster(interpolate=TRUE,show.legend = TRUE  )
pp <- pp + scale_fill_gradientn( colours = paleta_color,limits=c(cMin,cMax))
pp <- pp + geom_quiver(data=campo[[ii]][cuales,],aes(x=lon[cuales],y=lat[cuales],
                           u=Unor[cuales],v=Vnor[cuales]),
                       vecsize = 0.5,
                       inherit.aes = FALSE)
if (exists('shore')){
pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),
                         color = 'black', fill = 'grey80',inherit.aes=FALSE  )
}else{
  pp <- pp + geom_polygon( data=costa,aes(x=long,y=lat,group=group),
                           color = 'black', fill = 'grey80',inherit.aes=FALSE  )
}
# pp <- pp + geom_path( data=poligono,aes(x=lon-360,y=lat),
#                          color = 'red',size=1,inherit.aes=FALSE  )

for (kk in 1:length(fronteras)){
  f <- as.data.frame(fronteras[[kk]])
  
  pp <- pp + geom_point(data=f,aes( x=X1-360,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
}
 pp <- pp + scale_x_continuous(expand = c(0.0,0))
 pp <- pp + scale_y_continuous(expand = c(0,0))

 pp <- pp + coord_cartesian(xlim = limite.lon,ylim = limite.lat)

pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('DIRECCIÓN DE HIDROGRAFÍA\n Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = paste0('Campo de corrientes de ',mes.nombre[ii]),
                caption = 'Fuente: COPERNICUS MARINE ENVIRONMENT\n MONITORING SERVICE (CMEMS v3.0).\nClimatología: 2000-2018')

pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                                   axis.title.y = element_text( size=28,hjust=0.5  ),
                                   axis.text = element_text(size=28,colour = 'black'),
                                   title=element_text(size=28),
                                   plot.subtitle=element_text(size=24),
                                   plot.caption = element_text(size = 22,hjust = 0))
pp <- pp + guides( fill = guide_colorbar(  barheight = unit(22, "cm"),
                                           barwidth = unit(1.5,'cm'),
                                           label.theme = element_text(size=26),
                                           title = 'magnitud (m/s)',
                                           title.theme = element_text(size=26)))

plot(pp)
dev.off()
}

# rm(list=c('pp','campo','U.promedio','V.promedio'))
# rm(list=c('U','V'))
########
load('peru_costa_200mi.RData')
costa@polygons[[1]]@Polygons[[1]]@coords[,1] <- costa@polygons[[1]]@Polygons[[1]]@coords[,1] - 360

costa@bbox[1] <- costa@bbox[1] - 360
costa@bbox[3] <- costa@bbox[3] - 360

valor.poligono <- raster::extract(velocidad,costa)

ndatos <- length(valor.poligono[[1]][,1])

################

 magnitud <- matrix(nrow = ndatos,ncol = 12)
direccion <- magnitud

for (ii in 1:12){
   magnitud[,ii] <- sqrt(valor.poligono[[1]][,ii]^2 + valor.poligono[[1]][,ii+12]^2)
  direccion[,ii] <- 180*atan2(valor.poligono[[1]][,ii+12],valor.poligono[[1]][,ii])/pi
   cual.negativo <- which(direccion[,ii]<0)
  direccion[cual.negativo,ii] <- direccion[cual.negativo,ii] + 360
}

#######


rosa.corriente <- vector(mode='list',length=12)

for (ii in 1:12){
  rosa.corriente[[ii]] <- data.frame(direccion=direccion[,ii],magnitud=magnitud[,ii])
}

#########
graphics.off()
sistema.meteorologia <- FALSE
y_pos <- seq(0,20,by = 2)

meses <- months(x = seq(from=as.Date('2020-01-01'),to=as.Date('2020-12-01'),by='month'))


for (ii in 1:12){
  indcNa <- which(is.na(rosa.corriente[[ii]]$direccion)|is.na(rosa.corriente[[ii]]$magnitud)|
                    rosa.corriente[[ii]]$direccion=='NaN'|rosa.corriente[[ii]]$magnitud=='NaN') 
  if (length(indcNa)>0){  
  rosa.corriente[[ii]] <- rosa.corriente[[ii]][-indcNa,]
  }
  
  indc <- which(rosa.corriente[[ii]]$direccion > 360 - 22.5/2 )
  
  if(length(indc)>0){
    rosa.corriente[[ii]]$direccion[indc] <- rosa.corriente[[ii]]$direccion[indc] - 360
    rosa.corriente[[ii]]$dir <- base::cut(rosa.corriente[[ii]]$direccion,breaks=seq(from=- 22.5/2,to=360 - 22.5/2,by=22.5))
  }else{
    rosa.corriente[[ii]]$dir <- base::cut(rosa.corriente[[ii]]$direccion,breaks=seq(from=- 22.5/2,to=360 - 22.5/2,by=22.5))
  }
  
  rosa.corriente[[ii]]$mag <- base::cut(rosa.corriente[[ii]]$magnitud*100,
  breaks=seq(from=0,to=40,by=5))
}

 paleta_color <- cptcity::cpt(pal='kst_35_blue_waves',n=16)

for(ii in 1:12){
png(file=paste0(figuras,'rosa_corrientes_',sprintf(ii,fmt='%02d'),'.png'),
    width=650,height=650)
pp <- ggplot(data=rosa.corriente[[ii]],aes(x=dir,y=100*..count../sum(..count..),fill=mag) )
pp <- pp + geom_bar(position='stack',color='black',na.rm=TRUE)

# pp <- pp + scale_fill_manual(name='magntud(cm/s)',drop=FALSE,
#                              values=c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2'),
#                              guid =guide_colorbar())

pp <- pp + scale_fill_manual(values = rev(paleta_color),name='magntud\n(cm/s)')



pp <- pp + scale_x_discrete(drop=FALSE,
                            labels = c("E", "ENE", "NE", "NNE",
                                       "N", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO",
                                       "S", "SSE", "SE", "ESE"))
pp <- pp + scale_y_continuous(limits = c(-5,30),breaks = seq(from=0,to=30,by=10))

pp <- pp + coord_polar(theta='x',start = -0.5*pi+(-22.5/2)*pi/180,direction = -1)

pp <- pp + labs(x='',y='',title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                                       'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = paste0('Rosa de corrientes de ',mes.nombre[ii]),
                caption = 'Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 2000-2018'
)

pp <- pp + annotate("text",
        x = 5*c(1, 1, 1),
        y = c(10, 20, 30),
        label = c('10%','20%','30%'),
        family = "", fontface = 3, size=6)

# ,
# draw.ulim = TRUE,
# draw.llim = TRUE,
# ticks=TRUE,
# ticks.colour = 'black',
# barheight = unit(10, "cm"),
# barwidth = unit(1.5,'cm'),
# label.theme=element_text(size=14),
# title.theme=element_text(size=20,angle = 90),
# title.hjust = 0.35,
# title.position = 'right',nbin=11)

pp <- pp + theme( axis.text.x = element_text( size=16,face = 'bold' ),
                  axis.title.x=element_blank(),
                  axis.title.y = element_blank(   ),
                  axis.text.y = element_blank(),
                  title=element_text(size=20),
                  plot.subtitle=element_text(size=14),
                  plot.caption = element_text(size = 12,hjust = 0),
                  legend.position = 'right')
pp <- pp + labs(title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle=paste0('Corrientes para ',lugar,': ',meses[ii]),
                caption=paste0('Fuente: NOAA\n
ENVIRONMENTAL MODELLING CENTER\n
ftp://polar.ncep.noaa.gov/pub/history/waves/multi_1/'))


plot(pp)
dev.off()}

##########
for(ii in 1:12){
png(file=paste0(figuras,'ocurrencia_corrientes_',sprintf(ii,fmt='%02d'),'.png'),
    width=1000,height=1000)
pp <- ggplot(data=rosa.corriente[[ii]],aes(x=direccion ))
pp <- pp + stat_ecdf(color='blue',size=2)
pp <- pp + scale_y_continuous(breaks = seq(from=0,to=1,by=0.1),
                              labels=seq(from=0,to=1,by=0.1)*100)
pp <- pp + scale_x_continuous(breaks=seq(0,360,length.out = 17),
                              labels=etqts <- c('N','NNE','NE','ENE','E','ESE','SE','SSE','S',
                                                'SSO','SO','OSO','O','ONO','NO','NNO','N'))
pp <- pp + labs(x='Dirección',y='% Ocurrencia',title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                                       'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = paste0('Diagrama de ocurrencia del mes de ',mes.nombre[ii]),
                caption = 'Fuente: COPERNICUS MARINE ENVIRONMENT\n MONITORING SERVICE (CMEMS v3.0).\nClimatología: 2000-2018'
)
pp <- pp + theme(axis.text.x = element_text(size=20,face='bold'),
                 axis.text.y = element_text(size=20,face='bold'),
                 axis.title.x = element_text( size=28,hjust=0.5  ),
                 axis.title.y = element_text( size=28,hjust=0.5  ),
                 title=element_text(size=28),
                 plot.subtitle=element_text(size=24),
                 plot.caption = element_text(size = 22,hjust = 0))
plot(pp)
dev.off()
}

########################
for(ii in 1:12){
  png(file=paste0(figuras,'ocurrencia_corrientes_magnitud',sprintf(ii,fmt='%02d'),'.png'),width=1000,height=1000)
  pp <- ggplot(data=rosa.corriente[[ii]],aes(x=magnitud ))
  pp <- pp + stat_ecdf(color='blue',size=2)
  pp <- pp + scale_y_continuous(breaks = seq(from=0,to=1,by=0.1),
                                labels=seq(from=0,to=1,by=0.1)*100)
  pp <- pp + scale_x_continuous(breaks=seq(from=0,to=0.2,by=0.02),limits = c(0,0.2))
  pp <- pp + labs(x='Magnitud (m/s)',y='% Ocurrencia',title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                                                              'Dpto. de Oceanografía - Div. Oceanografía'),
                  subtitle = paste0('Diagrama de ocurrencia del mes de ',mes.nombre[ii]),
                  caption = 'Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 2000-2018'
  )
  pp <- pp + theme(axis.text.x = element_text(size=20,face='bold'),
                   axis.text.y = element_text(size=20,face='bold'),
                   axis.title.x = element_text( size=28,hjust=0.5  ),
                   axis.title.y = element_text( size=28,hjust=0.5  ),
                   title=element_text(size=28),
                   plot.subtitle=element_text(size=24),
                   plot.caption = element_text(size = 22,hjust = 0))
  plot(pp)
  dev.off()
}
