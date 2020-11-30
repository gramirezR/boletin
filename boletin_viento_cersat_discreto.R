### PROCESAR DATOS DE VIENTO CERSAT #####
cat('\014') 
graphics.off()
rm(list=ls())

library('ncdf4')
library('stringr')
library('tidyverse')
library('scales')
library('directlabels')
library('maptools')
library('reshape2')
library('ggquiver')
library('RCurl')
library(cptcity)
####
setwd('D:/programasR/boletin/')
# carpeta <- 'D:/boletin/viento/2018/09/'
#fecha.actual <- lubridate::today()-2;
#fecha.actual <- as.Date('2019-03-31')

#hoy <- lubridate::today() 
fecha.inicial <- lubridate::today()
dia <- lubridate::day(fecha.inicial)
dias.atras <- dia-1
fecha.anterior <- fecha.inicial-dias.atras

secuencia <- seq(from=fecha.anterior,to=fecha.inicial,by='month')

carpeta <-lapply(unique(format(secuencia,format='%Y/%m')),
                 function(x) paste0('D:/boletin/viento/',x) )

figuras <- lapply(unique(format(secuencia,format='%Y')), 
                  function(x) paste0('D:/boletin/viento/',x,'/figuras/'))


datos <- lapply(unique(format(secuencia,format='%Y')), 
                function(x)paste0('D:/boletin/viento/',x,'/datos/'))


lapply(carpeta ,function(x){
  if (!dir.exists(x)){
    dir.create(x,recursive = TRUE)
  } 
}  )

# if (!dir.exists(carpeta)){
#   dir.create(carpeta,recursive = TRUE)
# }

lapply(datos ,function(x){
  if (!dir.exists(x)){
    dir.create(x,recursive = TRUE)
  } 
}  )

# if (!dir.exists(datos)){
#   dir.create(datos,recursive = TRUE)
# }



# ftp.dir <- paste0('ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/WIND_GLO_WIND_L3_NRT_OBSERVATIONS_012_002/KNMI-GLO-WIND_L3-OBS_SCATSAT-1_OSCAT_50_DES_V2/',anio,'/',mes,'/')
ftp.dir <- lapply(unique(format(secuencia,format='%Y/%m/')),
                  function(x)paste0('ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/WIND_GLO_WIND_L4_NRT_OBSERVATIONS_012_004/CERSAT-GLO-BLENDED_WIND_L4-V6-OBS_FULL_TIME_SERIE/',x))
escala <- c(.2,1.5,1.0,0.02)

# ftp://nrt.cmems-du.eu/Core/WIND_GLO_WIND_L4_NRT_OBSERVATIONS_012_004/CERSAT-GLO-BLENDED_WIND_L4-V5-OBS_FULL_TIME_SERIE/2019/03/2019032412_6hm-ifremer-L4-EWSB-wind_blended-GLO-20190326124127NRT-02.0.nc

lista <- unlist(lapply(ftp.dir,
                       function(x) getURL(x,dirlistonly = TRUE,verbose=TRUE)))

lista <- unlist(str_extract_all(unlist(strsplit(lista,'\r\n')),'.+(.nc)'))
partes <- unlist(strsplit(as.character(fecha.inicial),'-',fixed=TRUE))

indc.arch <-  lapply(format(secuencia,format='%Y%m'),
                     function(x) grep(pattern = paste0('^',x,'[0-9]{2}'),x=lista))

# bajar <- paste0('GLO-WIND_L3-OBS_SCATSAT-1_OSCAT_50_DES_',anio,mes,dia,'.nc')

#bajar <- lista[indc.arch]#'2019032412_6hm-ifremer-L4-EWSB-wind_blended-GLO-20190326124127NRT-02.0.nc'
bajar <- vector(mode='list',length = length(indc.arch))

for (contador in 1:length(indc.arch)){
  bajar[[contador]] <- paste0(ftp.dir[[contador]],lista[indc.arch[[contador]]])
}

bajar <- unlist(bajar)

lapply( bajar,function (x){
  destino <- paste0(datos,substring(x,first=154,last=229))
  if (!file.exists(destino)){
    download.file( url= x, destfile= destino,
                   method = "auto",quiet = FALSE, mode="wb", cacheOK = TRUE  )
    closeAllConnections()}
})
#######################

destino <- list.files(path = unlist(datos),full.names = TRUE )

lon <- seq(from=-180,to=179.75,by=0.25)#var.get.nc(ncId,'lon')

indc <- lon<0

lon[indc] <- lon[indc]+360

lat <- seq(from=-80,to=80,by=0.25)#var.get.nc(ncId,'lat')

########################

lon.lim <- c(-92,-69) + 360
lat.lim <- c(-22,3)

indc.lon <- which((lon >=lon.lim[1]) & ( lon<=lon.lim[2] ))
indc.lat <- which((lat >=lat.lim[1]) & ( lat<=lat.lim[2] ))

u <- matrix(0.0,nrow=length(lon),ncol = length(lat))
v <- u


lista.fechas <- format(seq.Date(from=fecha.anterior,to=fecha.inicial,by='day'),format='%Y%m%d')
##############################################333
narchs <- 0
falla <- 0

for (ii in 1:length(lista.fechas)){
  anio <- substring(text = lista.fechas[ii],first = 1,last = 4)
  lista.dia <- list.files(path = paste0('D:/boletin/viento/',anio,'/datos/'),
                          pattern = paste0('^',lista.fechas[ii],'.+.nc$'),full.names = TRUE)
  if (length(lista.dia)>0){
    narchs <- narchs + length(lista.dia)
    kk <- 1
    for (jj in 1:length(lista.dia)){ 
      if ( kk<length(lista.dia)){
        tryCatch({
          ncId <- nc_open(lista.dia[jj])
          u <- ncvar_get(ncId,'eastward_wind') + u 
          v <- ncvar_get(ncId,'northward_wind') + v
          nc_close(ncId)},
          error=function(e) {e
            falla <- falla+1 })
        kk <- kk + 1
      }else{
        break
      }
    }}}
#####################################
nbuenos <- narchs - falla

u <- u/nbuenos
v <- v/nbuenos

u <- u[indc.lon,indc.lat]

dejar.lon <- seq(from=1,to=length(indc.lon),by = 3)
dejar.lat <- seq(from=1,to=length(indc.lat),by = 3)

u <- u[dejar.lon ,dejar.lat ]
u <- stack(as.data.frame(u))$values

v <- v[indc.lon,indc.lat]
v <- v[dejar.lon ,dejar.lat ]
v <- stack(as.data.frame(v))$values

magnitud <- sqrt(u^2 + v^2)        
magnitud <- stack( as.data.frame(magnitud)  )$values


lon <- lon[indc.lon]
lat <- lat[indc.lat]

lon <- lon[ dejar.lon ]
lat <- lat[ dejar.lat ]


malla <- expand.grid(lon,lat)
vientos <- data.frame( lon = malla$Var1, 
                       lat = malla$Var2,
                       mag = magnitud,
                       u = u,
                       v = v)
#### Suavizado #########

modelo.m<- mgcv::gam(mag~te(lon,lat,k=c(20,20)),data=vientos)


malla.i <- expand.grid( seq(from=270,to=290,length.out = 100),
                        seq(from=-20,to=2,length.out = 100))

names(malla.i) <- c('lon','lat')
mag.i <- predict(modelo.m,newdata=malla.i)


vientos.i <- data.frame(lon=malla.i$lon,lat=malla.i$lat,mag=mag.i)
########################################
vientos.i$prb <- cut(vientos.i$mag,breaks = seq(from=0,to=13,by=1),
                     labels = seq(from=0,to=12,by=1))
indcNa <- which(is.na(vientos.i$prb))
vientos.i$prb[indcNa] <- 0
#####################

load('costa_Peru_202.RDat')

# paleta_color <- c('#f0f0f0','#b4b4b4','#7878f7','#3c3cfb','#6000ff','#0040ff',
#                   '#0080ff','#00bfff','#00ffff','#2bffd4','#55ffaa','#80ff80',
#                   '#aaff55','#d4ff2b','#ffff00','#ffd400','#ffaa00','#ff8000',
#                   '#ff5500','#ff2b00','#ff0000','#d40000','#aa0000','#800000')
#############################
paleta_color <- cpt(pal=find_cpt('dv_wind_speed'),n=13)
# paleta_color <- cpt(pal=find_cpt('dv_wind_speed'),n=13)
niveles <- seq(from=0,to=10,by=2)
if(exists('pp')) rm(list='pp')

png(file=paste0(figuras,'mapa_viento_', fecha.inicial,  '_D.png'),width=1000,height=1200)
pp <- ggplot(data=vientos.i,aes(x=lon,y=lat,fill=prb))
pp <- pp + geom_raster(interpolate=TRUE)

pp <- pp + scale_fill_manual( values=paleta_color,drop=FALSE)

pp <- pp + geom_quiver(data=vientos,aes(x=lon,y=lat,u=u,v=v),
                       inherit.aes = FALSE,na.rm=TRUE,
                       vecsize=1,size=1)

pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),
                         color = 'black', fill = 'grey80',inherit.aes=FALSE )

for (kk in 1:length(fronteras)){
  f <- as.data.frame(fronteras[[kk]])
  pp <- pp + geom_point(data=f,aes( x=X1,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
}

pp <- pp + scale_x_continuous(limits = c(270,290),
                              expand = c(0,0))
pp <- pp + scale_y_continuous(limits = c(-20,2),
                              expand = c(0,0))
#pp <- pp + coord_cartesian(xlim=c(282,283),ylim=c(-13,-10))
pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía\n',
                             'Campo de Viento\n','Del ',format(fecha.inicial-dias.atras,format='%d %B del %Y'),' al ',
                             format(fecha.inicial,format='%d %B del %Y')),
                caption='Fuente: IFREMER CERSAT Global Blended Mean Wind Fields on 25km X 25km grid')
pp <- pp + theme_bw()
pp <- pp + theme( axis.title.x = element_text( size = 28,hjust=0.5  ),
                  axis.title.y = element_text( size = 28,hjust=0.5  ),
                  axis.text = element_text( size = 20),
                  title = element_text( size = 32),
                  plot.caption = element_text( size = 18,hjust = 0))

pp <- pp + guides( fill=guide_legend(reverse=TRUE,title.position = 'top',
                                     keywidth = 4,keyheight = 2,
                                     title.theme = element_text(size = 30,hjust = 0.5),
                                     label.theme = element_text(size=26),
                                     title = 'Magnitud\nm/s') )

plot(pp)
dev.off()

##################### ANOMALIA #####################
load(file = 'viento_climat_2000-2014.RData') 
#climatologiaMes <- readRDS('ClimatologiaMesViento.RDS')
# VARIABLES u_p, v_p, lon_p, lat_p, cuales.lat, cuales.lon
#mes.numero <- lubridate::month(fecha.inicial)

#clima <- climatologiaMes[[mes.numero]]

malla <- expand.grid( lon_p,lat_p )
names(malla) <- c('lon','lat')
u <- stack(as.data.frame(u_p))$values
v <- stack(as.data.frame(v_p))$values

promedio <- data.frame( u=u,v=v,lon=malla$lon,lat=malla$lat )
#names(promedio) <- c('u','v','lon','lat')
modelo <- mgcv::gam(data=promedio,u~te(lon,lat,k=c(10,10)))

promedio.u <- predict(modelo,newdata = data.frame(lon=vientos$lon,lat=vientos$lat))

modelo <- mgcv::gam(data=promedio,v~te(lon,lat,k=c(10,10)))

promedio.v <- predict(modelo,newdata = data.frame(lon=vientos$lon,lat=vientos$lat))

promedio.mag <- sqrt(promedio.u^2 + promedio.v^2)

anomalia <- data.frame( lon = vientos$lon,
                        lat = vientos$lat,
                   magnitud = vientos$mag - promedio.mag,
                          u = vientos$u - promedio.u,
                          v = vientos$v - promedio.v)

##############################
paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred',13)
indcM <- which(promedio.mag>14)
promedio.mag[indcM] <- NA
promedio <- data.frame(lon=vientos$lon,lat=vientos$lat,mag=promedio.mag)
if(exists('pp')) rm(pp)
windows(width = 850,height = 850)
pp <- ggplot(data=promedio,aes(x=lon,y=lat,fill=mag))
pp <- pp + geom_raster(interpolate=TRUE)
pp <- pp + scale_fill_gradientn(colours = paleta_color)
plot(pp)
#### Suavizado #########

modelo.a<- mgcv::gam(magnitud~te(lon,lat,k=c(25,25)),data=anomalia)

mag.a <- predict(modelo.a,newdata=malla.i)

anomalia.i <- data.frame(lon=malla.i$lon,lat=malla.i$lat,mag=mag.a)
indcNa <- which(is.na(anomalia$magnitud))
anomalia <- anomalia[-indcNa,]
################################
anomalia.i$prb <- cut(anomalia.i$mag,breaks = seq(from=-6,to=7,by=1),
                      labels = seq(from=-6,to=6,by=1))
indcNa <- which(is.na(anomalia.i$prb))
anomalia.i$prb[indcNa] <- 0
paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred',13)
###############################

niveles <- seq(from=-6,to=6,by=1)
if(exists('pp')) rm(pp)
png(file=paste0(figuras,'mapa_an_viento_', fecha.inicial,  '_D.png'),width=1000,height=1200)
pp <- ggplot(data=anomalia.i,aes(x=lon,y=lat,fill=prb))
pp <- pp + geom_raster(interpolate=TRUE)
pp <- pp + scale_fill_manual( values=paleta_color,drop=FALSE)

pp <- pp + geom_quiver(data=anomalia,
                       aes(x=lon,y=lat,u=u,v=v),
                       inherit.aes=FALSE,na.rm=TRUE,
                       vecsize=10,size=1)

pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),
                         color = 'black', fill = 'grey80',inherit.aes=FALSE  )

for (kk in 1:length(fronteras)){
  f <- as.data.frame(fronteras[[kk]])
  pp <- pp + geom_point(data=f,aes( x=X1,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
}

pp <- pp + scale_x_continuous(limits = c(270,290),
                              expand = c(0,0))
pp <- pp + scale_y_continuous(limits = c(-20,2),
                              expand = c(0,0))

pp <- pp + labs(    x ='Longitud',y='Latitud',
                title = paste0(
                             'DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía\n',
                             'Anomalía del Campo de Viento\nDel ',
                             format(fecha.inicial-dias.atras,format='%d %B del %Y'),
                             ' al ',  
                             format(fecha.inicial,format='%d %B del %Y')
                             ),
                caption = 'Fuente: IFREMER CERSAT Global Blended Mean Wind Fields on 25km X 25km grid
                Climatología: QUIKSCAT-ASCAT 2000-2014' )
pp <- pp + theme( 
                  axis.title.x = element_text( size=28,hjust=0.5  ),
                  axis.title.y = element_text( size=28,hjust=0.5  ),
                     axis.text = element_text(size=20),
                         title = element_text(size=32),
                  plot.caption = element_text(size = 18,hjust = 0)
                  )
pp <- pp + guides( 
                   fill = guide_legend(        reverse = TRUE,
                                        title.position = 'top',
                                              keywidth = 4,
                                             keyheight = 2,
                                           title.theme = element_text(size = 30,hjust = 0.5),
                                           label.theme = element_text(size=26),
                                                 title = 'Magnitud\nm/s'
                                        )
                   )

plot(pp)
dev.off()
