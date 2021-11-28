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
####S
setwd('E:/programasR/boletin/')
# carpeta <- 'D:/boletin/viento/2018/09/'
#fecha.actual <- lubridate::today()-2;
#fecha.actual <- 

#hoy <- lubridate::today() 
fecha.inicial <- as.Date('2021-11-01')
fecha.final <- lubridate::today() - 2

# fecha.final <- lubridate::today() - 1
dia <- lubridate::day(fecha.final) 
# dias.atras <- as.numeric(fecha.final - as.Date('2021-02-01'))
# 
# fecha.inicial <- fecha.final - dias.atras# - dias.atras

periodo <- 'semanal'

if (periodo == 'semanal'){
  dias <- seq(from=fecha.inicial,to=fecha.final,by='7 days')
  if (as.numeric(fecha.final-fecha.inicial)%%7>0){
    secuencia <- c(dias, fecha.final)
  }
}else{                                                            
  intervalo <- paste(as.numeric(fecha.final-fecha.inicial)-1,'days')
  secuencia <- seq(from=fecha.inicial,to=fecha.final,by=intervalo)
}

#intervalo <- paste(as.numeric(fecha.final-fecha.inicial)-1,'days')
#secuencia <- seq(from=fecha.inicial, to=fecha.final,by=intervalo)

carpeta <-lapply(unique(format(secuencia,format='%Y/%m')),
                 function(x) paste0('E:/boletin/viento/',lubridate::year(fecha.inicial),'/datos/') )
# ,lubridate::year(fecha.actual),'/datos/'
figuras <- lapply(unique(format(secuencia,format='%Y')), 
                  function(x) paste0('E:/boletin/viento/',x,'/figuras/'))


datos <- lapply(unique(format(secuencia,format='%Y')), 
                function(x)paste0('E:/boletin/viento/',x,'/datos/'))


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
ftp.dir <- lapply( unique(format(secuencia,format='%Y/%m/') ),
                  function(x)paste0('ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/WIND_GLO_WIND_L4_NRT_OBSERVATIONS_012_004/CERSAT-GLO-BLENDED_WIND_L4-V6-OBS_FULL_TIME_SERIE/',x))
escala <- c(.2,1.5,1.0,0.02)

# ftp://nrt.cmems-du.eu/Core/WIND_GLO_WIND_L4_NRT_OBSERVATIONS_012_004/CERSAT-GLO-BLENDED_WIND_L4-V5-OBS_FULL_TIME_SERIE/2019/03/2019032412_6hm-ifremer-L4-EWSB-wind_blended-GLO-20190326124127NRT-02.0.nc

lista <- lapply(ftp.dir,function(x) {
    y <- getURL(x,dirlistonly = TRUE,verbose=TRUE)
    strsplit(y,'\r\n')
    
    } )

lista <- lapply(X = lista,FUN=unlist)


arch.Viento <- mapply( FUN=function(x,y){
    ar <-paste(x,y,sep='/')
   },x= carpeta,y=lista)


indice <- lapply(arch.Viento,FUN=function(x){
  file.exists(x)
})


bajar <- mapply(function(x,y,z){
         cual <- which(!y)
         este <- paste0(x,basename(z[cual]))
},x=ftp.dir,y=indice,z=arch.Viento)

bajar <- unlist(bajar)

destino <- mapply(FUN=function(x,y){
    if(!y){
      x
    }else{
      NULL
    }
      
},x=arch.Viento,y=indice)

 destino <- unlist(destino)
 
if (length(destino>0)){
  mapply(FUN = function(a,b){
    info <- url.exists(url, header=TRUE)
    tamanio <- as.numeric(info['Content-Length']) 
    print(tamanio)
    if(!file.exists(b) ){
    download.file( url= a, destfile= b,
                   method = "wininet",quiet = FALSE, mode="wb", cacheOK = TRUE  )
    closeAllConnections()}
    },
     a=bajar,b=destino
       )}
#######################

#destino <- list.files(path = unlist(datos),full.names = TRUE )

lon <- seq(from=-180,to=179.75,by=0.25)#var.get.nc(ncId,'lon')

indc <- lon<0

lon[indc] <- lon[indc]+360

lat <- seq(from=-80,to=80,by=0.25)#var.get.nc(ncId,'lat')

########################

lon.lim <- c(-92,-69) + 360
lat.lim <- c(-22,3)

indc.lon <- which((lon >=lon.lim[1]) & ( lon<=lon.lim[2] ))
indc.lat <- which((lat >=lat.lim[1]) & ( lat<=lat.lim[2] ))


##############################################


nSecuencia <- length(secuencia) 

lista.fechas <- vector(mode='list',length = nSecuencia - 1 )

for (ii in 1:(nSecuencia-1)){
  lista.fechas[[ii]] <- format(seq.Date(from=secuencia[ii],
                                          to=secuencia[ii+1]-1,by='day'),format='%Y%m%d')
}

arch.Viento <- unlist(arch.Viento)

lista.ArchsAnalisis <- lapply(X=lista.fechas,FUN=function(x){
  resultado <-    lapply(x,FUN=function(y) {
       str_match_all(string = arch.Viento,pattern = paste0('.+',y,'.+\\.nc$')) 
     } )
  unlist(resultado)
})
###################################################

for (ii in 1:length(lista.ArchsAnalisis) ) {
  narchs <- 0
  .GlobalEnv$falla <- 0
  u <- matrix(0.0,nrow=1440,ncol = 641)
  v <- u
  lista.dia <- lista.ArchsAnalisis[[ii]]
     narchs <- narchs + length(lista.dia)
     kk <- 1
    for (jj in 1:length(lista.dia)){
       if ( kk<length(lista.dia)){
         tryCatch({
      ncId <- nc_open(lista.dia[jj])
      u <- ncvar_get(ncId,'eastward_wind') + u
      v <- ncvar_get(ncId,'northward_wind') + v
      lon <- ncvar_get(ncId,'lon') + 360
      lat <- ncvar_get(ncId,'lat') 
      nc_close(ncId)},
      error=function(e) {e
        .GlobalEnv$falla <- .GlobalEnv$falla + 1
        file.remove(lista.dia[jj])})
         kk <- kk + 1
       }else{
         break
       }
    }
  
#####################################
nbuenos <- narchs - .GlobalEnv$falla

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

load('costa_Peru_202.RDat')

paleta_color <- c('#f0f0f0','#b4b4b4','#7878f7','#3c3cfb','#6000ff','#0040ff',
                  '#0080ff','#00bfff','#00ffff','#2bffd4','#55ffaa','#80ff80',
                  '#aaff55','#d4ff2b','#ffff00','#ffd400','#ffaa00','#ff8000',
                  '#ff5500','#ff2b00','#ff0000','#d40000','#aa0000','#800000')

marcas_x <- seq(270,290,by=10)

#############################
niveles <- seq(from=0,to=14,by=2)
if(exists('pp')) rm(list='pp')

fecha.inicial <- as.Date(lista.fechas[[ii]][1],format = '%Y%m%d')
fecha.final <- as.Date(lista.fechas[[ii]][length(lista.fechas[[ii]])],format = '%Y%m%d')

png(file=paste0(figuras,'mapa_viento_',periodo,'_',fecha.inicial,'_',fecha.final,  '.png'),width=1000,height=1200)
pp <- ggplot(data=vientos.i,aes(x=lon,y=lat,fill=mag))
pp <- pp + geom_raster(data=vientos.i,aes(x=lon,y=lat,fill=mag),interpolate=TRUE)

pp <- pp + scale_fill_gradientn(colours = paleta_color,
                                breaks = niveles,
                                labels = as.character(niveles),
                                limits = range(niveles))

pp <- pp + geom_quiver(aes(x=lon,y=lat,u=u,v=v),
                       vecsize = 2,
                       size = 0.5,data=vientos,inherit.aes = FALSE)

pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),
                         color = 'black', fill = 'grey80',inherit.aes=FALSE )

for (kk in 1:length(fronteras)){
  f <- as.data.frame(fronteras[[kk]])
  pp <- pp + geom_point(data=f,aes( x=X1,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
}

pp <- pp + scale_x_continuous(limits = c(270,290),
                              expand = c(0,0),labels = (marcas_x-360),breaks = marcas_x)
pp <- pp + scale_y_continuous(limits = c(-20,2),
                              expand = c(0,0))
#pp <- pp + coord_cartesian(xlim=c(282,283),ylim=c(-13,-10))
pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = paste0('Campo de Viento\n','Del ',format(fecha.inicial,format='%d %B del %Y'),' al ',
                             #'Campo de Viento\n','Del ',format(fecha.inicial-dias.atras,format='%d %B del %Y')),
                             format(fecha.final,format='%d %B del %Y')),
                caption='Fuente: IFREMER CERSAT Global Blended Mean Wind Fields on 25km X 25km grid')
pp <- pp + theme_bw()
pp <- pp + theme( axis.title.x = element_text( size = 28,hjust=0.5  ),
                  axis.title.y = element_text( size = 28,hjust=0.5  ),
                  axis.text = element_text( size = 20),
                  plot.title = element_text( size = 32),
                  plot.subtitle=element_text( size = 38) ,
                  plot.caption = element_text( size = 18,hjust = 0))

pp <- pp + guides( fill = guide_colorbar(  barheight = unit(20, "cm"),
                                           barwidth = unit(2,'cm'),
                                           label.theme = element_text(size=24),
                                           title = 'Magnitud\nm/s',
                                           title.theme = element_text(size=24)
))

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

anomalia <- data.frame( lon=vientos$lon,
                        lat=vientos$lat,
                        magnitud=vientos$mag- promedio.mag,
                        u=vientos$u,
                        v=vientos$v)

#### Suavizado #########

modelo.a<- mgcv::gam(magnitud~te(lon,lat,k=c(25,25)),data=anomalia)

mag.a <- predict(modelo.a,newdata=malla.i)

anomalia.i <- data.frame(lon=malla.i$lon,lat=malla.i$lat,mag=mag.a)
paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
###############################

niveles <- seq(from=-6,to=6,by=1)
if(exists('pp')) rm(pp)
png(file=paste0(figuras,'mapa_an_viento_',periodo,'_', fecha.inicial,'_',fecha.final,  '.png'),width=1000,height=1200)
pp <- ggplot(data=anomalia.i,aes(x=lon,y=lat,fill=mag))
pp <- pp + geom_raster(data=anomalia.i,aes(x=lon,y=lat,fill=mag),interpolate=TRUE)
pp <- pp + scale_fill_gradientn(colours = paleta_color,
                                breaks = niveles,
                                labels = as.character(niveles),
                                limits=range(niveles))

pp <- pp + geom_quiver(data=anomalia,
                       aes(x=lon,y=lat,u=u,v=v),
                       vecsize = 2,
                       size = 0.5,
                       inherit.aes=FALSE)

pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),
                         color = 'black', fill = 'grey80',inherit.aes=FALSE  )

for (kk in 1:length(fronteras)){
  f <- as.data.frame(fronteras[[kk]])
  pp <- pp + geom_point(data=f,aes( x=X1,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
}

pp <- pp + scale_x_continuous(limits = c(270,290),
                              expand = c(0,0),labels = (marcas_x-360),breaks = marcas_x)
pp <- pp + scale_y_continuous(limits = c(-20,2),
                              expand = c(0,0))

pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía\n','Anomalía del Campo de Viento'),
                subtitle=paste0('Del ',format(fecha.inicial,format='%d %B del %Y'),' al ',  
                             format(fecha.final,format='%d %B del %Y')),
                caption='Fuente: IFREMER CERSAT Global Blended Mean Wind Fields on 25km X 25km grid
                Climatología: QUIKSCAT-ASCAT 2000-2014' )
pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                  axis.title.y = element_text( size=28,hjust=0.5  ),
                  axis.text = element_text(size=28),
                  plot.title=element_text(size=32),
               plot.subtitle=element_text(size=38),
                  plot.caption = element_text(size = 18,hjust = 0))
pp <- pp + guides( fill = guide_colorbar(  barheight = unit(20, "cm"),
                                           barwidth = unit(2,'cm'),
                                           label.theme = element_text(size=24),
                                           title = 'Magnitud\nm/s',
                                           title.theme = element_text(size=24)
))

plot(pp)
dev.off()
}
