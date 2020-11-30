cat('\014') 
graphics.off()
rm(list=ls())

library('stringr')
library('tidyverse')
library('scales')
library('metR')
library('maptools')
library('reshape2')
library('ggquiver')
library('RCurl')
library('raster')
library('mgcv')
library('rasterVis')

   datos.ftp <- 'https://cfs.ncep.noaa.gov/cfs/godas/pentad/2019/'

 datos.local <- 'D:/GODAS_tpot/'
 
 rdata.arch <- 'peru_costa_200mi.RData'

       fecha <- '20190301'

         ymax <- 200
       lat.tr <- -18       
       
     archivo <- paste0('godas.P.',fecha,'.grb')
arch.sin.grb <- substring(text = archivo,first=9,last=16)

      salida <- paste0(datos.local,archivo)

if(!file.exists(salida)){
  download.file( url=paste0(datos.ftp, archivo), destfile=salida,
                 method = "auto",quiet = FALSE, mode="wb", cacheOK = TRUE  )}

 

## 1:40 POT ; 41:80 SAL ; 81:20 UOGRD ; 121:160 VOGRD ; 161:200 DZDT ; 201 isoterma
## 202 capa de mezcla ; 203:207 informacion irrelevante

################### REGION GLOBAL ######
      
load(rdata.arch) 
datos <- brick(salida)
      
lat <- seq(from=-74.66667,to=64.66567,length.out = 418)
lon <- seq(from=0,to=360,length.out = 360)

profs <- c(5,15,25,35,45,55,65,75,85,95,105,115,125,135,145,155,165,175,185,195,205,
           215,225,238,262,303,366,459,584,747,949,1193,1479,1807,2174,
           2579,3016,3483,3972,4478)

malla.dat <- expand.grid(lon,lat)

names(malla.dat) <- c('lon','lat')

nProfs <- length(profs)


########## ARMADO DE LA MATRIZ DE DATOS##############

poli <- data.frame(costa@polygons[[1]]@Polygons[[1]]@coords)

names(poli) <- c('lon','lat')

indc <- which(point.in.polygon(   lon, rep(lat.tr,length(lon)),
                                  poli$lon,poli$lat )==1)

     lon.p <- lon[indc]
   profMax <- 200
lon.interp <- seq( from = min(lon.p),to = (min(lon.p)+200/60), length.out = 200 )
lat.interp <- rep(lat.tr,length(lon.interp))
  z.interp <- seq(from = 0, to = profMax, length.out = 200  )

 malla.int <- data.frame(lon= lon.interp, lat=lat.interp)
  
########### RECORTE DEL MAR PERUANO ##################

pol.lat <- c(-20,2,2,-20,-20)
pol.lon <- c(290,290,270,270,290)

load('peru_costa_200mi.RData')

indc <- which(point.in.polygon(   malla.dat$lon, malla.dat$lat,
                                  pol.lon,pol.lat )==1)

poli.sp <- SpatialPoints(cbind(malla.dat$lon[indc],malla.dat$lat[indc]))
proj4string(poli.sp) <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')

valor <- matrix(nrow =length(profs),ncol = length(lon.interp) )

for (ii in 1:length(profs)){
  datos.GODAS <- raster::extract(datos,poli.sp,layer=ii,nl=1)
         tPot <- data.frame( lon = malla.dat$lon[indc],
                             lat = malla.dat$lat[indc],
                           valor = datos.GODAS[,1]  )
       modelo <- mgcv::gam( valor ~ te(lon,lat,k=c(15,15)),data=tPot  )
   valor[ii,] <- predict(modelo, newdata=malla.int)
}

##########  SUAVIZADO ################
       mapa.dats <- expand.grid(profs,malla.int$lon)
names(mapa.dats) <- c('prof','lon')
   mapa.dats$pot <- stack(as.data.frame(valor))$value

       mapa.nuevo <- expand.grid(z.interp,malla.int$lon)
names(mapa.nuevo) <- c('prof','lon')
           modelo <- mgcv::gam( pot ~ te(prof,lon,k=c(15,15)),data=mapa.dats  )

   mapa.nuevo$pot <- predict(modelo, newdata=mapa.nuevo)


# mapa.nuevo$lon <- (mapa.nuevo$lon - min(mapa.nuevo$lon))*60
mapa.nuevo$millas <- (mapa.nuevo$lon - min(mapa.nuevo$lon))

# modelo <- mgcv::gam( pot ~ te(prof,lon,k=c(15,15)),data=mapa.nuevo)

# mapa.nuevo <- predict(modelo,newdata=mapa.nuevo)

#######################

# paleta <- cptcity::cpt('ncl_amwg_blueyellowred')
  paleta <- c( '#f0f0f0','#b4b4b4','#7878f7','#3c3cfb','#6000ff','#0040ff',
               '#0080ff','#00bfff','#00ffff','#2bffd4','#55ffaa','#80ff80',
               '#aaff55','#d4ff2b','#ffff00','#ffd400','#ffaa00','#ff8000',
               '#ff5500','#ff2b00','#ff0000','#d40000','#aa0000','#800000' )

marcas_x <- ceiling(rev( seq(from=0,to=20/6,length.out=10) ))
 etqts_x <- ceiling(marcas_x)*60
 
######################
 
contornos <- seq(from=0,to=30,by=1)
png(width=1200,height=850,
    filename =paste0('C:/Users/gramirez/ANM/boletin/temp/2019/figuras/transecto_prueba',
                     arch.sin.grb,'_',abs(lat.tr),'S.png'))

pp <- ggplot(data=mapa.nuevo,aes(x=millas,y=prof,fill=pot))
pp <- pp + geom_raster(interpolate=TRUE)
pp <- pp + scale_fill_gradientn(colours=paleta,limits=c(0,30),breaks=seq(from=0,to=30,by=2))
pp <- pp + stat_contour(aes(x=millas,y=prof,z=pot),col='grey45' ,breaks=contornos )
pp <- pp + geom_text_contour(aes(x=millas,y=prof,z=pot), stroke = 0.15,skip=0,min.size =3,
                             size=10,rotate = FALSE,check_overlap = TRUE,breaks=contornos)
pp <- pp + scale_y_reverse(limits=c(profMax,0),
                           expand = c(0,0),
                           breaks=c(seq(ymax,20,by=-20),5) )
pp <- pp + scale_x_reverse(limits=rev(c(0,200)/60),
                           expand = c(0,0),
                           breaks =marcas_x,
                           labels = etqts_x)
pp <- pp + theme_bw()

pp <- pp + labs(x = 'Distancia de la costa (mi)',y = 'Profundidad (m)',
                title = paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = paste0('Temperatura Potencial: Pentada del ',
                                  format(as.Date(fecha,format='%Y%m%d'),format='%d de %B de %Y'),
                                  '\nTransecto de 200 millas ',abs(lat.tr),'S'),
                caption = 'Fuente: Global Ocean Data Assimilation System (GODAS)\n https://cfs.ncep.noaa.gov/cfs/godas/pentad/')

pp <-  pp + theme( axis.title.x = element_text( size=20,hjust=0.5  ),
                   axis.title.y = element_text( size=20,hjust=0.5  ),
                   axis.text = element_text(size=20),
                   title=element_text(size=26),
                   plot.subtitle=element_text(size=24),
                   plot.caption = element_text(size = 14,hjust = 0))
pp <- pp + guides( fill = guide_colorbar(barheight = unit(20, "cm"),
                                         barwidth = unit(1,'cm'),
                                         label.theme = element_text(size=18),
                                         title = '°C',title.theme = element_text(size=18)))

plot(pp)
dev.off()

############# ANOMALIA GODAS ######################

# temp.GODAS <- mapa.nuevo

load('D:/GODAS_1981-2009/climatologia_GODAS_1981-2009.RDat')

anomalia.Temp <- data.frame( lat = mapa.nuevo$lat,prof = mapa.nuevo$prof,
                             pot = mapa.nuevo$pot - temp.GODAS$pot)


#####################################################
# niveles <- seq(from=-10,to=10,by=1)
# ymax <- 150
# png(width=1200,height=850,
#     filename =paste0('C:/Users/gramirez/ANM/boletin/temp/2019/figuras/anomalia_temp_GODAS_',
#                      arch.sin.grb,'.png'))
# 
# pp <- ggplot(data=anomalia.Temp,aes(x=lat,y=prof,fill=pot))
# pp <- pp + geom_raster(interpolate=TRUE)
# pp <- pp + scale_fill_gradientn(colours=paleta,limits=range(niveles),breaks=niveles)
# pp <- pp + stat_contour(aes(x=lat,y=prof,z=pot),col='grey45' ,breaks=niveles )
# pp <- pp + geom_text_contour(aes(x=lat,y=prof,z=pot), stroke = 0.15,skip=0,min.size =3,
#                              size=10,rotate = FALSE,check_overlap = TRUE,breaks=niveles)
# pp <- pp + scale_y_reverse(limits=c(ymax,0),
#                            expand = c(0,0),
#                            breaks=c(seq(ymax,20,by=-20),5) )
# pp <- pp + scale_x_reverse(limits=rev(range(x)),
#                            expand = c(0,0),
#                            breaks=marcas_x ,
#                            labels=etiquetas_x)
# pp <- pp + theme_bw()
# 
# pp <- pp + labs(x = 'Latitud',y = 'Profundidad (m)',
#                 title = paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
#                                'Dpto. de Oceanografía - Div. Oceanografía'),
#                 subtitle = paste0('Anomalía de Temperatura Potencial: Pentada del ',
#                                   format(as.Date(fecha,format='%Y%m%d'),format='%d de %B de %Y'),
#                                   '\nFranja de 50 millas'),
#                 caption = 'Fuente: Global Ocean Data Assimilation System (GODAS)\nClimatología: 1989-2009\nhttps://cfs.ncep.noaa.gov/cfs/godas/pentad/')
# 
# pp <-  pp + theme( axis.title.x = element_text( size=20,hjust=0.5  ),
#                    axis.title.y = element_text( size=20,hjust=0.5  ),
#                    axis.text = element_text(size=20),
#                    title=element_text(size=26),
#                    plot.subtitle=element_text(size=24),
#                    plot.caption = element_text(size = 18,hjust = 0))
# pp <- pp + guides( fill = guide_colorbar(barheight = unit(20, "cm"),
#                                          barwidth = unit(1,'cm'),
#                                          label.theme = element_text(size=18),
#                                          title = '°C',title.theme = element_text(size=18)))
# 
# plot(pp)
# dev.off()
# 



