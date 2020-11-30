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
 datos.local <- 'E:/GODAS_tpot/'
       fecha <- '20190301'
       rdata.arch <- 'peru_costa_50mi.RData'
     archivo <- paste0('godas.P.',fecha,'.grb')
arch.sin.grb <- substring(text = archivo,first=9,last=16)

      salida <- paste0(datos.local,archivo)

if(!file.exists(salida)){
download.file( url=paste0(datos.ftp, archivo), destfile=salida,
               method = "auto",quiet = FALSE, mode="wb", cacheOK = TRUE  )}



datos <- brick(salida)



## 1:40 POT ; 41:80 SAL ; 81:20 UOGRD ; 121:160 VOGRD ; 161:200 DZDT ; 201 isoterma
## 202 capa de mezcla ; 203:207 informacion irrelevante

#################POLÍGONO PARA PROMEDIAR#####################

load(rdata.arch) #### VARIABLE costa

poli <- costa@polygons[[1]]@Polygons[[1]]@coords


poli <- as.data.frame(poli)

names(poli) <- c('lon','lat')
#########################
lat <- seq(from=-74.66667,to=64.66567,length.out = 418)
lon <- seq(from=0,to=360,length.out = 360)

profs <- c(5,15,25,35,45,55,65,75,85,95,105,115,125,135,145,155,165,175,185,195,205,
           215,225,238,262,303,366,459,584,747,949,1193,1479,1807,2174,
           2579,3016,3483,3972,4478)
ymax <- 300
nProfs <- length(profs)
########################

malla <- expand.grid(lat,lon)

names(malla) <- c('lat','lon')

indc <- which(point.in.polygon(   malla$lon,  malla$lat,
                                poli$lon,poli$lat )==1)

lat.p <-sort(  unique(malla$lat[indc]),decreasing = TRUE)

malla.prof <- expand.grid(profs,lat.p)
names(malla.prof) <- c('prof','lat')
poli.sp <- SpatialPoints(cbind(malla$lon[indc],malla$lat[indc]))
####################################
nlats <- length(lat.p)
Tpot <- array(dim=c(nlats,nProfs))

for (ii in seq(from=1,to=nProfs,by=1)){
  tpot <- data.frame( y = malla$lat[indc],
                      z = extract(datos,poli.sp,layer=ii,nl=1) )
  names(tpot) <- c('y','z')
  for (jj in 1:nlats){
    indc2 <- which( tpot$y ==lat.p[jj]  )
    Tpot[jj,ii] <- mean(tpot$z[indc2],na.rm=TRUE)
  }}

########### SUAVIZADO DE LA IMAGEN ###############

Tpot[Tpot>35 |Tpot<0 ] <- NA

Tpot2 <- stack(as.data.frame(Tpot))

malla <- expand.grid(lat.p,profs)

mapa <- data.frame(lat=malla$Var1,
                   prof=malla$Var2,
                   pot=Tpot2$values)
# windows()
# pp <- ggplot(data=mapa,aes(x=lat,y=prof,fill=pot))
# pp <- pp + geom_raster() + scale_y_reverse()
# plot(pp)
###################################

y <- seq(from=0,to=ymax,by=1)
x <- seq(from=-20,to=2,by=0.1)

malla.int <- expand.grid(lat=x,prof=y)

interpolacion <- gam(pot~te(lat,prof,k=c(20,20)),data=mapa)

tmp <- predict(interpolacion,newdata=malla.int)

mapa.nuevo <- data.frame( lat=malla.int$lat,prof=malla.int$prof,pot=tmp )
#######################

# paleta <- cptcity::cpt('ncl_amwg_blueyellowred')
paleta <- c('#f0f0f0','#b4b4b4','#7878f7','#3c3cfb','#6000ff','#0040ff',
                  '#0080ff','#00bfff','#00ffff','#2bffd4','#55ffaa','#80ff80',
                  '#aaff55','#d4ff2b','#ffff00','#ffd400','#ffaa00','#ff8000',
                  '#ff5500','#ff2b00','#ff0000','#d40000','#aa0000','#800000')
marcas_x <- rev(seq(from=min(x),to=max(x),by=2))
etiquetas_x <- unlist(lapply( as.vector(marcas_x),
                              function(x) if( x<0 ) {paste0(-x,'S')}else if(x>0){paste0(x,'N')}else{x} )   )

contornos <- seq(from=0,to=30,by=1)
png(width=1200,height=850,
    filename =paste0('C:/Users/gramirez/ANM/boletin/temp/2019/figuras/perfil_',
                     arch.sin.grb,'.png'))

pp <- ggplot(data=mapa.nuevo,aes(x=lat,y=prof,fill=pot))
pp <- pp + geom_raster(interpolate=TRUE)
pp <- pp + scale_fill_gradientn(colours=paleta,limits=c(0,30),breaks=seq(from=0,to=30,by=2))
pp <- pp + stat_contour(aes(x=lat,y=prof,z=pot),col='grey45' ,breaks=contornos )
pp <- pp + geom_text_contour(aes(x=lat,y=prof,z=pot), stroke = 0.15,skip=0,min.size =3,
                             size=10,rotate = FALSE,check_overlap = TRUE,breaks=contornos)
pp <- pp + scale_y_reverse(limits=c(ymax,0),
                           expand = c(0,0),
                           breaks=c(seq(ymax,20,by=-20),5) )
pp <- pp + scale_x_reverse(limits=rev(c(-20,2)),
                           expand = c(0,0),
                           breaks=marcas_x ,
                           labels=etiquetas_x)
pp <- pp + theme_bw()

pp <- pp + labs(x = 'Latitud',y = 'Profundidad (m)',
                title = paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = paste0('Temperatura Potencial: Pentada del ',
                                  format(as.Date(fecha,format='%Y%m%d'),format='%d de %B de %Y'),
                                  '\nFranja de 50 millas'),
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

temp.GODAS <- mapa.nuevo

load('D:/GODAS_1989-2009/climatologia_GODAS_1989-2009.RDat')

anomalia.Temp <- data.frame( lat = mapa.nuevo$lat,prof = mapa.nuevo$prof, 
                             pot = mapa.nuevo$pot - temp.GODAS$pot)


#####################################################
niveles <- seq(from=-10,to=10,by=1)
ymax <- 150
png(width=1200,height=850,
    filename =paste0('C:/Users/gramirez/ANM/boletin/temp/2019/figuras/anomalia_temp_GODAS_',
                     arch.sin.grb,'.png'))

pp <- ggplot(data=anomalia.Temp,aes(x=lat,y=prof,fill=pot))
pp <- pp + geom_raster(interpolate=TRUE)
pp <- pp + scale_fill_gradientn(colours=paleta,limits=range(niveles),breaks=niveles)
pp <- pp + stat_contour(aes(x=lat,y=prof,z=pot),col='grey45' ,breaks=niveles )
pp <- pp + geom_text_contour(aes(x=lat,y=prof,z=pot), stroke = 0.15,skip=0,min.size =3,
                             size=10,rotate = FALSE,check_overlap = TRUE,breaks=niveles)
pp <- pp + scale_y_reverse(limits=c(ymax,0),
                           expand = c(0,0),
                           breaks=c(seq(ymax,20,by=-20),5) )
pp <- pp + scale_x_reverse(limits=rev(range(x)),
                           expand = c(0,0),
                           breaks=marcas_x ,
                           labels=etiquetas_x)
pp <- pp + theme_bw()

pp <- pp + labs(x = 'Latitud',y = 'Profundidad (m)',
                title = paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = paste0('Anomalía de Temperatura Potencial: Pentada del ',
                                  format(as.Date(fecha,format='%Y%m%d'),format='%d de %B de %Y'),
                                  '\nFranja de 50 millas'),
                caption = 'Fuente: Global Ocean Data Assimilation System (GODAS)\nClimatología: 1989-2009\nhttps://cfs.ncep.noaa.gov/cfs/godas/pentad/')

pp <-  pp + theme( axis.title.x = element_text( size=20,hjust=0.5  ),
                   axis.title.y = element_text( size=20,hjust=0.5  ),
                   axis.text = element_text(size=20),
                   title=element_text(size=26),
                   plot.subtitle=element_text(size=24),
                   plot.caption = element_text(size = 18,hjust = 0))
pp <- pp + guides( fill = guide_colorbar(barheight = unit(20, "cm"),
                                         barwidth = unit(1,'cm'),
                                         label.theme = element_text(size=18),
                                         title = '°C',title.theme = element_text(size=18)))

plot(pp)
dev.off()




