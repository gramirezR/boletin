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
library('XML')

bajar <- FALSE


if (bajar) {
     fecha.ini <- as.Date('2002-01-01')
     fecha.fin <- as.Date('2009-12-31')

         anios <- format(seq(from=fecha.ini,to=fecha.fin,by='year'),format='%Y/')

     datos.ftp <- 'https://cfs.ncep.noaa.gov/cfs/godas/pentad/'
   datos.local <- 'D:/GODAS_1989-2009/'

carpetas.godas <- as.list( paste0(datos.ftp,anios) )

lista.archivos <- lapply(carpetas.godas,function(x){
                       getURL(x,dirlistonly = TRUE,verbose=TRUE,ftp.use.epsv=TRUE)
})

lista.archivos <- unlist(lapply(lista.archivos,getHTMLLinks))


indc <- grep(x =  lista.archivos,pattern = '*.grb$')

lista.archivos <- lista.archivos[indc]

lista.archivos <- lista.archivos[27:584]
   
  

lapply(lista.archivos,function(x) {
                        aniop <- substring(text = x,first=9,last=12)
                        bajar <- paste0(datos.ftp,aniop,'/',x)
                      guardar <- paste0(datos.local,x)
                      tryCatch(exp=download.file(url=bajar,destfile = guardar,method = 'wget'),
                              error=function(e) print('no hay archivo'),finally= print('fin'))
                      
} )}
########################

rm(list=ls())

datos.local <- 'D:/GODAS_1989-2009/'


lista.archs <- list.files(path = datos.local,pattern = '*.grb$')

narchs <- length(lista.archs)



#################POLÍGONO PARA PROMEDIAR#####################

poli <- matrix(c(-78.58496522661444,1.021363921916344,
                 -78.50821467634449,1.995645822855158,
                 -80.30572848827556,2.025213831709041,
                 -81.2372525805624,1.181851944834083,
                 -81.79922620545072,0.3147624559786603,
                 -82.21204959552139,-0.7914000688917554,
                 -82.35469370390547,-1.76458520377435,
                 -82.44274298290497,-5.970298698269664,
                 -80.20921895298528,-9.310288457483033,
                 -78.55209504689213,-11.66145904313229,
                 -76.36591298318982,-14.99821017255604,
                 -75.57632675617657,-15.72360097456322,
                 -72.81356440565773,-17.21642432319155,
                 -70.88302457895013,-18.52557999293764,
                 -70.51836406783396,-23.3255325661761,
                 -69.70814968403121,-23.45032018737819,
                 -69.69105217116025,-18.43992354088474,
                 -74.21932794176908,-14.89847456848834,
                 -80.01421792420165,-5.944666319555537,
                 -80.4984813774702,-3.869946019332824,
                 -79.57004536265183,-3.344470234683232,
                 -79.3287430140312,-2.380465782697968,
                 -80.28674566945817,-2.019071907113946,
                 -79.64826791466814,0.5552058725350278,
                 -78.58496522661444,1.021363921916344),nrow=25,ncol=2,byrow=TRUE)

poli <- as.data.frame(poli)

names(poli) <- c('lon','lat')


poli$lon <- 360+poli$lon

###########################

lat <- seq(from=-74.66667,to=64.66567,length.out = 418)
lon <- seq(from=0,to=360,length.out = 360)

profs <- c(5,15,25,35,45,55,65,75,85,95,105,115,125,135,145,155,165,175,185,195,205,
           215,225,238,262,303,366,459,584,747,949,1193,1479,1807,2174,
           2579,3016,3483,3972,4478)
  ymax <- 200
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
Tpot <- array(dim=c(nlats,nProfs),data = 0)
tmpo <- Tpot

for (kk in 1:narchs){
  datos <- brick(paste0(datos.local,lista.archs[[kk]]))
  for (ii in seq(from=1,to=nProfs,by=1)){
    tpot <- data.frame( y = malla$lat[indc],
                        z = extract(datos,poli.sp,layer=ii,nl=1) )
    names(tpot) <- c('y','z')
    for (jj in 1:nlats){
      indc2 <- which( tpot$y ==lat.p[jj]  )
      tmpo[jj,ii] <- mean(tpot$z[indc2],na.rm=TRUE)
    }}
  
  Tpot <- tmpo + Tpot
}

Tpot <- Tpot/narchs
####################################
Tpot[Tpot>35 |Tpot<0 ] <- NA

Tpot2 <- stack(as.data.frame(Tpot))

malla <- expand.grid(lat.p,profs)

mapa <- data.frame(lat=malla$Var1,
                   prof=malla$Var2,
                   pot=Tpot2$values)

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
    filename =paste0('C:/Users/gramirez/ANM/boletin/temp/2019/figuras/climatologia_GODAS_1981-2009.png'))

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
                           expand = c(-0.1,0),
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
