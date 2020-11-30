cat('\014')
rm(list=ls())

library('RNetCDF')
library('tidyverse')
library('ggquiver')
library('RCurl')
library('magrittr')
setwd('D:/programasR/boletin')
ftp.ascat <- 'ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/MWF/L3/ASCAT/Daily/Netcdf/'
ftp.quick <- 'ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/MWF/L3/QuikSCAT/Daily/Netcdf/'
datos.dir <- paste0('D:/boletin/viento/climatologia.NetCDF/')
figuras <- paste0('D:/boletin/viento/')


climatologia <- c(2000,2014)


limites.lon = c(270, 290)-360
limites.lat = c(-20, 2)

bajar=FALSE

if(bajar){
  lista.dias.quick <- seq(from = as.Date('2000-01-03',origin='1970-01-01'),
                        to = as.Date('2007-12-31',origin='1970-01-01'),
                        by = 'day')


lista.dias.ascat <- seq(from = as.Date('2008-01-01',origin='1970-01-01'),
                        to = as.Date('2014-12-31',origin='1970-01-01'),
                        by = 'day')
##################
if (!file.exists('quickscatNC.RData')){
  arch <- paste0(ftp.quick,gsub(x = lista.dias.quick[1],pattern = '-',replacement = '/'),'/')
  
  arch2 <- getURL(arch,dirlistonly = TRUE,verbose=TRUE)
  arch3 <- str_extract(string =arch2,pattern = '.*bz2$' )
  arch4 <- paste0(arch,arch3)
  arch5 <- paste0(datos.dir,arch3)
  # download.file( url=arch4, destfile=arch5,
  #                method = "auto",quiet = FALSE, mode="wb", cacheOK = TRUE  )
  
  arch.sal <- substring(text = arch5,first=1,last= (str_length(arch5)-4) )
  # R.utils::bunzip2(arch5,
  #            destname=arch.sal,
  #            skip  =FALSE,
  #            remove=TRUE,
  #            overwrite=TRUE)
  
  ############################
  ncId <- open.nc(arch.sal)
  
  lat <- var.get.nc(ncId,variable = 'latitude')
  
  lon <- var.get.nc(ncId,variable = 'longitude') + 360
  
  close.nc(ncId)
  
  # ###############
  #   
  indc.x <- which((lon > limites.lon[1]) & (lon < limites.lon[2]))
  
  indc.y <- which((lat > limites.lat[1]) & (lat < limites.lat[2]))
  
  inix <- indc.x[1]
  cuantos.x <- diff(range(indc.x))+1
  
  iniy <- indc.y[1]
  cuantos.y <- diff(range(indc.y))+1
  
  
  save(file='quickscatNCGRASP.RData',list=c('inix','iniy','cuantos.x','cuantos.y'))
}else{
  load('quickscatNC.RData')
}
###############################
load('quickscatNCGRASP.RData')
arch.sal <- list.files(path=datos.dir,full.names = TRUE)
########### QUICKSCAT #########################

lista.archs.quick <- unlist(lapply(lista.dias.quick,
                                   function(x) {
                                     nomb <- gsub(x=x,pattern = '-',replacement = '/')
                                     paste0(ftp.quick,nomb,'/')
                                   }
)
)

lapply(lista.archs.quick[-seq(1,675)], function (x){
  tryCatch(
    {bajar <- unlist(strsplit(getURL(x,dirlistonly = TRUE,verbose=TRUE),'\r\n'))
    descomp <- paste0(datos.dir, bajar)
    arch.sal <- str_extract(descomp,pattern='.*[^.bz2]')
    closeAllConnections()
    if (!file.exists(arch.sal)){
      download.file( url=paste0(x, bajar), destfile=descomp,
                     quiet = FALSE, mode="wb", cacheOK = TRUE  )
      closeAllConnections()
      R.utils::bunzip2(descomp,
                       destname=arch.sal,
                       skip  =FALSE,
                       remove=TRUE,
                       overwrite=TRUE)}
    },error=function(e){print('No existe el archivo')}
  )
})

########### ASCAT ###############################  

lista.archs.ascat <- unlist(lapply(lista.dias.ascat,
                                   function(x) {
                                     nomb <- gsub(x=x,pattern = '-',replacement = '/')
                                     paste0(ftp.ascat,nomb,'/')
                                   }
)
)

lapply(lista.archs.ascat, function (x){
  tryCatch(
    {bajar <- unlist(strsplit(getURL(x,dirlistonly = TRUE,verbose=TRUE),'\r\n'))
    descomp <- paste0(datos.dir, bajar)
    arch.sal <- str_extract(descomp,pattern='.*[^.bz2]')
    closeAllConnections()
    if (!file.exists(arch.sal)){
      download.file( url=paste0(x, bajar), destfile=descomp,
                     quiet = FALSE, mode="wb", cacheOK = TRUE  )
      closeAllConnections()
      R.utils::bunzip2(descomp,
                       destname=arch.sal,
                       skip  =FALSE,
                       remove=TRUE,
                       overwrite=TRUE)}
    },error=function(e){print('No existe el archivo')}
  )
})

}
######################

if (exists('u')) rm(list='u')
if(exists('v'))rm(list='v')

lista.arch.nc <- list.files(path=datos.dir,pattern = '*.nc$')
narchs <- length(lista.arch.nc)
nc.ID <-  open.nc(paste0(datos.dir,lista.arch.nc[1]))

lon <- var.get.nc(nc.ID,variable='longitude') 

lat <- var.get.nc(nc.ID,variable='latitude') 


ind.lon <- (lon>=limites.lon[1])&(lon<=limites.lon[2])
ind.lat <- (lat>=limites.lat[1])&(lat<=limites.lat[2])

cuales.lon <- which(ind.lon)
cuales.lat <- which(ind.lat)

# A <- var.get.nc(nc.ID,'eastward_wind',
#                 start=c(cuales.lon[1],cuales.lat[1],1,1),
#                 count=c(length(cuales.lon),length(cuales.lat),1,1))
# B <- var.get.nc(nc.ID,'northward_wind',
#                 start=c(cuales.lon[1],cuales.lat[1],1,1),
#                 count=c(length(cuales.lon),length(cuales.lat),1,1))
# close.nc(nc.ID)
# 
# A[is.na(A)] = 0
# B[is.na(B)] = 0

lon <- lon[cuales.lon]
lat <- lat[cuales.lat]

# u <- array(dim=c(length(lon),length(lat),narchs))
# v <- u

######################## PROMEDIO MENSUAL ##################
source('separar_por_mes.R')
lista.arch.nc <-  separar_por_mes(lista.arch.nc)
source('leer_por_mes.R')


u <- leer_por_mes(    directorio = datos.dir,
                  lista_archivos = lista.arch.nc,
                        variable = 'eastward_wind',
                          inicio = c(cuales.lon[1],cuales.lat[1],1,1),
                         cuantos = c(length(cuales.lon),length(cuales.lat),1,1))

v<- leer_por_mes(    directorio = datos.dir,
                 lista_archivos = lista.arch.nc,
                       variable = 'northward_wind',
                         inicio = c(cuales.lon[1],cuales.lat[1],1,1),
                        cuantos = c(length(cuales.lon),length(cuales.lat),1,1))

u_p <- u
v_p <- v
lon_p <- lon
lat_p <- lat
##############################  
save(list = c('u_p','v_p','lon_p','lat_p','cuales.lon','cuales.lat'),
     file = 'viento_climat_GRASP_2000-2014.RData')  
rm(list=c('u_p','v_p'))
#################################  
source('interpolar_componente.R')
malla <- expand.grid(lon_p,lat_p)
names(malla) <- c('lon','lat')


 u2 <- interpolar_componente(u,malla)
# 
 v2 <- interpolar_componente(v,malla)

  parcial <- Map(function(x,y){
   indcx <- is.na(x)
   x[indcx] <- 0
   indcy <- is.na(y)
   y[indcy] <- 0
   z <-  x+y
 },
 lapply(u2,function(x){x$valor^2}),
 lapply(v2,function(x){x$valor^2}))
 
magnitud <- lapply(parcial,function(x){sqrt(x)})
###########################################
rm(list=c('u','v'))


load('costa_05-20.RData') 
Z <- lapply(magnitud,function(x) stack( as.data.frame(x) )$values)

source('quitarPuntosDentroCosta.R')

indcCosta2 <- quitarPuntosDentroCosta(malla)

for (ii in 1:12){
  Z[[ii]][indcCosta2] <- NA
  u2[[ii]]$valor[indcCosta2] <- NA
  v2[[ii]]$valor[indcCosta2] <- NA
}

###################
# windows()
# plot(shore,col='red')
#  
#  points(malla$lon[indcCosta2[,1]]-360,malla$lat[indcCosta2[,1]])


########################## 
 mapa <- vector(mode='list',length=12)
for (ii in 1:12){
mapa[[ii]] <- data.frame( lon = as.matrix(u2[[ii]]$lon),
                    lat = as.matrix(u2[[ii]]$lat),
                    mag = as.matrix(Z[[ii]]),
                    u = as.matrix(u2[[ii]]$valor),
                    v = as.matrix(v2[[ii]]$valor))}
 saveRDS(mapa,file='ClimatologiaMesViento.RDS')
################################################

quitar <- floor(seq(from=1,to=length(u[[1]]),length.out = 6000)) 
vectores <- lapply(mapa,function(x) x[-quitar,])
##################################
paleta_color <- c('#f0f0f0','#b4b4b4','#7878f7','#3c3cfb','#6000ff','#0040ff',
                  '#0080ff','#00bfff','#00ffff','#2bffd4','#55ffaa','#80ff80',
                  '#aaff55','#d4ff2b','#ffff00','#ffd400','#ffaa00','#ff8000',
                  '#ff5500','#ff2b00','#ff0000','#d40000','#aa0000','#800000')

niveles <- seq(from=0,to=12,by=1)
###########################
shore <- fortify(shore)
shore$long <- shore$long
##################################  
graphics.off()

meses <- lubridate::month(ISOdate(2019,seq_along(mapa),1,0,0,0),label=TRUE,abbr=FALSE) 
# shore$long <- shore$long-360
# seq_along(mapa)
nContornos <- 10
for (ii in seq_along(mapa)){
  M <- mapa[[ii]]
  V <- vectores[[ii]]
  contornos <- round(seq(from = min(M$mag,na.rm = TRUE),
                     to = max(M$mag,na.rm = TRUE),
                   length.out = nContornos),digits=1)
  png(file=paste0(figuras,'CvientoG_2000-2014',as.character(meses[ii]),'.png'),
                          width=1000,height=1200)
  pp <- ggplot(data=M,aes(x=lon,y=lat,fill=mag))
  pp <- pp + geom_raster(interpolate=TRUE)
  pp <- pp + scale_fill_gradientn(colours = paleta_color,
                                  breaks = niveles,
                                  labels = as.character(niveles),
                                  limits=range(niveles))
  pp <- pp + geom_quiver(data=V,
                         aes(x=lon,y=lat,u=u,v=v),
                         vecsize = 5,
                         size = 1,
                         inherit.aes=FALSE)
  pp <- pp + geom_contour(aes(z=mag),
                          color ='black',show.legend = TRUE,
                           size=1.,binwidth=0.5)
  pp <- pp + metR::geom_text_contour(aes(z=mag),
                                     stroke = 0.15,skip=0,min.size =5,size=12,
                                     rotate = FALSE,
                                     check_overlap=TRUE)
  pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),color = 'black',
                           fill = 'grey80',inherit.aes=FALSE  )
  for (kk in 1:length(fronteras)){
    f <- as.data.frame(fronteras[[kk]])
    pp <- pp + geom_point(data=f,aes( x=X1,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
  }
  pp <- pp + coord_cartesian(xlim=limites.lon,ylim=limites.lat,expand = FALSE)
  pp <- pp + labs(x='Longitud',y='Latitud',
                  title=paste0('Climatología del campo de viento:\n',as.character(meses[ii]),' 2000-2014'),
                  caption='Fuente: Remote Sensing Systems ASCAT C-2015\n Daily Ocean Vector Winds on 0.25 deg grid, Version 02.1
                \n Elaboración: Dirección de Hidrografía y Navegación, División de Oceanografía')
  
  pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                    axis.title.y = element_text( size=28,hjust=0.5  ),
                    axis.text = element_text(size=30),
                    title=element_text(size=32),
                    plot.caption = element_text(size = 22,hjust = 0)                      )
  pp <- pp + guides( fill = guide_colorbar(  barheight = unit(11, "cm"),
                                             barwidth = unit(1,'cm'),
                                             label.theme = element_text(size=24),
                                             title = 'Mag\nm/s',
                                             title.theme = element_text(size=24)
  ))
  plot(pp)
  dev.off() 
  rm(list=c('pp','M','V'))
}


