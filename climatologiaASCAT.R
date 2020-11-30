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


limites.lon = c(270, 290)
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
#####################


if (exists('u')&exists('v')) rm(list=c('u','v'))

lista.arch.nc <- list.files(path=datos.dir,pattern = '\\.nc$')
narchs <- length(lista.arch.nc)
nc.ID <-  open.nc(paste0(datos.dir,lista.arch.nc[1]))

lon <- var.get.nc(nc.ID,variable='longitude') + 360

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

lon <- lon[ind.lon]
lat <- lat[ind.lat]

u <- array(dim=c(length(lon),length(lat),narchs))
v <- u

malla <- expand.grid(lon=lon,lat=lat)

for (ii in 1:narchs){
  arch <- paste0(datos.dir,lista.arch.nc[ii])
  if (file.exists(arch)){
    tryCatch(
     {nc.ID <-  open.nc(arch)  
      u[,,ii]<- var.get.nc(nc.ID,'eastward_wind',
                           start=c(cuales.lon[1],cuales.lat[1],1,1),
                           count=c(length(cuales.lon),length(cuales.lat),1,1))
      v[,,ii] <- var.get.nc(nc.ID,'northward_wind',
                            start=c(cuales.lon[1],cuales.lat[1],1,1),
                            count=c(length(cuales.lon),length(cuales.lat),1,1))
      close.nc(nc.ID)},error = function(e) e, 
                      finally = print("no se leyó el archivo"))
   
   }else{
     print(paste0('No existe',arch))
   }
}

  U <- 0.01*apply(u,c(1,2),mean,na.rm=TRUE) 
  V <- 0.01*apply(v,c(1,2),mean,na.rm=TRUE)
  
  rm(list=c('u','v'))
  u <- U
  v <- V
  rm(list=c('U','V'))
  
  u_p <- u
  v_p <- v
  lon_p <- lon
  lat_p <- lat
  ##############################  
save(list = c('u_p','v_p','lon_p','lat_p','cuales.lon','cuales.lat'),
                  file = 'viento_climat_GRASP_2000-2014.RData')  
  rm(list=c('u_p','v_p'))
#################################  
  
  magnitud <- sqrt(u^2+v^2)
  
  Z <- stack( as.data.frame(magnitud) )
  u <- stack( as.data.frame(u) )
  v <- stack( as.data.frame(v) )
  mapa <- data.frame( lon = as.matrix(malla$lon),
                      lat = as.matrix(malla$lat),
                      mag = as.matrix(Z$values),
                      u = as.matrix(u$values),
                      v = as.matrix(v$values))
  ################################################
  malla <- expand.grid(lon_p,lat_p)
  u <- stack(as.data.frame(u))$values
  v <- stack(as.data.frame(v))$values
  mapa <- data.frame(lon=malla$Var1,lat=malla$Var2,u=u,v=v)
  mapa$mag <- sqrt(mapa$u^2+mapa$v^2)
  dejar <- floor(seq(from=1,to=length(u),length.out = floor(length(u)/3)  )) 
  vectores <- mapa[dejar,]
##################################
  paleta_color <- c('#f0f0f0','#b4b4b4','#7878f7','#3c3cfb','#6000ff','#0040ff',
                    '#0080ff','#00bfff','#00ffff','#2bffd4','#55ffaa','#80ff80',
                    '#aaff55','#d4ff2b','#ffff00','#ffd400','#ffaa00','#ff8000',
                    '#ff5500','#ff2b00','#ff0000','#d40000','#aa0000','#800000')
  
  niveles <- seq(from=0,to=12,by=1)
  
##################################  
  load('costa_viento.RDat') 
  # shore$long <- shore$long-360
  
  png(file=paste0(figuras,'Cviento_2000-2014.png'),width=1000,height=1200)
  
  pp <- ggplot(data=mapa,aes(x=lon,y=lat,fill=mag))
  pp <- pp + geom_raster(interpolate=TRUE)
  pp <- pp + scale_fill_gradientn(colours = paleta_color,
                                  breaks = niveles,
                                  labels = as.character(niveles),
                                  limits=range(niveles))
  pp <- pp + geom_quiver(data=vectores,
                         aes(x=lon,y=lat,u=u,v=v),
                         vecsize = 5,
                         size = 1,
                         inherit.aes=FALSE)
  pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),color = 'black',
                           fill = 'grey80',inherit.aes=FALSE  )
  # pp <- pp + geom_contour(aes(z=mag),
  #                         color='black',show.legend = TRUE,bins = 10,line.width=2)
  # 
  # for (kk in 1:length(fronteras)){
  #   f <- as.data.frame(fronteras[[kk]])
  #   pp <- pp + geom_point(data=f,aes( x=X1,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
  # }
  # pp <- pp + scale_x_continuous(limits=range(limites.lon),
  #                               expand=c(0,0))
  # pp <- pp + scale_y_continuous(limits=range(limites.lat),
  #                               expand=c(0,0))
  pp <- pp + coord_cartesian(xlim=limites.lon,ylim=limites.lat)
  pp <- pp + labs(x='Longitud',y='Latitud',
                  title='Climatología del campo de viento: 2000-2014',
                  caption='Fuente: Remote Sensing Systems ASCAT C-2015\n Daily Ocean Vector Winds on 0.25 deg grid, Version 02.1
                \n Elaboración: Dirección de Hidrografía y Navegación, División de Oceanografía')
  
  pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                    axis.title.y = element_text( size=28,hjust=0.5  ),
                    axis.text = element_text(size=30),
                    title=element_text(size=32),
                    plot.caption = element_text(size = 22,hjust = 0))
  pp <- pp + guides( fill = guide_colorbar(  barheight = unit(11, "cm"),
                                             barwidth = unit(1,'cm'),
                                             label.theme = element_text(size=24),
                                             title = 'Mag\nm/s',
                                             title.theme = element_text(size=24)
  ))
  plot(pp)
dev.off() 
  
  
  
  