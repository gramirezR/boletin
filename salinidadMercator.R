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

limite.lon <- c(-95,-75)
limite.lat <- c(-6,2)

# limite.lon <- c(-85,-66)
# limite.lat <- c(-60,-18)

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
cuantos <- c(cuantos.lon,cuantos.lat,50,1)


capaMezcla <- promedio(ncConn,'so',inicio,cuantos,factor.var=0.00152592547237873) -0.00152592547237873

close.nc(ncConn)

temporalCm <- 0*capaMezcla

longitud <- longitud[indc.lon]
latitud <- latitud[indc.lat]

mapa <- pasar.aRaster(capaMezcla,longitud,latitud)

windows()
plot(mapa)
rm('mapa')

##########################



salinidad <- vector(mode='list',length=(12))

for (jj in 1:12){
  kk <- 0
  temporalS <- temporalCm*0
  
  for (ii in seq(from=jj,to=narchs,by=12)){
    tryCatch({  
      ncConn <- open.nc(lista.archivos.local[ii])
      
      temporalS <- temporalS + promedio(ncConn,'so',inicio,cuantos,factor.var=1)
      
      # temporalU <- temporalU + pasar.aRaster(,longitud,latitud)
      # 
      # temporalV <- temporalV + pasar.aRaster(promedio(ncConn,'vo',inicio,cuantos,factor.var=0.000610370188951492),longitud,latitud)
      kk <- kk+1
      close.nc(ncConn)},error=function(e) print(ii)
    )
  }
  
  salinidad[[jj]] <- pasar.aRaster((temporalS/kk)*0.00152592547237873 -0.00152592547237873,longitud,latitud)
  proj4string(salinidad[[jj]]) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  
}


################################

salinidad <- lapply(salinidad,FUN=function(x){
  if (!is.null(x)){
    raster2ggplot(x)}
}) 


#########################################

paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
load('D:/programasR/boletin/costa_ecuador.RData') 
meses <- months(x = seq(from=as.Date('2020-01-01'),to=as.Date('2020-12-01'),by='month'))
##################################
graphics.off()

salMin <- 31
salMax <- 35.5
for (ii in 1:12){
  png(width=1200,height=800,file=paste0(raiz,'figuras_ecuador/salinidad',sprintf(ii,fmt='%02d'),'.png'))
  pp <- ggplot(data=salinidad[[ii]],aes(x=lon,y=lat,fill=z,z=z))
  pp <- pp + geom_raster(interpolate=TRUE,show.legend = TRUE  )
  
  pp <- pp + scale_fill_gradientn(colours= paleta_color,
                                  breaks=seq(from=salMin,to=salMax,by=0.5),
                                  limits=c(salMin,salMax))
  
  pp <- pp + geom_contour(linetype = 1 ,
                          col = 'black' ,
                          breaks = seq(from=salMin,to=salMax,by=0.1))
  
  pp <- pp + metR::geom_text_contour(  stroke = 0.15,skip=0,min.size =5,size=12,
                                       breaks = seq(from=salMin,to=salMax,by=0.1),
                                       rotate = FALSE,
                                       check_overlap = TRUE)
  
  
  pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),
                           color = 'black', fill = 'grey80',inherit.aes=FALSE  )
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
                  title=paste0('DIRECCIÓN DE HIDROGRAFÍA\nY NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                  subtitle = paste0('Salinidad de ',meses[ii]),
                  caption = 'Fuente: COPERNICUS MARINE ENVIRONMENT\nMONITORING SERVICE (CMEMS v3.0).\nClimatología: 2000-2018')
  
  pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                    axis.title.y = element_text( size=28,hjust=0.5  ),
                    axis.text = element_text(size=28,colour = 'black'),
                    title=element_text(size=28),
                    plot.subtitle=element_text(size=24),
                    plot.caption = element_text(size = 22,hjust = 0))
  pp <- pp + guides( fill = guide_colorbar(  barheight = unit(22, "cm"),
                                             barwidth = unit(1.5,'cm'),
                                             label.theme = element_text(size=26),
                                             title = 'UPS',
                                             title.theme = element_text(size=26)))
  
  plot(pp)
  dev.off()
}








