cat("\014") 
graphics.off()
rm(list=ls())

# library('RNetCDF')
setwd('C:/Users/gramirez/programasR/boletin/')
library('stringr')
library('tidyverse')
library('scales')
library('directlabels')
library('maptools')
library('reshape2')
require('RNetCDF')
require('metR')
gshhs.dir <- 'C:/Users/gramirez/programasR/gshhg-bin-2.3.7/'
carpetaFIg <- 'D:/NOAA/SST/figuras/'
load('costa_Peru_205.RDat')
figuraN <- 1

listaArchs <- list.files(path = 'D:/NOAA/SST/',pattern = '*.nc',full.names = TRUE)

lim.lon <- c(180, 290)
lim.lat <- c(-20, 5)

ncId <- open.nc(listaArchs[1])

lon <- var.get.nc(ncId,'lon')
lat <- var.get.nc(ncId,'lat')
temp <- 0.0*var.get.nc(ncId,'sst',start=c(1,1,1),count = c(NA,NA,1))
close.nc(ncId)

indc.lon <-  which(lon >=lim.lon[1] & lon<=lim.lon[2] )
indc.lat <-  which(lat >=lim.lat[1]& lat<=lim.lat[2])

ndatos <- 0

for (archivo in listaArchs){
  
  ncId <- open.nc(archivo)
  tiempo <- as.POSIXct(var.get.nc(ncId,'time')*86400,origin='1800-01-01')
  
  for (ii in 1:length(tiempo)){
    temp <- var.get.nc(ncId,'sst',start = c(1,1,ii),count = c(NA,NA,1) ) + temp
    ndatos <- ndatos + 1
    }
}

temp <- temp[indc.lon,indc.lat]/ndatos

temp <- stack(as.data.frame(temp))$values
###############################
malla <- expand.grid(lon[indc.lon],lat[indc.lat])

climatologiaSST <- data.frame(x=malla$Var1,y=malla$Var2,z=temp)

paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
niveles <- seq(10,30,by=1)

save(file = 'D:/NOAA/climatologia_1982-2018.RDat',list = 'climatologiaSST')
#######
subtitulo.grafico <- 'Climatología 1982-2018'
png(width=1200,height=800,filename = paste0(carpetaFIg,'climatologiaSST.png'))
pp <- ggplot(data=climatologiaSST,aes(x=x,y=y,z=z,fill=z))
pp <- pp+geom_raster(aes(fill = z),interpolate=TRUE,show.legend = TRUE  )
pp <- pp + scale_fill_gradientn( colours = paleta_color,
                                 limits = range(niveles))
pp <- pp + geom_contour(linetype = 1 ,
                        col = 'black' ,
                        breaks = niveles)
pp <- pp + geom_text_contour(stroke = 0.15,skip=0,
                             min.size =10,size=10,rotate = FALSE,
                             check_overlap=TRUE,breaks = niveles)
pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),color = 'black', fill = 'grey80',inherit.aes=FALSE  )
for (kk in 1:length(fronteras)){
  f <- as.data.frame(fronteras[[kk]])
  pp <- pp + geom_point(data=f,aes( x=X1,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
}
pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = subtitulo.grafico,
                caption = 'Fuente: ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2.highres/')
pp <- pp + theme_bw(   )
pp <- pp + scale_x_continuous(limits = lim.lon,
                              expand = c(0.01,0))
pp <- pp + scale_y_continuous(limits = lim.lat,
                              expand = c(0.01,0))
pp <- pp + guides( fill = guide_colorbar(  barheight = unit(14, "cm"),
                                           barwidth = unit(0.85,'cm'),
                                           label.theme = element_text(size=20),
                                           title = 'SST °C',
                                           title.theme = element_text(size=22)))
pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                  axis.title.y = element_text( size=28,hjust=0.5  ),
                  axis.text = element_text(size=20),
                  title=element_text(size=26),
                  plot.subtitle=element_text(size=24),
                  plot.caption = element_text(size = 18,hjust = 0))
plot(pp)
dev.off()

