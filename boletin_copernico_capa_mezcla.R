cat("\014") 
graphics.off()
rm(list=ls())
setwd('E:/programasR/boletin/')
library('stringr')
library('tidyverse')
library('scales')
library('directlabels')
library('maptools')
library('reshape2')
require('RNetCDF')
source('bajarCopernico.R')
source('boletin_listaArchivos.R')

###############CARPETAS###############

#raiz <- 'C:/Users/gramirez/ANM/boletin/'
raiz <- 'E:/boletin/temp/capa_climatologia_Mensual/'
#setwd('C:/Users/gramirez/programasR/boletin/')
gshhs.dir <- 'E:/programasR/gshhg-bin-2.3.7/'
#      raiz <- 'C:/Users/Gerardo/Almacen/DHN/Depto_oceanografia/boletin/'
# gshhs.dir <-'C:/Users/Gerardo/Almacen/paquetesR/'
mes <- lubridate::month(seq(from=as.Date('2021-01-01'), to=as.Date('2021-12-31'), by='month'),label=TRUE, abbr=FALSE )
#############DEFINICION DE FECHAS##################

t0.copernico <- as.Date('1981-01-01') # segundos desde esta fecha

# dias.atras <- lubridate::yday(lubridate::today() )

lista_archivos <- list.files(path=raiz, pattern = '*.nc', full.names = TRUE, include.dirs = FALSE)

######################

# 'mlotst'

ncin <- open.nc(con = lista_archivos[[1]])

print.nc(ncin)

lon <- var.get.nc(ncfile = ncin, variable = 'longitude' )
lat <- var.get.nc(ncfile = ncin, variable = 'latitude' )
capa_mezcla <- var.get.nc(ncfile = ncin, variable = 'mlotst', start = c(1,1,1) )
close.nc(ncin)
############
limites.lon = c(270, 290)-360
limites.lat = c(-20, 2)
indc.lon <- lon >= limites.lon[1] & lon <= limites.lon[2]
indc.lat <- lat >= limites.lat[1] & lat <= limites.lat[2]

lon <- lon[indc.lon]
lat <- lat[indc.lat]
capa_mezcla <- capa_mezcla[indc.lon,indc.lat]

load('costa_Peru_202.RDat')
##################

malla <- expand.grid(lon,lat)

for (ii in 1:12){
  ncin <- open.nc(con = lista_archivos[[ii]])
  capa_mezcla <- var.get.nc(ncfile = ncin,
                            variable = 'mlotst',
                            start = c(1,1,1),
                            unpack = TRUE)
  close.nc(ncin)
  capa_mezcla <- capa_mezcla[indc.lon,indc.lat]
  z <- stack(as.data.frame(capa_mezcla))$values
  
  mapa <- data.frame(x=malla$Var1, y=malla$Var2, z=z)
  #############
  
  paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
  png(width = 1000, height = 900, filename = paste0(raiz,'figuras/capa_mezcla_',mes[ii],'.png'))
  pp <- ggplot(data=mapa, aes(x= x, y=y, fill=z)) 
  pp <- pp + geom_raster()
  pp<- pp + scale_fill_gradientn( colours = paleta_color)
  pp <- pp + scale_x_continuous(limits = c(-90,-70),
                                expand = c(0.01,0))
  pp <- pp + geom_polygon( data=shore,aes(x=long-360,y=lat,group=group),color = 'black', fill = 'grey80',inherit.aes=FALSE  )
  pp <- pp + guides( fill = guide_colorbar(  barheight = unit(10, "cm"),
                                             barwidth = unit(1.5,'cm'),
                                             label.theme = element_text(size=26),
                                             title = 'metros',
                                             title.theme = element_text(size=26)))
  pp <- pp + labs(x='Longitud',y='Latitud',
                  title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                  subtitle = paste0('Profundidad de la capa de mezcla (napa)\npromedio de  ', mes[[ii]]),
                  caption = 'Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 1981-2009')
  pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                    axis.title.y = element_text( size=28,hjust=0.5  ),
                    axis.text.x = element_text(size=24,colour = 'black'),
                    axis.text.y = element_text(size=24,colour = 'black'),
                    title=element_text(size=28),
                    plot.subtitle=element_text(size=38),
                    plot.caption = element_text(size = 22,hjust = 0))
  plot(pp)
  dev.off()
}
