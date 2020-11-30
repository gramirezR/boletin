## PROCESAMIENTO DE TEMPERATURA SUPERFICIAL DEL MAR DE LA BASE DE DATOS COPERNICO
#
#

cat("\014") 
graphics.off()
rm(list=ls())

# library('RNetCDF')
setwd('E:/programasR/boletin/')
library('stringr')
library('tidyverse')
library('scales')
library('directlabels')
library('maptools')
library('reshape2')
source('bajarCopernico.R')
source('boletin_listaArchivos.R')
###############CARPETAS###############

#raiz <- 'C:/Users/gramirez/ANM/boletin/'
raiz <- 'E:/boletin/datos/'
#setwd('C:/Users/gramirez/programasR/boletin/')
gshhs.dir <- 'E:/programasR/gshhg-bin-2.3.7/'
#      raiz <- 'C:/Users/Gerardo/Almacen/DHN/Depto_oceanografia/boletin/'
# gshhs.dir <-'C:/Users/Gerardo/Almacen/paquetesR/'
#############DEFINICION DE FECHAS##################

t0.copernico <- as.Date('1981-01-01') # segundos desde esta fecha

# dias.atras <- lubridate::yday(lubridate::today() )


fecha.actual <- lubridate::today() - 1
fecha.referencia <- as.Date('2020-11-01')
dias.atras.hov <- 30*6 + 1 


#for (ii in seq(from=as.Date('2019-07-02'),to=as.Date('2019-07-31'),by='day')){
#  fecha.actual <- as.Date(ii,origin='1970-01-01')  #as.Date('2019-07-02')

  dias <- seq(from=fecha.referencia,to=lubridate::today(),by='7 days')
 periodo <- 'semanal'
 for (ii in 1:( length(dias) - 1 ) ){


 fecha.referencia <- dias[[ii]]
     fecha.actual <- dias[[ii+1]]


dias.atras <- as.numeric(fecha.actual - fecha.referencia)

#dias.atras <- lubridate::day( fecha.actual) 

fecha.anterior <- fecha.actual - dias.atras   #fecha.actual - dias.atras # en días


###### BAJAR DATOS DE COPERNICO ########

prueba <- FALSE

if (prueba){
  load('lista_archivos.Rdata')
}else{
  salida <- boletin_listaArchivos(       ruta.ftp = 'ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001/METOFFICE-GLO-SST-L4-NRT-OBS-ANOM-V2/'
                                  ,   ruta.salida = raiz
                                  ,  fecha.actual = fecha.actual
                                  ,fecha.anterior = fecha.anterior
                                  ,  cuantos.dias = dias.atras
                                  ,      variable = 'temp')
  save(file = 'lista_archivos.Rdata','salida')
}
archivos <- salida[[1]]
boletin.dir <- salida[[2]]
rm(salida)
#archivo.hoy <- archivos
######## MAPA COSTERO DE TEMPERATURA MENSUAL ######
source('boletin_mapa_costero_temp.R')
lim.lon <- c(270, 290)
lim.lat <- c(-20, 2)
# archivo.hoy <- last(archivos)

mapa_costero <- mapa_costero_temp(archivos,'analysed_sst',
                                  lim.lon,lim.lat,
                                  fecha.anterior,fecha.actual)

png(filename = paste0(boletin.dir,'mapa_costero_tsm_',periodo, fecha.actual ,'.png'),
    width = 1200,
    height = 1200)
plot(mapa_costero)
dev.off()

######## MAPA COSTERO DE ANOMALÍA DE TEMPERATURA MENSUAL#######
source('boletin_mapa_costero_temp.R')
lim.lon <- c(270, 290)
lim.lat <- c(-20, 2)
mapa_costero <- mapa_costero_temp(archivos,'sst_anomaly',
                                  lim.lon,lim.lat,
                                  fecha.anterior,fecha.actual)

png(filename = paste0(boletin.dir,'mapa_costero_atsm_',periodo, fecha.actual ,'.png'),
    width=1200,height=1200)
plot(mapa_costero)
dev.off()

####### MAPA ECUATORIAL DE ANOMALÍA DE TEMPERATURA MENSUAL #######
source('boletin_mapa_costero_temp.R')
if (exists('mapa_costero')) rm(mapa_costero)
lim.lon <- c(120,290)
lim.lat <- c(-25,25)
mapa_costero <- mapa_costero_temp(archivos,'sst_anomaly',
                                  lim.lon,lim.lat,fecha.anterior,fecha.actual)

png(filename = paste0(boletin.dir,'mapa_ecua_atsm_',periodo, fecha.actual ,'.png'),
    width=2400,height=850)
plot(mapa_costero)
dev.off()
}
   
############ POLÍGONO PARA HOVMOLLER COSTEROS ###############

poli50 <- matrix(c(-78.58496522661444,1.021363921916344,
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

poliRegional <- matrix(c(-79.25718437360111,-0.02045551295067481,
                         -75.95669367603105,4.928730924427694,
                         -79.26336696672081,4.971155918812383,
                         -82.20427179072144,-0.7759698639260368,
                         -82.28927994362095,-3.054822981610691,
                         -82.37564729008081,-5.937559641958849,
                         -80.35794039920143,-8.551969129339305,
                         -78.36022492256039,-11.52384654193936,
                         -75.71854169769844,-15.67331407404462,
                         -72.57379780920375,-17.38935314706341,
                         -70.75018563039053,-18.57414925303788,
                         -70.88292981921209,-23.41073330082782,
                         -70.87535861206111,-23.86352466378628,
                         -70.85974510434868,-24.27403465011905,
                         -70.84678327205114,-25.29157590204255,
                         -69.52919730311162,-25.31071697617294,
                         -69.72127732538453,-18.39589981860917,
                         -74.2641217978654,-14.88617029089832,
                         -78.52178535779115,-8.578493979544939,
                         -80.064007824725,-5.848721764356786,
                         -80.58392170646887,-3.792195105928715,
                         -79.53786092374951,-3.37457833468202,
                         -79.35761075502703,-2.326805210602894,
                         -80.39764235147892,-1.988502085098738,
                         -79.25718437360111,-0.02045551295067481),nrow=25,ncol=2,byrow=TRUE)
###################################
# fecha.actual <- as.Date('2017-04-01')
dias.atras.hov <- 30*6 + 1 
fecha.anterior <- fecha.actual - dias.atras.hov  # en días
# dias.atras.hov <- as.numeric(fecha.actual-fecha.anterior)
# dias.atras <- dias.atras.hov

prueba <- FALSE
if (prueba){
  load('lista_archivos.Rdata')
}else{
  salida <- boletin_listaArchivos(        ruta.ftp ='ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001/METOFFICE-GLO-SST-L4-NRT-OBS-ANOM-V2/'
                                   ,   ruta.salida = raiz
                                   ,  fecha.actual = fecha.actual -1
                                   ,fecha.anterior = fecha.anterior
                                  ,  cuantos.dias = dias.atras.hov
                                   ,      variable = 'temp')
  save(file='lista_archivos.Rdata','salida')
}
archivos <- salida[[1]]
boletin.dir <- salida[[2]]
rm('salida')
archivo.hoy <- archivos

####### HOVMOLLER COSTERO DE DE ANOMALÍA TEMPERATURA ############
source('boletin_hovmoller_costero_temp.R') 

lim.lat <- c(-20, 2)
hovmoller_costero_atsm <- hovmoller_costero(lista=archivos,poligono=poli50,'sst_anomaly',lim.lat,'50')

png(filename = paste0(boletin.dir,'hovmoller_costero_atsm',fecha.actual,'.png'),
    width=1600,height=1000)
plot(hovmoller_costero_atsm)
dev.off()
###### HOVMOLLER COSTERO DE TEMPERATURA ######
source('boletin_hovmoller_costero_temp.R')
hovmoller_costero_tsm <- hovmoller_costero(archivos,poli50,'analysed_sst',lim.lat,'50')

png(filename = paste0(boletin.dir,'hovmoller_costero_TSM_',fecha.actual,'.png'),
    width=1600,height=1000)
plot(hovmoller_costero_tsm)
dev.off()
#####################################################
T <- as.numeric(levels(factor(hovmoller_costero_tsm$data$tiempo)))

tiempos <- split(hovmoller_costero_tsm$data,
                 cut(hovmoller_costero_tsm$data$tiempo,breaks = as.numeric(levels(factor(hovmoller_costero_tsm$data$tiempo)))))


promedios <- lapply(tiempos,function(x){
  mean(x$atsm)})

promedios <- do.call(what = rbind,args = promedios)

promedios <- data.frame(T=T[-1],atsm = promedios )

promedios$T <- as.POSIXct(promedios$T,origin='1981-01-01')

png(filename = paste0(boletin.dir,'promedio_tsm_50millas_',fecha.actual,'.png'),
    width=1600,height=850)
pp <- ggplot(data=promedios,aes(x=T,y=atsm))
pp <- pp + geom_line(col='blue',size=2)
pp <- pp + scale_x_datetime(date_breaks = 'month',date_labels = '%b-%Y' )
pp <- pp + scale_y_continuous(limits = c(16,32) )
pp <- pp + labs(x='Fecha',y='°C',
                title='Promedio de la TSM dentro de las 50 millas',
                caption='Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0)\nClimatología: 1981-2009')
pp <- pp + theme(axis.text.x = element_text( size = 24,color='black',angle=90 ),
                 axis.text.y = element_text( size = 34,color='black' ),
                 axis.title.x = element_text( size = 40 ),
                 axis.title.y = element_text( size = 40 ),
                 title = element_text( size = 36 ),
                 plot.subtitle = element_text(size = 28),
                 plot.caption = element_text( size = 28,hjust = 0))

plot(pp)
dev.off()

save(file=paste0(boletin.dir,'indice_T_02.RData'),list = 'promedios')


############ HOVMOLLER ENTRE 50-200 MILLAS ########
source('boletin_hovmoller_costero_temp.R')
load('E:/programasR/boletin/peru_poli_ENFEN.RData')# variable costa

poli50_200 <- costa@polygons[[1]]@Polygons[[1]]@coords
lim.lat <- c(-20, 2)
hovmoller_costero <- hovmoller_costero(archivos,poli50_200,'sst_anomaly',lim.lat,'50 a 200')

png(filename = paste0(boletin.dir,'hovmoller_costero_atsm_50-200_',fecha.actual,'.png'),
    width=1600,height=1000)
plot(hovmoller_costero)
dev.off()
##### HOVMOLLER ECUATORIAL DE ANOMALÍA DE TEMPERATURA ########
limites.lat <- c(-2,2)
limites.lon <- c(140,290)
source('boletin_hovmoller_ecuatorial_temp.R')
hovmoller_ecuatorial <- hovmoller_ecuatorial_temp(archivos,'sst_anomaly',
                                                  limites.lon,limites.lat)

png(file=paste0(boletin.dir, 'hovmoller_ecuatorial_atsm_', 
                lubridate::today(), '.png'  ),
    width=1600,height=950)
plot(hovmoller_ecuatorial)
dev.off()

######################  Índices COSTEROS DE TEMPERATURA ########################

source('boletin_indicesCosteros.R')
indices <- indicesCosteros(hovmoller_costero_tsm,hovmoller_costero_atsm)

###################

# listaPng <- list.files(path = boletin.dir,pattern = '.+\\.png$',full.names = TRUE)
# 
# zip(zipfile = paste0(boletin.dir,'temperatura_semana',sprintf(ii,fmt = '%02d'),'.zip'),
#       files = listaPng)
#   file.remove(listaPng)
# 
# }