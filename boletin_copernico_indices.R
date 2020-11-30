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
source('bajarCopernico_indc.R')

###############CARPETAS###############

#raiz <- 'C:/Users/gramirez/ANM/boletin/'
raiz <- 'D:/boletin/datos/'
#setwd('C:/Users/gramirez/programasR/boletin/')
gshhs.dir <- 'D:/programasR/gshhg-bin-2.3.7/'
#      raiz <- 'C:/Users/Gerardo/Almacen/DHN/Depto_oceanografia/boletin/'
# gshhs.dir <-'C:/Users/Gerardo/Almacen/paquetesR/'
#############DEFINICION DE FECHAS##################

t0.copernico <- as.Date('1981-01-01') # segundos desde esta fecha

# dias.atras <- lubridate::yday(lubridate::today() )
#fecha.actual <- lubridate::today()-1
#for (ii in seq(from=as.Date('2019-07-02'),to=as.Date('2019-07-31'),by='day')){
#  fecha.actual <- as.Date(ii,origin='1970-01-01')  #as.Date('2019-07-02')
fecha.actual <- lubridate::today()
dia <- lubridate::day(fecha.actual)
dias.atras <- dia-1
dias.atras.hov <- 177
#dias.atras <- lubridate::day( fecha.actual) 

fecha.anterior <- fecha.actual - dias.atras # en días


###### BAJAR DATOS DE COPERNICO ########

prueba <- FALSE
diario <- FALSE

if (prueba){
  load('lista_archivos.Rdata')
}else{
  salida <- bajarCopernico_indc(       ruta.ftp = 'ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/INSITU_GLO_TS_OA_NRT_OBSERVATIONS_013_002_a/CORIOLIS-GLOBAL-NRTOA-OBS_TIME_SERIE'
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

