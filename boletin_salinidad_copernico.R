#####
cat('\014')
rm(list=ls())
graphics.off()
rm(list=ls())

setwd('E:/programasR/boletin/')
library('RNetCDF')
library('stringr')
library('tidyverse')
library('scales')
library('directlabels')
library('maptools')
library('reshape2')
library('metR')


# source('bajarCopernico.R')
source('boletin_lista_salinidad.R',encoding = 'UTF-8')
source('boletin_mapa_costero_salinidad.R',encoding = 'UTF-8')
raiz <- 'E:/boletin/salinidad/'
gshhs.dir <- 'E:/programasR/gshhg-bin-2.3.7/'
#############DEFINICION DE FECHAS##################

t0.copernico <- as.Date('1981-01-01') # segundos desde esta fecha
fecha.inicial <- as.Date('2021-11-01')
fecha.final <- lubridate::today() - 2

dias.atras.hov <- 30*13 + 1
periodo <- 'semanal'

if (periodo=='semanal'){
  intervalo <- paste( as.numeric(fecha.final-fecha.inicial)-1,'days' )
  dias <- seq(from=fecha.inicial, to=fecha.final,by=intervalo)  
}else{
  dias <- seq(from=fecha.inicial, to=fecha.final,by='7 days')
  if (as.numeric(fecha.final-fecha.inicial)%%7>0){
    dias <- c(dias, fecha.final)
  }
}
 

for (ii in 1:(length(dias)-1)){
  lista_archivos <- boletin_lista_salinidad(seq(dias[ii],dias[ii+1], by='day'),raiz)
  row.names(lista_archivos) <- NULL 
  boletin_mapa_costero_salinidad(lista_archivos,dias[ii],dias[ii+1]) 
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

#############################################################

lim.lat <- c(-20, 2)

dias <- seq(from=lubridate::today() - dias.atras.hov, to=fecha.final,by='day')

lista_archivos <- boletin_lista_salinidad(dias,raiz)
##################################
source('boletin_hovmoller_costero_salinidad.R',encoding = 'UTF-8')
hovmoller_costero_salinidad(lista_archivos, poli50,lim.lat,50)

