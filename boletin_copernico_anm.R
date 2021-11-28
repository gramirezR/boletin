## PROCESAMIENTO DE ANOMAL?A DE NIVEL DE MAR DE COPERNICO
#
#

cat("\014") 
rm(list=ls())
setwd('E:/programasR/boletin/')
graphics.off()
library('RNetCDF')
library('stringr')
library('tidyverse')
library('scales')
library('directlabels')
library('maptools')
library('reshape2')
# library('RDCOMClient')
library('mgcv')
source('bajarCopernico.R')
source('boletin_mapa_costero_anm.R')
source('boletin_hovmoller_ecuatorial_anm.R')
source('boletin_estaciones_anm.R')
###############CARPETAS###############

     raiz <- 'E:/boletin/'
gshhs.dir <- 'E:/programasR/gshhg-bin-2.3.7/'
servidores <- list( 'ftp://gramirez2:$boletinDHN2018@my.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047/dataset-duacs-rep-global-merged-allsat-phy-l4/',
                  'ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L4_NRT_OBSERVATIONS_008_046/dataset-duacs-nrt-global-merged-allsat-phy-l4/')
servidor <- servidores[[2]]
#############DEFINICION DE FECHAS##################

 t0.copernico <- as.Date('1981-01-01') # segundos desde esta fecha

       # hoy <- lubridate::today() 
       # 
       # dia.uno <- lubridate::yday(hoy)
 fecha.inicial <- as.Date('2021-11-01') 
 fecha.final <- lubridate::today() - 2

 dias.atras.hov <- 30*12 + 2 
 
periodo <- 'mensual'

if (periodo == 'semanal'){
   dias <- seq(from=fecha.inicial,to=fecha.final,by='7 days')
   if (as.numeric(fecha.final-fecha.inicial)%%7>0){
      dias <- c(dias, fecha.final)
   }
}else{
   intervalo <- paste( as.numeric(fecha.final-fecha.inicial) ,'days')
   dias <- seq(from=fecha.inicial,to=fecha.final,by=intervalo)
}

############ POLÍGONO PARA GRÁFICOS COSTEROS ###############

# load('E:/programasR/boletin/peru_poli_ENFEN.RData')
# 
# poli50 <- costa@polygons[[1]]@Polygons[[1]]@coords
# 
# poli50 <- as.data.frame(poli50)
# 
# names(poli50) <- c('lon','lat')
# 
##########################################
if(1){
for (ii in 1:( length(dias) - 1 ) ){
  
   if (exists(x = 'fecha.anterior')) rm(fecha.anterior)
   if (exists(x = 'fecha.actual')) rm(fecha.actual)
   if (exists('pp')) rm(pp)
   fecha.anterior <- dias[[ii]]
   fecha.actual <- dias[[ii+1]]

   dias.atras <- fecha.actual - fecha.anterior
  
fecha.anterior <- fecha.actual - dias.atras # en d?as

###### BAJAR DATOS DE COPERNICO ########
prueba <- FALSE
if (prueba){
  load('lista_anm.Rdata')
}else{
salida <- bajarCopernico(         ruta.ftp = servidor
                           ,   ruta.salida = raiz
                           ,  fecha.actual = fecha.actual
                           ,fecha.anterior = fecha.anterior
                           ,  cuantos.dias = dias.atras
                           ,      variable = 'anm')

   archivos <- salida[[1]]
boletin.dir <- salida[[2]]
rm(salida)
save(file='lista_anm.Rdata',list=c
     ('archivos','boletin.dir'))
}

####### LISTA PARA GENERAR GR?FICOS DE FECHAS ANTERIORES #######

dias.archivo <- sort( unique(unlist(
                      lapply(archivos,function(x) {
                         donde <- regexpr(text = x,pattern = '_[0-9]{8}_',)[1]
                         format(as.Date(substring(x,donde+1,donde+8),format='%Y%m%d'),'%Y%m%d')}  )
                      )))
  indc <- which( as.Date(dias.archivo,origin='1970-01-01',format='%Y%m%d') > fecha.actual )
  if (length(indc)>0){
    archivos2 <- archivos[-indc]
    narchs <- length(archivos2)
  }else{
    archivos2 <- archivos
    narchs <- length(archivos2)
  }
  # dias.atras <- dia.uno
  cuales.archivos <- seq( from = narchs , to=(narchs - as.numeric(dias.atras)+1) , by=-1   )
  # cuales.archivos <- seq( from = narchs , to=1 , by=-1   )
  indc <- which(cuales.archivos<=0)
  if (length(indc)>0){
  cuales.archivos <- cuales.archivos[-indc]}
     lista.sinRep <- sort(unique(rev(archivos2[cuales.archivos])))
   
 
######## MAPA COSTERO MENSUAL########
print('Elaborando Mapa Costero')     
source('E:/programasR/boletin/boletin_mapa_costero_anm_raster.R')
mapa_costero <- boletin_mapa_costero_anm_raster(lista = lista.sinRep,
                                         limites.lon = c(270, 290),
                                         limites.lat = c(-20, 2),
                                         fecha.anterior,
                                         fecha.actual)

png(filename = paste0(boletin.dir,'mapa_costero_anm_',periodo,fecha.actual,'.png'),width=950,height=1200)
plot(mapa_costero)
dev.off()
############ MAPA ECUATORIAL MENSUAL########################
print('Elaborando Mapa Ecuatorial')
source('E:/programasR/boletin/boletin_mapa_costero_anm_raster.R')
mapa_costero <- boletin_mapa_costero_anm_raster(lista = lista.sinRep,
                                         limites.lon = c(120,290),
                                         limites.lat = c(-25, 25),
                                         fecha.anterior,
                                         fecha.actual)

png(filename = paste0(boletin.dir,'mapa_ecuatorial_anm_',periodo,fecha.actual,'.png'),width=2400,height=850)
plot(mapa_costero)
dev.off()

###################
}}

t0.copernico <- as.Date('1981-01-01') # segundos desde esta fecha


fecha.anterior <- fecha.final - dias.atras.hov

#############################################

 if(periodo=='mensual'){
    prueba <- FALSE
 if (prueba){
   load('lista_anm.Rdata')
 }else{
    salida <- bajarCopernico(ruta.ftp= servidor
                            ,   ruta.salida = raiz
                            ,  fecha.actual = fecha.final
                            ,fecha.anterior = fecha.anterior
                            ,  cuantos.dias = dias.atras.hov
                            ,      variable = 'anm')

   archivos <- salida[[1]]
   boletin.dir <- salida[[2]]
   rm(salida)
   save(file='lista_anm.Rdata',list=c('archivos','boletin.dir'))
 } 
 #############################

 dias.archivo <- sort( unique(unlist(
   lapply(archivos,function(x) format(as.Date(substring(x,57,65),format='%Y%m%d'),'%Y%m%d')  )
 )))
 # indc <- which( as.POSIXct(dias.archivo,origin='1970-01-01',format='%Y%m%d') > as.POSIXct(fecha.actual,origin='1970-01-01',format='%Y-%m-%d') )
 # 
 # if (length(indc)>0){
 #   archivos2 <- archivos[-indc]
 #   narchs <- length(archivos2)
 # }else{
 #   archivos2 <- archivos
 #   narchs <- length(archivos2)
 # }
 archivos2 <- archivos

 # cuales.archivos <- cuales.archivos <- seq( from = narchs , to=(narchs - as.numeric(dias.atras.hov)+1) , by=-1   )
 # indc <- which(cuales.archivos<=0)
 # if (length(indc)>0){
 #   cuales.archivos <- cuales.archivos[-indc]
 #   }
 # lista.sinRep <- sort(unique(rev(archivos2[cuales.archivos])))
 lista.sinRep <- archivos
 #########  SERIES DE TIEMPO DE ANOMALIA DEL NIvEL DEL MAR DE ISLA BALTA, TALARA Y CHIMBOTE #####
 if(1){
   print('Elaborando Estaciones')   
 source('E:/programasR/boletin/boletin_estaciones_anm_raster.R', encoding = 'UTF-8')
 posicion <- data.frame(    balta = c(-90.283333+360,-0.433333),
                            talara = c(-81.283+360,-4.617),
                            chimbote = c(-78.633+360,-9.083),
                            mollendo = c(-72.017964+360,-17.035183))
 
 estaciones.anm <- boletin_estaciones_anm_raster(lista=lista.sinRep,
                                                 posicion,
                                                 fecha.anterior,
                                                 fecha.final)
 
 png(file=paste0(boletin.dir,'estaciones_anm_',lubridate::today(), '.png'),
     width=1200,height=650)
 
 plot(estaciones.anm)
 dev.off()
   }
 ####### HOVMOLLER ECUATORIAL #######
 # fecha.actual <- fecha.final
 print('Elaborando hovmoller ecuatorial')
 source('E:/programasR/boletin/boletin_hovmoller_ecuatorial_anm_raster_discreto.R', 
        encoding = 'UTF-8')
 hovmoller.ecuatorial <- boletin_hovmoller_ecuatorial_anm_raster_discreto(lista =  lista.sinRep,
                                                      limite.lon = c(140,280),
                                                      limite.lat = c(-2,2),
                                                      fecha.anterior,
                                                      fecha.final,
                                                      filtro =1)

png(file=paste0(boletin.dir, 'hovmoller_anm_ecuatorial_', fecha.anterior, '_discreto.png'  )
     ,width=1600,height=950)
 plot(hovmoller.ecuatorial)
 graphics.off()
######## HOVMOLLER COSTERO 50 millas#######
 print('Elaborando Hovmoller 50 millas')
 load('E:/programasR/peru_poli__5N_35S.RData')# variable costa
 poli50 <- costa@polygons[[1]]@Polygons[[1]]@coords
 
 poli50 <- as.data.frame(poli50, row.names = FALSE)
 
 names(poli50) <- c('lon','lat')
 source('E:/programasR/boletin/boletin_hovmoller_anm_raster_discreto.R',
        encoding = 'utf-8' )
 filtro = 1
 if(filtro){
   nombre_filtro <- '_filtro'  
 }else{
   nombre_filtro <- '_sin_filtro'
 }
 hovmoller_costero_50 <- boletin_hovmoller_anm_raster_discreto(lista = lista.sinRep,
                                               poligono = poli50,
                                             limite.lon = c(-90, -70),
                                              limite.lat = c(-30, 2),
                                                  fecha.anterior,
                                             fecha.final,'50',
                                             filtro = filtro)
 
 png(filename = paste0(boletin.dir,
                       'Hovmoller_anomalia_anm_',
                       fecha.actual,nombre_filtro,
                       '_discreto.png'),width=2400,height=1200)
 plot(hovmoller_costero_50)
 dev.off()
############ HOVMOLLER 50 - 200 MILLAS ###############
 print('Elaborando Hovmoller 50-200 millas')
 source('E:/programasR/boletin/boletin_hovmoller_anm_raster.R',
        encoding = 'utf-8' )
 load('E:/programasR/boletin/peru_poli_ENFEN_p.RData')# variable costa

 poli50_200 <- costa@polygons[[1]]@Polygons[[1]]@coords
 hovmoller.costero_50_200 <- boletin_hovmoller_anm_raster(      lista = lista.sinRep,
                                                  poligono = poli50_200,
                                                  limite.lon = c(-90, -70),
                                                  limite.lat = c(-30, 2),
                                                  fecha.anterior,
                                                  fecha.final,'50 A 200',
                                                  filtro = 1)

 png(filename = paste0(boletin.dir,'Hovmoller_anomalia_anm_50-200_',fecha.actual,'_filtro.png'),width=2400,height=1200)
 plot(hovmoller.costero_50_200)
 dev.off()
 #################
}
 # listaPng <- list.files(path = boletin.dir,pattern = '.+\\.png$',full.names = TRUE)
 # 
 # zip(zipfile = paste0(boletin.dir,'anm_semana',sprintf(ii,fmt = '%02d'),'.zip'),
 #     files = listaPng)
 # file.remove(listaPng)
 # 
 #}