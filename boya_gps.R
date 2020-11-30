cat('\014')
rm(list=ls())
library('maptools')
library(tidyverse)
library(rgdal)
library(readxl)
gpclibPermit()
gshhs.dir <- 'D:/programasR/gshhs/'


carpeta <- 'D:/DHN/Depto_oceanografia/proyectos/'

archivo <- paste0(carpeta,'oleaje.xlsx')

salida <- paste0(carpeta,'gps.kml')

datos <- read_xlsx(archivo,sheet = '2892020',col_names = FALSE)

###############################################


limites.lon <- c(-90, -75)+360
limites.lat <- c(-12.5, -12)

shore <- getRgshhsMap( paste0(gshhs.dir,'gshhs_f.b') ,
                       xlim = limites.lon-360,
                       ylim = limites.lat,avoidGEOS=TRUE,shift=TRUE)
###############


windows() 
plot(shore)  
points(datos[[5]],datos[[4]])

################
x <- datos[[5]]
y <- datos[[4]]
datosKML <- SpatialPointsDataFrame(coords = cbind(x,y),
                                   data = datos[,1],
                                   proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84'))


writeOGR(datosKML, dsn=salida, layer= salida, driver="KML", dataset_options=c("NameField=name"))


###########################
fsize = 2
graphics.off()
png(width=1200,height=800,filename = paste0(carpeta,'acelerometro.png'))
plot(datos[[1]],datos[[12]],type='l',col='blue',
     ylim=c(-10,14),main='Acelerómetro',xlab='Tiempo',ylab='m/s^2',
     cex.lab=fsize,cex.axis=0.75*fsize)
lines(datos[[1]],datos[[13]],type='l',col='red')
lines(datos[[1]],datos[[14]],type='l',col='black')
dev.off()


png(width=1200,height=800,filename = paste0(carpeta,'giroscopio.png'))
plot(datos[[1]],datos[[15]],type='l',col='blue',
     ylim=c(-2,2),main='Giroscopio',xlab='Tiempo',ylab='grad/S',
     cex.lab=fsize,cex.axis=0.75*fsize)
lines(datos[[1]],datos[[16]],type='l',col='red')
lines(datos[[1]],datos[[17]],type='l',col='black')
dev.off()

png(width=1200,height=800,filename = paste0(carpeta,'magnetometro.png'))
plot(datos[[1]],datos[[18]],type='l',col='blue',ylim=c(-30,60),
     main='Magnetómetro',xlab='Tiempo',ylab='uT',cex.lab=fsize,cex.axis=0.75*fsize)
lines(datos[[1]],datos[[19]],type='l',col='red')
lines(datos[[1]],datos[[20]],type='l',col='black')
dev.off()

##############################################

dT <- diff(datos[[1]])

indc <-  which( dT>20 )

dT <- dT[-indc]

dT <- as.numeric(mean(dT))

y <- 0.5*(datos[[12]] - 9.8 )


windows(width = 1200,height=800)
boxplot(y)

windows(width = 1200,height=800)
plot(y,type='l')

#########################

y <- 0.5*(datos[[14]] )

windows(width = 1200,height=800)
boxplot(y)

windows(width = 1200,height=800)
plot(y,type='l')
