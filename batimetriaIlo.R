cat('\014')
library('raster')
library('tidyverse')
library('oce')
library('ggquiver')
library('gstat')  
library('rgdal')
library('sp')
setwd('D:/programasR/boletin/')
rm(list=ls())
graphics.off()

carpeta <- 'D:/DHN/Depto_oceanografia/ILO/yessica/CD BATY/DATA ILO/ilo-info Macha/Sort/'

archivos <- list.files(path = carpeta,pattern='.+\\.xyz$',full.names = TRUE)

narch <- length(archivos)

 datos <- vector(mode='list',length = 1)
##################
cual.archivo <- archivos[1]


print(paste0(cual.archivo,'  ',file.size(cual.archivo)))

salida <- 'batimetria_final_18.RDS'

#######################
datos <- lapply(cual.archivo,FUN=function(x){
  
  y <- read.csv2(file = x,header = FALSE,sep = ' ',dec = '.')
  
  if(ncol(y)!=3){
      z <- y[,1:3]
      rm(y)
      y <- z
      rm(z)
  }
  
  names(y) <- c('lon','lat','z')
  return(y)
})

head(datos)

datos <- do.call(what = rbind,args = datos)


ndatos <- length(datos$lon)

# indc <- sample.int(n = ndatos, size = floor(0.15*ndatos), replace = TRUE)
# 
# datos <- datos[indc,]
# windows()
# plot(datos$lon,datos$lat)
# Sys.sleep(5)
#######################################

indc <- which( is.na(datos$lon) | is.na(datos$lat) | is.na(datos$z))

if (!is_empty(indc)){
datos <- datos[-indc,]
}

bati.UTM <- SpatialPointsDataFrame(coords = datos[,1:2],data=data.frame(z=datos$z),
                proj4string = CRS('+proj=utm +zone=18L +south +datum=WGS84 +units=m +no_defs +ellps=WGS84'))


bati.latlon <- spTransform(x = bati.UTM,CRSobj = CRS('+proj=longlat'))

limites <-extent(bati.latlon)


#########################

grd   <- as.data.frame(spsample(bati.latlon, "regular", n=2500))

names(grd)       <- c("lon", "lat")
coordinates(grd) <- c("lon", "lat")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

proj4string(grd) <- proj4string(bati.latlon)


vgm1 <- variogram(z~1, bati.latlon)


print('** CALCULANDO VARIOGRAMA **')

parametros <- fit.variogram(vgm1, vgm("Exp"),fit.sills = TRUE,fit.kappa = TRUE)

windows()
plot( vgm1 , parametros )

Sys.sleep(0.5)

variograma <- vgm(psill = parametros$psill[2],model = parametros$model[2],
                  range = parametros$range[2],kappa = parametros$kappa[2])

P.idw <- gstat::krige(formula = z ~ 1, locations = bati.latlon, newdata=grd,model=variograma)

batimetria <- raster(P.idw )

###################
windows()
plot(batimetria)
contour(x = batimetria,nlevels=25,add=TRUE)

############
saveRDS(object = batimetria,file = paste0(carpeta,salida))


