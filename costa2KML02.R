cat('\014') 
graphics.off()
rm(list=ls())

library(maptools)
library(rgdal)
library('splines')
library('ggplot2')
graphics.off()
setwd("D:/programasR/")
load('costa_viento.RDat')

#################

# windows()
# pp <- ggplot(data=shore,aes(x=long,y=lat,group=group))
# pp <- pp + geom_point()
# plot(pp)
# 
#################
kml.arch <- "peru_poli_ENFEN.kml"
rdata.arch <- "peru_poli_ENFEN.RData"

ancho.ini <- 0/60 # EN GRADOS: 1 MINUTO = 1 MiNautica
ancho.fin <- 0/60

dlat <- diff(range(shore$lat))

mpol <- abs(ancho.fin - ancho.ini)/dlat

if (exists('costa'))rm(costa)
########### 
graphics.off()
costa.tmp1 <- shore[shore$group==2.1,]
costa.tmp2 <- shore[shore$group==3.1,]

# windows()
# pp <- ggplot(data=costa.tmp1,aes(x=long,y=lat))
# pp <- pp + geom_point()
# plot(pp)

costa.tmp1 <- costa.tmp1[1100:29870,]
costa.tmp2 <- costa.tmp2[1:83000,]
windows()
pp <- ggplot(data=costa.tmp1,aes(x=long,y=lat))
pp <- pp + geom_point()
plot(pp)
##############
costa.tmp1 <- costa.tmp1[,c(1,2)]
costa.tmp1$long <- rev(costa.tmp1$long)
costa.tmp1$lat <- rev(costa.tmp1$lat)

costa.tmp2 <- costa.tmp2[,c(1,2)]
costa.tmp2$long <- rev(costa.tmp2$long)
costa.tmp2$lat <- rev(costa.tmp2$lat)

costa.tmp <- rbind(costa.tmp1,costa.tmp2)

# windows()
# pp <- ggplot(data=costa.tmp,aes(x=long,y=lat))
# pp <- pp + geom_point()
# plot(pp)
##################### se reduce en numero de datos ###########
Pol1 <- costa.tmp[seq(from=1,to=dim(costa.tmp)[1],by=400),c(1,2)]

colnames(Pol1) <- c('lon','lat')
# windows()
# pp <- ggplot(data=as.data.frame(Pol1),aes(x=lon,y=lat))
# pp <- pp + geom_point()
# plot(pp)
############### SUAVIZADO DE LA LINEA DE COSTA ##############

y <- seq(from=-45,to=12.3,by=0.05)
suave1 <- smooth.spline( Pol1$lat,Pol1$lon,df=35 )
df.2 <- predict(suave1,newdata = y)
names(df.2) <- c('lat','lon')
# windows()
# plot(df.2$lon,df.2$lat)
# lines(Pol1$lon,Pol1$lat)

############# se crea el poligono #####

ancho.pol <- ancho.ini - mpol*(df.2$lat-min(df.2$lat))
Pol2 <- as.data.frame(df.2)
Pol2$lon <- df.2$lon - rev(ancho.pol)

#indc <- 250:550
#Pol2 <- Pol2[-indc,]
windows()
plot(Pol1$lon,Pol1$lat)
lines(Pol2$lon,Pol2$lat)
####### UNION DE LOS POLIGONOS #############

costa.df <- rbind(Pol1,Pol2)
costa <- Polygon(costa.df)
# 
costa <- Polygons(list(costa),'lc50m')
# 
costa <- SpatialPolygons(list(costa))
# 
pid <- sapply(slot(costa, "polygons"), function(x) slot(x, "ID"))
# 
p.df <- data.frame( ID=1:length(costa), row.names = pid)
# 
costa <- SpatialPolygonsDataFrame(costa, p.df)
# 
# 
# 
windows()
plot(costa)
# 
# ###############
# 
proj4string(costa) <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
# 
if (file.exists(kml.arch)) file.remove(kml.arch)
# 
writeOGR(costa, dsn=kml.arch, layer= kml.arch, driver="KML", dataset_options=c("NameField=name"))
# 

save(file=rdata.arch,costa)
# 
######
campoNormal = data.frame(x = Pol2[-1,2],y=Pol2[-1,1],ux = -diff(Pol2[,1])/0.5,uy=diff(Pol2[,2])/0.5)
mag <- sqrt(campoNormal$ux^2+campoNormal$uy^2)
campoNormal$ux <- campoNormal$ux/mag
campoNormal$uy <- campoNormal$uy/mag

windows(width=350,height=750)
pp <- ggplot(data=campoNormal,aes(x= x,y=y,u= 1000*ux,v=1000*uy))
pp <- pp + geom_point()
pp <- pp + ggquiver::geom_quiver()
plot(pp)

#####

distancia <- 200/60
puntos200 <- Pol2[-1,c('lon','lat')] + distancia*campoNormal[,3:4]
distancia <- 50/60
puntos50 <- Pol2[-1,c('lon','lat')] + distancia*campoNormal[,3:4]

windows(width=350,height=750)
pp <- ggplot(data=puntos200,aes(x= lon,y=lat))
pp <- pp + geom_point(col='black')
pp <- pp + geom_point(data=puntos50,aes(x=lon,y=lat),inherit.aes = FALSE,col='red')
plot(pp)
########

suave1 <- smooth.spline( puntos200$lat,puntos200$lon,df=25 )
df.2 <- as.data.frame(predict(suave1,newdata = y))
names(df.2) <- c('lat','lon')

windows(width=350,height=750)
pp <- ggplot(data=df.2,aes(x= lon,y=lat))
pp <- pp + geom_point(col='black')
pp <- pp + geom_point(data=puntos200,aes(x=lon,y=lat),inherit.aes = FALSE,col='red')
plot(pp)


df.2$lat <- rev(df.2$lat)
df.2$lon <- rev(df.2$lon)
costa.df <- rbind(puntos50,df.2)

costa <- Polygon(costa.df)
# 
costa <- Polygons(list(costa),'lc50m')
# 
costa <- SpatialPolygons(list(costa))
# 
pid <- sapply(slot(costa, "polygons"), function(x) slot(x, "ID"))
# 
p.df <- data.frame( ID=1:length(costa), row.names = pid)
# 
costa <- SpatialPolygonsDataFrame(costa, p.df)
# 
# 
# 
windows()
plot(costa)

proj4string(costa) <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
# 
if (file.exists(kml.arch)) file.remove(kml.arch)
# 
writeOGR(costa, dsn=kml.arch, layer=kml.arch, driver="KML", dataset_options=c("NameField=name"))
# 
save(file=rdata.arch,costa)