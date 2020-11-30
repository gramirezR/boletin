cat('\014') 
graphics.off()
rm(list=ls())

library('stringr')
library('tidyverse')
library('scales')
library('metR')
library('maptools')
library('reshape2')
library('ggquiver')
library('RCurl')
library('raster')
library('mgcv')
library('rasterVis')
library('RNetCDF')

# El tiempo está en milisegundos DESDE ENERO 1, 1970
setwd('C:/Users/gramirez/programasR/boletin/')
datos.ftp <- 'ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/GLOBAL_ANALYSIS_FORECAST_PHY_001_024/global-analysis-forecast-phy-001-024/'


carpeta.local <- 'C:/Users/gramirez/ANM/boletin/perfilesCOPERNICO/'


t0.copernico <- as.Date('1970-01-01') 

fecha.actual <- lubridate::today()

carpeta.remota <- paste0(datos.ftp,format(fecha.actual,format='%Y/%m/') )

x <- format(fecha.actual,format='%Y%m%d')

#archivo <- paste0('mercatorpsy4v3r1_gl12_mean_',x,'_R',x,'.nc')

archivo <- paste0('mercatorpsy4v3r1_gl12_mean_20190305_R20190313.nc')

bajar <- paste0(carpeta.remota,archivo)

guardar <- paste0(carpeta.local,archivo)

if (!file.exists(guardar)){
download.file(url = bajar, destfile = guardar ,method = "auto",
               quiet = FALSE, mode="wb", cacheOK = TRUE  )}
lat.tr <- -3.50
if (lat.tr<0) {
  hem <-  'S'
}else{ 
  hem <-  'N'
  }
creditos <- 'Operational Mercator global ocean analysis and forecast system y Copernicus Marine Service,
nrt.cmems-du.eu/Core/GLOBAL_ANALYSIS_FORECAST_PHY_001_024/global-analysis-forecast-phy-001-024/
Elaboración: Dpto. Oceanografía, Div. Oceanografía Física'

#########################

lon.lim <- c(-90,-70)
lat.lim <- c(-20,5)

ncId <- open.nc(guardar)

# print.nc(ncId)
     lon <- var.get.nc(ncfile = ncId,variable = 'longitude')
     lat <- var.get.nc(ncfile = ncId,variable = 'latitude')
    prof <- var.get.nc(ncfile = ncId,variable ='depth')
indc.lon <- which(( lon >= lon.lim[1] ) & ( lon <= lon.lim[2] ))
indc.lat <- which(( lat >= lat.lim[1] ) & ( lat <= lat.lim[2] ))

  nprofs <- length(prof)

desplazamiento <- att.get.nc(ncfile = ncId,variable='thetao',attribute = 'add_offset')
        escala <- att.get.nc(ncfile = ncId,variable='thetao',attribute = 'scale_factor')
          temp <- desplazamiento + escala*var.get.nc(ncfile = ncId,variable = 'thetao',
                    start = c(indc.lon[1],indc.lat[1],1,1),
                    count = c(length(indc.lon),length(indc.lat),length(prof),1) )

close.nc(ncId)
########
load('peru_costa_200mi.RData') ## variable costa

malla <- expand.grid(lon[indc.lon]+360,lat[indc.lat])
names(malla) <- c('lon','lat')

#########
poli <- data.frame(costa@polygons[[1]]@Polygons[[1]]@coords)

names(poli) <- c('lon','lat')
######
indc <- point.in.polygon(  malla$lon, malla$lat,
                            poli$lon,  poli$lat )==1
T <- vector(mode='list',length = nprofs)

for (ii in 1:nprofs){
  malla$temp <- (stack( as.data.frame(temp[,,ii]) )$value)
  
  malla$temp[!indc] <- NA
  
  T[[ii]] <- t(matrix(data = malla$temp,
                nrow = length(indc.lon),
                ncol = length(indc.lat)))
  
  T[[ii]] <- flip(raster( T[[ii]], xmn = min(malla$lon),xmx =  max(malla$lon),
                    ymn = min(malla$lat),ymx =  max(malla$lat),
                    crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')),
              direction = 2)
}

T <- raster::stack(T)

########

 tr.ind <- which(malla$lat== lat.tr& indc)
 tr.lon <- malla$lon[tr.ind]
 tr.lat <- malla$lat[tr.ind]
 
 poli.sp <- SpatialPoints( cbind(tr.lon,tr.lat)  )
 proj4string(poli.sp) <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
 
 tr.tem <- matrix(nrow = nprofs,ncol = length(tr.ind))
 tr.lon <- tr.tem
 tr.prof <- tr.tem
 for (ii in 1:nprofs){
    tr.tem[ii,] <-  raster::extract(T,poli.sp,layer=ii,nl=1)
    tr.lon[ii,] <-  poli.sp@coords[,1]
   tr.prof[ii,] <-  rep(prof[ii],length(tr.ind))
 }
 
 X <- stack(as.data.frame(tr.lon))$value
 Y <- stack(as.data.frame(tr.prof))$value
 Z <- stack(as.data.frame(tr.tem))$value
 
 T2 <- as.data.frame(cbind(X,Y,Z))
 
 names(T2) <- c('lon','prof','temp')
 
 ######### INTERPOLACION #########
 
 lon <- seq(from = min(tr.lon),to = max(tr.lon),length.out = 100)
   z <- seq(from = 0  ,to = 200  ,length.out = 200) 
 
  tt <- expand.grid( lon,z )
  names(tt) <- names(T2[,1:2]) 

  modelo <- mgcv::gam(data=T2,temp~te(lon,prof,k=c(15,15) ) )
  
  temp.i <- predict(modelo,newdata=tt)   

      T2 <- cbind(tt,temp.i)
      names(T2) <- c('lon','prof','temp')
  
 ############
 paleta_color <- c( '#f0f0f0','#b4b4b4','#7878f7','#3c3cfb','#6000ff','#0040ff',
                    '#0080ff','#00bfff','#00ffff','#2bffd4','#55ffaa','#80ff80',
                    '#aaff55','#d4ff2b','#ffff00','#ffd400','#ffaa00','#ff8000',
                    '#ff5500','#ff2b00','#ff0000','#d40000','#aa0000','#800000' )
 niveles <- seq(from=0,to=32)

 marcas <- seq(from = ceiling(min(tr.lon)),
                 to = ceiling(max(tr.lon)),by = 0.5)
 
  etiquetas <- paste0(abs(marcas-360),'W')

  png(width=1200,height=850,
      filename =paste0('C:/Users/gramirez/ANM/boletin/temp/2019/figuras/transecto_prueba',
                       'mercator_',abs(lat.tr),'S.png'))
  pp <- ggplot()
  pp <- pp + geom_raster(data=T2,aes(x=lon,y=prof,fill=temp))
  pp <- pp + scale_fill_gradientn(colours = paleta_color,limits=c(0,30))
  pp <- pp + geom_contour(data=T2,aes(x=lon,y=prof,z=temp),
              breaks = niveles,col='black' )
  pp <- pp + geom_text_contour(data=T2,aes(x=lon,y=prof,z=temp),stroke = 0.15,skip=0,min.size =3,
                     size=6,rotate = FALSE,breaks = niveles,check_overlap = TRUE)
  
  pp <- pp + scale_y_reverse(expand=c(0,0))
  pp <- pp + scale_x_continuous(expand = c(0,0),
                                breaks = marcas,
                                 label = etiquetas)
  pp <- pp + theme_bw()
  pp <-  pp + theme( axis.title.x = element_text( size=20,hjust=0.5  ),
                     axis.title.y = element_text( size=20,hjust=0.5  ),
                        axis.text = element_text(size=20),
                            title = element_text(size=26),
                    plot.subtitle = element_text(size=24),
                     plot.caption = element_text(size = 14,hjust = 0))  
  pp <- pp + labs(x = 'Longitud (grads)',y = 'Profundidad (m)',
                  title = paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                                 'Dpto. de Oceanografía - Div. Oceanografía'),
                  subtitle = paste0('Temperatura Potencial: Análisis del ',
                                    format(as.Date(fecha.actual,format='%Y%m%d'),format='%d de %B de %Y'),
                                    '\n transecto de 200 mi. Latitud ',abs(lat.tr),'S'),
                  caption = creditos )
  pp <- pp + guides( fill = guide_colorbar(barheight = unit(20, "cm"),
                                            barwidth = unit(1,'cm'),
                                         label.theme = element_text(size=18),
                                               title = '°C',title.theme = element_text(size=18)))  
 plot(pp)
dev.off()


############ ANOMALIA ###############


