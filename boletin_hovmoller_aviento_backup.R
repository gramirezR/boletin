cat("\014") 
graphics.off()
rm(list=ls())
library('RCurl')
library('ncdf4')
library('stringr')
library('tidyverse')
library('scales')
library('directlabels')
library('maptools')
library('reshape2')
# library('RDCOMClient')
library('mgcv')
require('metR')
library('ggquiver')
# source('bajarCopernico.R')

###############CARPETAS###############
raiz <- 'D:/boletin/'
setwd(raiz)
gshhs.dir <- 'D:/programasR/gshhg-bin-2.3.7/'

#############DEFINICION DE FECHAS##################

dias.atras <- 190
fecha.actual <- lubridate::today() -2

fecha.anterior <- fecha.actual - dias.atras # en días
prueba <- FALSE

lista.dias <- seq(from=fecha.anterior,to=fecha.actual,by='day')
lista.dias <- gsub(lista.dias,pattern = '-',replacement = '/')
dias.arch <- gsub(lista.dias,pattern = '/',replacement = '')
fecha.titulo <- gsub(lista.dias,pattern = '/',replacement = '') 
datos.dir <- paste0('D:/boletin/viento/',lubridate::year(fecha.actual),'/datos/')
anio <- lubridate::year(lista.dias)
mes <- formatC(lubridate::month(lista.dias),width=2,flag=0)
fechas <- unique(paste0(anio,'/',mes))
figuras <- paste0('D:/boletin/viento/',lubridate::year(fecha.actual),'/figuras/')

if (!dir.exists(datos.dir )){
  dir.create(datos.dir ,recursive = TRUE)
}

# ftp.dir <- 'ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/MWF/L3/ASCAT/Daily/Netcdf/'

ftp.dir <- paste0('ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/WIND_GLO_WIND_L4_NRT_OBSERVATIONS_012_004/CERSAT-GLO-BLENDED_WIND_L4-V6-OBS_FULL_TIME_SERIE/')

lista.carpetas <- paste0(ftp.dir,fechas,'/')

archs.bajar <- lapply(lista.carpetas,function(x) paste0(x,unlist(strsplit(getURL(paste0(x,'*.nc'),dirlistonly = TRUE,verbose=TRUE),'\r\n'))))
archs.bajar <- unlist(archs.bajar)
closeAllConnections()
nletras <- str_count(archs.bajar[1])

lapply(archs.bajar , function (x){
  arch.sal <- paste0(datos.dir,
                     str_sub(string=x,start=154,end=nletras ))
  if (!file.exists(arch.sal)){
    download.file( url=x, destfile=arch.sal,
                   method = "auto",quiet = FALSE, mode="wb", cacheOK = TRUE  )
    closeAllConnections()}
})



lista.archs <- lapply(archs.bajar , function (y){
  arch.sal <- paste0(datos.dir,
                     substr(x=y,start = 154,stop = nletras)  )
})
lista.archs <- unlist(lista.archs)

###############################################

diasLista <- unlist(lapply(lista.archs,function(x){
  str_sub(string = x ,start = 30,end =37 )
}))

diasInd <- min(which(unlist(lapply(diasLista,function(y){
  pracma::strcmp(s1 = y,s2= gsub( fecha.anterior,pattern = '-',replacement = '') )
}))))


lista.archs <- lista.archs[-(1:diasInd)]
##############################################
lista.archs <- list.files(path=datos.dir,full.names = TRUE)
####### POLÍGONOS PARA HOVMOLLER ###############
load('D:/programasR/boletin/peru_poli_50mi.RData')
#load('C:/Users/gramirez/programasR/boletin/peru_poli_viento.RData')
cualPoli <- costa@polygons[[1]]@Polygons[[1]]@coords

cualPoli <- as.data.frame(cualPoli)

names(cualPoli) <- c('lon','lat')


# cualPoli$lon <- 360+cualPoli$lon 
####### LECTURA DE ARCHIVOS ###########

load(file = 'D:/programasR/boletin/viento_climat_2000-2014.RData')

arch <- lista.archs[1]
ncId <- nc_open(arch)
u <- 0.0*ncvar_get(ncId,'eastward_wind') 
v <- 0.0*ncvar_get(ncId,'northward_wind')
lon <- ncvar_get(ncId,'lon') + 360
lat <- ncvar_get(ncId,'lat')
nc_close(ncId)
Magnitud <- sqrt(u^2 + v^2)

malla <-  expand.grid(lon=lon,lat=lat)

narchs <- length(lista.archs)

indc <- which(point.in.polygon( malla$lon,malla$lat,
                                cualPoli$lon,cualPoli$lat )==1)


latitudes <- unique(malla$lat[indc])
longitudes <- unique(malla$lon[indc])

datos.M <- matrix( nrow=length(latitudes), ncol= length(dias.arch) ) 
datos.U <- datos.M
datos.V <- datos.M
datos.A <- datos.M
##########################

Prom <- sqrt(u_p^2+v_p^2)

malla.prom <- expand.grid(lon_p,lat_p)

names(malla.prom) <- c('lon','lat')

P <- stack(as.data.frame(Prom))$values
up <- stack(as.data.frame(u_p))$values
vp <- stack(as.data.frame(v_p))$values
malla.prom <- data.frame( lon=malla.prom$lon,lat=malla.prom$lat,prom=P,u_p=up,v_p=vp ) 

modelo <- mgcv::gam( prom ~ te(lon,lat,k=c(15,15)),data=malla.prom )

Prom <- stack( predict( modelo,newdata=malla[indc,]  ) )$values

modelo <- mgcv::gam( u_p ~ te(lon,lat,k=c(15,15)),data=malla.prom )

Up <- stack( predict( modelo,newdata=malla[indc,]  ) )$values

modelo <- mgcv::gam( v_p ~ te(lon,lat,k=c(15,15)),data=malla.prom )

Vp <- stack( predict( modelo,newdata=malla[indc,]  ) )$values

########################
kk <- 1
falla <- 0
for (ii in 1:length(dias.arch)){
  indc.dia <- grep(pattern=paste0('/',dias.arch[ii]),x=lista.archs)
  for (jj in 0:(length(indc.dia)-1)){
    if (kk<length(lista.archs)){
      tryCatch({
        ncin <- nc_open(lista.archs[kk])
        u <- ncvar_get(ncin,'eastward_wind') + u
        v <- ncvar_get(ncin,'northward_wind') + v
        nc_close(ncin)},
        error=function(e) {e
          falla <- falla + 1 
        },finally = {kk <- kk + 1}  )
    }else{
      break
    }}
  ################# 
  nbuenos <- length(indc.dia) - falla
  
  if (nbuenos>0){
    u <- u/nbuenos
    v <- v/nbuenos
    Magnitud <- sqrt( u^2 + v^2 )
    X <- malla$lon[indc]
    Y <- malla$lat[indc]
    Z <- Magnitud[indc]
    A <- Z - Prom
    U <- u[indc]-Up
    V <- v[indc]-Vp
    for ( qq in 1:length(latitudes)){
      indc2 <- Y==latitudes[qq]
      datos.M[qq,ii] <- mean( Z[indc2],na.rm=TRUE  )
      datos.U[qq,ii] <- mean( U[indc2],na.rm=TRUE  )
      datos.V[qq,ii] <- mean( V[indc2],na.rm=TRUE  )
      datos.A[qq,ii] <- mean( A[indc2],na.rm=TRUE)
    }
  }else{
    u <- NA*u
    v <- NA*v
  }  
  
  u <- 0.0*u
  v <- 0.0*v
}



# rm(list=c('Z','U','V'))
###### EJES ###############

tiempo <- as.numeric(seq(from=fecha.anterior,to=fecha.actual,by='day'  ))

######### MALLA #########

malla <- expand.grid(lat=latitudes,tiempo=tiempo)

Z <- stack( as.data.frame(datos.M) )$values
U <- stack( as.data.frame(datos.U) )$values
V <- stack( as.data.frame(datos.V) )$values
A <- stack( as.data.frame(datos.A) )$values

hovmoller <- data.frame( tiempo = malla$tiempo,
                         lat = malla$lat,
                         mag = A)
nSerie <- length(malla$tiempo)
poner <- seq(from=1,to=nSerie,length.out = floor(length(malla$tiempo)/6)  )

suave <- gam(mag ~ te(tiempo,lat,k=c(50,10)),data = hovmoller)
pred <- predict(suave,newdata = as.data.frame(malla))
Z <- as.matrix( pred )
hovmoller <- data.frame(   tiempo = as.Date(malla$tiempo,origin='1970-01-01'),
                           lat = malla$lat,
                           mag = Z,
                           u = U,
                           v = V)

campo <- data.frame(tiempo = as.Date(malla$tiempo[poner],origin='1970-01-01'),
                    lat = malla$lat[poner],
                    u = U[poner],
                    v = V[poner])

rm(list=c('Z','U','V'))
###############

marcas_y <- seq(-20,2,by=2)
etiquetas_y <- unlist(lapply( as.vector(marcas_y),
                              function(x) if( x<0 ) {paste0(-x,'S')}else if(x>0){paste0(x,'N')}else{x} )   )

######### DIAGRAMA DE HOVMOLLER ################
if (exists('pp')){
  rm(pp)
}

paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')

niveles <- seq(from=-8,to=8,by=2)
##################################
png(filename = paste0(figuras,'Hovmoller_anomalia_viento_',fecha.actual,'.png'),width=2400,height=1000)
pp <- ggplot( data=hovmoller,aes(x = tiempo,y = lat,fill = mag) )
pp <- pp + geom_raster(aes(fill = mag),interpolate=TRUE,show.legend = TRUE  )
pp <- pp + scale_fill_gradientn(colours=paleta_color,limits =range(niveles),
                                breaks = niveles)
pp <- pp + stat_contour(data = hovmoller, aes(x=tiempo,y=lat,z=mag),
                        breaks = niveles,col='grey45' ,inherit.aes=FALSE )
pp <- pp + geom_text_contour(data=hovmoller,aes(x=tiempo,y=lat,z=mag,label=..level..),
                             rotate=FALSE,size=12,stroke=0.15,
                             check_overlap = TRUE)
pp <- pp + geom_quiver(data = campo,
                       aes(   x = tiempo,
                              y = lat,
                              u = u,
                              v = v),
                       vecsize = 3,
                       size = 1,
                       inherit.aes = FALSE)
pp <- pp + labs(x = 'Fecha',y = 'Latitud',
                title = paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = paste0('Anomalía del campo de viento: ',
                                  format(fecha.anterior,'%B-%d'),' a ',
                                  format(fecha.actual,'%B-%d'),'\nFranja de 50 millas'),
                caption = 'Fuente: IFREMER CERSAT Global Blended Mean Wind Fields on 25km X 25km grid
                \n Climatología: QUIKSCAT-ASCAT 2000-2014')
pp <- pp + theme_bw()
pp <- pp +theme( axis.text.x = element_text( size=30, angle = 90 ),
                 axis.text.y = element_text( size=30 ),
                 axis.title.x = element_text( size=28 ),
                 axis.title.y = element_text( size=28 ),
                 title = element_text(size=36),
                 plot.caption = element_text(size = 30,hjust = 0))

pp <- pp + guides( fill = guide_colorbar(barheight = unit(20, "cm"),
                                         barwidth = unit(2,'cm'),
                                         label.theme = element_text(size=28),
                                         title = 'm/s',
                                         title.theme = element_text(size=28)))
pp <- pp + scale_x_date( breaks=seq.Date(from = fecha.anterior, 
                                         to = fecha.actual, 
                                         by = "week"),
                         date_labels = '%b-%d',
                         expand = c(0,0) ) 

pp <- pp + scale_y_continuous(limits=c(-20,2),breaks = marcas_y  ,
                              expand = c(0,0),
                              labels = etiquetas_y) 

plot(pp)

dev.off()
