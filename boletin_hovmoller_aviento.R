### PROCESAR DATOS DE VIENTO ASCAT #####

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

###############CARPETAS############
###
raiz <- 'E:/boletin/'
setwd(raiz)
gshhs.dir <- 'E:/programasR/gshhg-bin-2.3.7/'
############### DEFINICI?N DEL POL?GONO #################33
franja <- '50'
if (is.na(as.numeric(franja))){
  load('E:/programasR/boletin/peru_poli_ENFEN.RData')
}else{
  load('E:/programasR/boletin/peru_poli_50mi.RData')
}
cualPoli <- costa@polygons[[1]]@Polygons[[1]]@coords
#############DEFINICION DE FECHAS##################

dias.atras <- 30*13
fecha.actual <- lubridate::today() - 2
fecha.anterior <- fecha.actual - dias.atras# en d?as
prueba <- FALSE

lista.dias <- seq(from=fecha.anterior,to=fecha.actual,by='day')
lista.dias <- gsub(lista.dias,pattern = '-',replacement = '/')
dias.arch <- gsub(lista.dias,pattern = '/',replacement = '')
fecha.titulo <- gsub(lista.dias,pattern = '/',replacement = '') 
datos.dir <- paste0('E:/boletin/viento/',lubridate::year(fecha.actual),'/datos/')
anio <- lubridate::year(lista.dias)
mes <- formatC(lubridate::month(lista.dias),width=2,flag=0)
fechas <- unique(paste0(anio,'/',mes))
figuras <- paste0('E:/boletin/viento/',lubridate::year(fecha.actual),'/figuras/')

if (!dir.exists(figuras)){
  dir.create(figuras)
}

if (!dir.exists(datos.dir )){
  dir.create(datos.dir ,recursive = TRUE)
}
# prueba <- TRUE
if (!prueba){
# ftp.dir <- 'ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/MWF/L3/ASCAT/Daily/Netcdf/'

ftp.dir <- paste0('ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/WIND_GLO_WIND_L4_NRT_OBSERVATIONS_012_004/CERSAT-GLO-BLENDED_WIND_L4-V6-OBS_FULL_TIME_SERIE/')

lista.carpetas <-as.list(paste0(ftp.dir,fechas,'/'))

archs.bajar <- lapply(lista.carpetas,function(x) paste0(x,unlist(strsplit(getURL(paste0(x,'*.nc'),dirlistonly = TRUE,verbose=TRUE),'\r\n'))))
archs.bajar <- unlist(archs.bajar)
closeAllConnections()

lapply(archs.bajar , function (x){
  arch.sal <- paste0(datos.dir,
                     str_extract(x,pattern = '/[0-9]+(-|_).+\\.nc' ))
  if (!file.exists(arch.sal)){
    download.file( url=x, destfile=arch.sal,
                   method = "auto",quiet = FALSE, mode="wb", cacheOK = TRUE  )
    closeAllConnections()}
})

nletras <- str_count(archs.bajar[1])
lista.archs <- lapply(archs.bajar , function (x){
  arch.sal <- paste0(datos.dir,
                     str_sub(string=x,start=154,end=nletras ))})
lista.archs <- unlist(lista.archs)
#############################
diasLista <- unlist(lapply(lista.archs,function(x){
  str_sub(string = x ,start = 30,end =37 )
}))

diasInd <- min(which(unlist(lapply(diasLista,function(y){
  pracma::strcmp(s1 = y,s2= gsub( fecha.anterior,pattern = '-',replacement = '') )
}))))-1


lista.archs <- lista.archs[-(1:diasInd)]
}
##############################################
# lista.archs <- list.files(path=datos.dir,full.names = TRUE)
####### POL?GONOS PARA HOVMOLLER ###############
# load('E:/programasR/boletin/peru_poli_ENFEN.RData')# variable costa
# poli50_200 <- costa@polygons[[1]]@Polygons[[1]]@coords
#load('C:/Users/gramirez/programasR/boletin/peru_poli_viento.RData')

cualPoli <- as.data.frame(cualPoli)

names(cualPoli) <- c('lon','lat')

####### LECTURA DE ARCHIVOS ###########
load(file = 'E:/programasR/boletin/viento_climat_GRASP_2000-2014.RData')

u_p <- Reduce(f="+",x=u_p,accumulate=FALSE)/12

v_p <- Reduce(f="+",x=v_p,accumulate=FALSE)/12

Prom <- sqrt(u_p^2+v_p^2)
arch <- lista.archs[1]
ncin <- nc_open(arch)
u <- 0.0*ncvar_get(ncin,'eastward_wind')[cuales.lon,cuales.lat]
v <- 0.0*ncvar_get(ncin,'northward_wind')[cuales.lon,cuales.lat]
lon <- ncvar_get(ncin,'lon')[cuales.lon]
lat <- ncvar_get(ncin,'lat')[cuales.lat]
nc_close(ncin)
Magnitud <- sqrt(u^2 + v^2) 

malla <- expand.grid(lon=lon,lat=lat)
U <- stack( as.data.frame(u) )
V <- stack( as.data.frame(v) )
Z <- stack( as.data.frame(Magnitud) )
mapa <- data.frame( lon = as.matrix(malla$lon),
                    lat = as.matrix(malla$lat),
                    u = as.matrix(U$values),
                    v = as.matrix(V$values),
                    z = as.matrix(Z$values)  )

######    PROMEDIOS CADA CUATRO ARCHIVOS ########################
indc <- which(point.in.polygon( malla$lon+360,malla$lat,
                                cualPoli$lon,cualPoli$lat )==1)


latitudes <- unique(malla$lat[indc])

narchs <- length(lista.archs)
datos.M <- matrix( nrow=length(latitudes), ncol= length(lista.dias) ) 
datos.U <- datos.M
datos.V <- datos.M
datos.A <- datos.M
############################


kk <- 1

for (ii in 1:length(dias.arch)){
  indc.dia <- grep(pattern=paste0('/',dias.arch[ii]),x=lista.archs)
  .GlobalEnv$falla <- 0
  for (jj in 0:(length(indc.dia)-1)){
    if (kk<length(lista.archs)){
      tryCatch({
        ncin <- nc_open(lista.archs[kk])
        u <- ncvar_get(ncin,'eastward_wind')[cuales.lon,cuales.lat] + u
        v <- ncvar_get(ncin,'northward_wind')[cuales.lon,cuales.lat] + v
        nc_close(ncin)},
        error=function(e) {
          print(lista.archs[kk])
          file.remove(lista.archs[kk])
          .GlobalEnv$falla <- .GlobalEnv$falla+1 
        },finally = {kk <- kk + 1}  )
    }else{
      break
    }
    
  }
  ################# 
  nbuenos <- length(indc.dia) - .GlobalEnv$falla
  
  if (nbuenos>0){
    u <- u/nbuenos
    v <- v/nbuenos
    Magnitud <- sqrt( u^2 + v^2 )
    X <- malla$lon[indc]
    Y <- malla$lat[indc]
    Z <- Magnitud[indc] - Prom[indc]
    U <- u - u_p[indc]
    V <- v - v_p[indc]
    for ( qq in 1:length(latitudes)){
      indc2 <- Y==latitudes[qq]
      datos.M[qq,ii] = mean( Z[indc2],na.rm=TRUE  )
      datos.U[qq,ii] = mean( U[indc2],na.rm=TRUE  )
      datos.V[qq,ii] = mean( V[indc2],na.rm=TRUE  )
    }
  }else{
    u <- NA*u
    v <- NA*v
  }  
  
  u <- 0.0*u
  v <- 0.0*v
}

rm(list=c('Z','U','V'))
###### EJES ###############

lati <- unique( mapa$lat[indc] )
tiempo <- as.numeric(seq(from=fecha.anterior,to=fecha.actual,by='day'  ))

######### MALLA #########

malla <- expand.grid(lat=lati,tiempo=tiempo)

Z <- stack( as.data.frame(datos.M) )$values
U <- stack( as.data.frame(datos.U) )$values
V <- stack( as.data.frame(datos.V) )$values

indNa <- is.na(Z)

indNa <- is.na(U) | indNa
indNa <- is.na(V) | indNa
indNa <- which(indNa)

if (length(indNa)>0){
  Z <- Z[-indNa]
  U <- U[-indNa]
  V <- V[-indNa]
  malla2 <- malla[-indNa,]
}else{
  malla2 <- malla
  U <- U
  V <- V
}

# malla <- malla2
nSerie <- length(malla$tiempo)
interpolar <- data.frame( tiempo = malla2$tiempo,
                             lat = malla2$lat,
                              mag = Z,
                                u = U,
                                v = V)

  rm(list=c('U','V','Z'))

 suave <- gam(mag ~ te(tiempo,lat,k=c(50,10)),data = interpolar)
  pred <- predict(suave,newdata = as.data.frame(malla))
     Z <- stack( pred )$value

 suave <- gam(u ~ te(tiempo,lat,k=c(50,10)),data = interpolar)
  pred <- predict(suave,newdata = as.data.frame(malla))
     U <- stack( pred )$values

 suave <- gam(v ~ te(tiempo,lat,k=c(50,10)),data = interpolar)
  pred <- predict(suave,newdata = as.data.frame(malla))
     V <- stack( pred )$values

######
hovmoller <- data.frame(   tiempo = as.Date(malla$tiempo,origin='1970-01-01'),
                           lat = malla$lat,
                           anm = Z,
                             u = U,
                             v = V)

mag <- sqrt(hovmoller$u^2 + hovmoller$v^2)

hovmoller$u <- hovmoller$u/mag
hovmoller$v <- hovmoller$v/mag
#############

  rm(list=c('Z','U','V','interpolar'))

######### DIAGRAMA DE HOVMOLLER ################
 poner <- seq(from=1,to=nSerie,length.out = 7700  )
if (exists('pp')){
  rm(pp)
}

marcas_y <- seq(-20,2,by=2)

etiquetas_y <- unlist(lapply( as.vector(marcas_y),
                              function(x) 
                                if( x<0 ) 
                                {paste0(-x,'S')}else if(x>0){paste0(x,'N')
                                }else{x} )   )

# paleta_color <- c('#f0f0f0','#b4b4b4','#7878f7','#3c3cfb','#6000ff','#0040ff',
#                   '#0080ff','#00bfff','#00ffff','#2bffd4','#55ffaa','#80ff80',
#                   '#aaff55','#d4ff2b','#ffff00','#ffd400','#ffaa00','#ff8000',
#                   '#ff5500','#ff2b00','#ff0000','#d40000','#aa0000','#800000')

paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
niveles <- seq(from=-8,to=8,by=1)
if (is.na(as.numeric(franja))){
  franja <- str_replace_all('50 a 200',pattern = ' ',replacement = '_')
}

png(filename = paste0(figuras,'Hovmoller_aviento_',fecha.actual,'_',franja,'millas.png'),
              width=2400,height=1000)
pp <- ggplot( data=hovmoller,aes(x = tiempo,y = lat,fill = anm) )
pp <- pp + geom_raster(interpolate=TRUE,show.legend = TRUE  )
pp <- pp + scale_fill_gradientn(colours=paleta_color,limits =range(niveles),
                                breaks = niveles)
pp <- pp + stat_contour(aes(z=anm),
                        breaks = niveles,col='grey45')
pp <- pp + geom_text_contour(data=hovmoller,aes(x=tiempo,y=lat,z=anm,label=..level..),
                             rotate=FALSE,size=12,stroke=0.15,
                             check_overlap = TRUE)
pp <- pp + geom_quiver(data=hovmoller[poner,],aes(x=tiempo[poner],y=lat[poner],
                             u = u[poner],
                             v = v[poner]),
                       vecsize = 3,
                          size = 1,
                   inherit.aes = FALSE)
pp <- pp + labs(x = 'Fecha',y = 'Latitud',
                title = paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                               'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = paste0('Anomalía del viento: ',
                                  format(fecha.anterior,'%B-%d'),' a ',
                                  format(fecha.actual,'%B-%d'),'\nFranja de ',franja,' millas'),
                caption = 'Fuente: IFREMER CERSAT Global Blended Mean Wind Fields on 25km X 25km grid')
pp <- pp + theme_bw()
pp <-  pp +theme( axis.text.x = element_text( size=28, angle = 90 ),
                  axis.text.y = element_text( size=28 ),
                  axis.title.x = element_text( size=28 ),
                  axis.title.y = element_text( size=28 ),
                  plot.title = element_text(size=36),
                  plot.subtitle = element_text(size=42),
                  plot.caption = element_text(size = 30,hjust = 0))

pp <- pp + guides( fill = guide_colorbar(barheight = unit(20, "cm"),
                                         barwidth = unit(2,'cm'),
                                         label.theme = element_text(size=28),
                                         title = 'm/s',title.theme = element_text(size=28)))
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

