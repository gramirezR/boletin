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
source('bajarCopernico.R')
source('boletin_lista_salinidad.R')
raiz <- 'E:/boletin/salinidad/'
gshhs.dir <- 'E:/programasR/gshhg-bin-2.3.7/'
#############DEFINICION DE FECHAS##################

t0.copernico <- as.Date('1981-01-01') # segundos desde esta fecha
fecha.inicial <- as.Date('2020-11-01')
fecha.actual <- lubridate::today() - 2
dias.atras.hov <- 30*6 + 1 

dias <- seq(from=fecha.inicial, to=fecha.actual,by='day')
periodo <- 'mensual'

boyas <- data.frame(x=c(-85,-85),y=c(-5,-8))


ftp = 'ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/GLOBAL_ANALYSIS_FORECAST_PHY_001_024/global-analysis-forecast-phy-001-024-3dinst-so'
prefijo <- 'mercatorpsy4v3r1_gl12_so_'

lista_archivos <- lapply(dias, FUN=function(x){
  anio <- lubridate::year(x)
  mes <- sprintf(lubridate::month(x),fmt = '%02d')
  dia <- sprintf(lubridate::day(x),fmt='%02d')
  # dia_fin<- sprintf(lubridate::day(x)+1,fmt='%02d')
  arch1 <- paste0( raiz,prefijo,anio,mes,dia,'_00h_R',anio,mes,dia,'.nc')
  # arch6 <- paste0( raiz,prefijo,anio,mes,dia_ini,'_06h_R',anio,mes,dia_fin,'.nc')
  # arch12 <- paste0( raiz,prefijo,anio,mes,dia_ini,'_12h_R',anio,mes,dia_fin,'.nc')
  # arch18 <- paste0( raiz,prefijo,anio,mes,dia_ini,'_18h_R',anio,mes,dia_fin,'.nc')
  # resultado <- rbind(arch1,arch6,arch12,arch18)
  resultado <- rbind(arch1)
  return( resultado )
})

lista_archivos <- do.call(what = rbind, args = lista_archivos )

lista_archivos

existe_archivo <- lapply(lista_archivos, FUN=function(x){
  return(file.exists(x))
}  )

existe_archivo <-  do.call(what = rbind, args = existe_archivo )

archivos_que_no_hay <- lista_archivos[ which( !existe_archivo)  ]

archivo_mercator <- function(archivo){
  y = substr(archivo,start = 22, stop = 71)
  anio <- substr(y,start = 26, stop = 29)
  mes <-  substr(y,start = 30, stop = 31)
  paste(ftp,anio,mes,y, sep='/')
}



if ( length(archivos_que_no_hay)>0 ){
  lapply(archivos_que_no_hay, FUN = function(x){
    carpeta <- dirname(x)
    if (!dir.exists(carpeta)){ dir.create(carpeta,recursive = TRUE)}
    # print( archivo_mercator(x) )
    archivo_descarga <- archivo_mercator(x)
    if(!file.exists(x)){
    e <- simpleError("test error")
    tryCatch( download.file( url = archivo_descarga, destfile = x , method = "auto",
                             quiet = FALSE, mode="wb", cacheOK = TRUE  ),
              error = function(e){
                               print(x)
                               file.remove(x)
                                 }, 
              finally = print("Hello") )  }
    
  } ) }

###################################################################################

# t0 <- hours since 1950-01-01 00:00:00


indices_no_hay <- lapply(lista_archivos,FUN = function(x){
  file.exists(x)
})

indices_no_hay <- which(!do.call(what = rbind, args = indices_no_hay ))

if ( length(indices_no_hay) ){
  lista_archivos <- lista_archivos[-indices_no_hay]
}

lim.lon <- c(270, 290) - 360
lim.lat <- c(-20, 2)

ncin <- open.nc( lista_archivos[1] )

# print.nc(ncin)

lon.p <- var.get.nc(ncin,'longitude')
lat.p <- var.get.nc(ncin,'latitude')

indc.lon <- which(lon.p >= lim.lon[1] & lon.p <= lim.lon[2])
indc.lat <- which(lat.p >= lim.lat[1] & lat.p <= lim.lat[2])

lon <- lon.p[indc.lon]
lat <- lat.p[indc.lat]

rm('lon.p')
rm('lat.p')

A <- var.get.nc(ncin,'so',start = c(indc.lon[1],indc.lat[2],1,1),
                count = c(length(indc.lon), length(indc.lat), 1, 1), collapse = TRUE, unpack = TRUE)
############### PROMEDIO DE LOS ARCHIVOS #######################  
close.nc(ncin)

narchs <- length(lista_archivos)
if (narchs>1){
  for (ii in 2:narchs){
    
    arch <- lista_archivos[ii]
    tryCatch(
             {ncin <- open.nc(arch)
             A <- var.get.nc(ncin,'so',start = c(indc.lon[1],indc.lat[2],1,1),
                             count = c(length(indc.lon), length(indc.lat), 1, 1), collapse = TRUE, unpack = TRUE) + A
             close.nc(ncin)
              },
             error = function(e) e,
             finally = print("Hello")
             )
  }}

A <- A/narchs
#########################################

malla <- expand.grid(lon=lon,lat=lat)

Z <- stack( as.data.frame(A) )

mapa <- data.frame( lon = as.matrix(malla$lon),
                    lat = as.matrix(malla$lat),
                    z = as.matrix(Z$values)  )

#######################################
load('costa_Peru_202.RDat') 
Dt <- 0.5
npts <- 10
tamanio <- 10

suave <- mgcv::gam(z~te(lon,lat,k=c(15,15)),data=mapa,na.action = 'na.omit')

pred <- predict(suave,newdata = as.data.frame(malla))
Z <- as.matrix( pred )
mapa <- data.frame(     lon = malla$lon,
                        lat = malla$lat,
                        z = Z)

costa <- shore[ shore$group=='3.1', ]

indc <- which( point.in.polygon( malla$lon,malla$lat,
                                 costa$long-360,costa$lat )==1 )

mapa$z[indc] <- NA

rm('pred')
rm('suave')
##########################################

niveles <- c(seq(31,34,by=0.5),seq(34,36,by=0.1))
subtitulo.graf <- 'Salinidad'
titulo.barra <- 'Salinidad (UPS)'

subtitulo.grafico <- paste0(subtitulo.graf,': del ',
                            format(fecha.inicial, format='%d-%B'),' al ',
                            format(fecha.actual, format='%d-%B-%Y'))
paleta_color <- cptcity::cpt('ncl_StepSeq25')#[20:100]
# paleta_color <- cptcity::cpt('ncl_StepSeq25')
# paleta_color <- cptcity::cpt('cb_div_BrBG_11')
# paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
# cmocean_haline
##########################################


if ( exists('pp')){
  rm(pp)  
}
marcas_x <- seq(lim.lon[1],lim.lon[2],by=5)

etiquetas_x <- unlist(lapply( (marcas_x ) ,
                              function(x){ 
                                if (x > 180){
                                  paste0(as.vector( as.character(360-x)),'W') 
                                }else if( x< 180) {
                                  paste0( x ,'E')
                                }else{
                                  as.character(x)
                                }
                              }) )

marcas_y <- seq(from=lim.lat[1],to=lim.lat[2],by=5)
etiquetas_y <- unlist(lapply( as.vector(marcas_y),
                              function(x) if( x<0 ) {paste0(-x,'S')}else if(x>0){paste0(x,'N')}else{x} )   )
##################################
png(width=1200, height=1200, filename = paste0(raiz,'figuras/salinidad',fecha.actual,'.png'  ))
pp <- ggplot( data=mapa,aes(x=lon,y=lat,fill=z) )
pp <- pp + geom_raster(aes(fill = z),interpolate=TRUE,show.legend = TRUE  )
pp <- pp + scale_fill_gradientn( colours = paleta_color,
                                 limits = range(niveles))
pp <- pp + stat_contour(data=mapa, aes(x=lon,y=lat,z=z),
                        linetype = 1 ,
                        col = 'black' ,
                        inherit.aes = FALSE,
                        breaks = niveles)

 pp <- pp + geom_polygon( data=shore,aes(x=long-360,y=lat,group=group),color = 'black', fill = 'grey80',inherit.aes=FALSE  )
for (kk in 1:length(fronteras)){
  f <- as.data.frame(fronteras[[kk]])
  pp <- pp + geom_point(data=f,aes( x=X1-360,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
}

 pp <- pp + geom_text_contour(aes(z=z),
                              stroke = 0.1,skip=0,min.size =2,size=tamanio,rotate = FALSE,
                              check_overlap=TRUE,breaks = niveles)
 

  pp <- pp + geom_point(data=boyas,aes(x=x,y=y),color='black',size=5,inherit.aes=FALSE)  
  pp <- pp + geom_text(data=boyas,aes(x=x,y=y-0.4,label=c('B1','B2')),
                       color='black',size=12,inherit.aes=FALSE)
  

pp <- pp + scale_x_continuous(limits = range(marcas_x),
                              expand = c(0,0),
                              breaks = marcas_x,
                              labels = etiquetas_x)
pp <- pp + scale_y_continuous(limits = lim.lat,
                              expand = c(0,0),
                              breaks = marcas_y,
                              labels = etiquetas_y)
pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = subtitulo.grafico,
                caption = 'Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 1981-2009')
#pp <- pp + coord_cartesian(xlim=c(282,283),ylim=c(-13,-10))
pp <- pp + theme_bw(   )

  pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                    axis.title.y = element_text( size=28,hjust=0.5  ),
                    axis.text = element_text(size=28,colour = 'black'),
                    title=element_text(size=28),
                    plot.subtitle=element_text(size=24),
                    plot.caption = element_text(size = 22,hjust = 0))



pp <- pp + guides( fill = guide_colorbar(  barheight = unit(20, "cm"),
                                           barwidth = unit(1.5,'cm'),
                                           label.theme = element_text(size=26),
                                           title = titulo.barra,
                                           title.theme = element_text(size=26)))



plot(pp)

dev.off()

