
ruta.ftp <- 'ftp://gramirez2:$boletinDHN2018@my.cmems-du.eu/Core/GLOBAL_REANALYSIS_PHY_001_030/global-reanalysis-phy-001-030-monthly/'
# ftp://nrt.cmems-du.eu/Core/GLOBAL_ANALYSIS_FORECAST_PHY_001_024/global-analysis-forecast-phy-001-024-monthly/2016/
######################################################

ruta.archivos <- function(inicio,final,ruta){
  anio.inicial <- lubridate::year(inicio)
  anio.final <-  lubridate::year(final)
  lista.carpetas.ftp <- lapply(seq(from=anio.inicial,to=anio.final,by=1),
                               function(x){
                                 paste0(ruta,x,'/mercatorglorys12v1_gl12_mean_',x)                   
                               })
  lista <- lapply(lista.carpetas.ftp, function(x){
    meses <- seq(from=1,to=12,by=1)
    paste0(x,sprintf( meses,fmt = '%02d'),'.nc')
  })
  unlist(lista)
}

###########################################################

bajar.archivos <- function(lista,guardar.en.carpeta){
  lapply(lista, function(x){
    nombre.archivo <- substring(x,first = 125,last = 163)
    ruta.local <- paste0(guardar.en.carpeta,nombre.archivo)
    if (!file.exists(ruta.local)){
      download.file( x, ruta.local,method = "auto",quiet = FALSE, mode="wb", cacheOK = TRUE  )}
  })
}

###########################################################
promedio <- function(archivo,variable,inicio,cuantos,factor.var){
  
  variable <- var.get.nc(archivo,variable,
                         start = inicio,
                         count = cuantos)
  return(apply(X = variable,MARGIN = c(1,2),FUN = mean,na.rm=TRUE)*factor.var)
}

#########################################################

pasar.aRaster <- function(datos,x,y){
  x <- seq(from=x[1],to=last(x),length.out = length(x))
  y <- seq(from=y[1],to=last(y),length.out = length(y))
  
  
  malla <- expand.grid(x,y)
  
  datos <- utils::stack(as.data.frame(datos))$values
  
  mapa <- data.frame(lon=malla$Var1,lat=malla$Var2,valor=datos)
  
  coordinates(mapa) <- ~ lon + lat 
  
  gridded(mapa) <- TRUE
  
  crs(mapa) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  
  return(rasterFromXYZ(mapa))
}

##########################################################

promedio.mes <- function(lista,mes.numero){
  
  mes.nombre <- lubridate::month(paste('2020',sprintf(mes.numero,fmt = '%02d'),'01',sep = '-'),
                                 label=TRUE,abbr=FALSE)
  indice.arch <- seq(from=1,to=length(lista),by=1)
  indice.mes <- indice.arch%%12
  if (mes.numero==12){
    cuales <- which(indice.mes==0)
  }else{
    cuales <- which(indice.mes==mes.numero)
  }

  x <- vector(mode='list',length=length(cuales))
  cuantos <- length(cuales)
  if (cuantos>0){
  for (ii in 1:cuantos){
    x[[ii]] <- lista[[cuales[ii]]]
  }
  
  es.nulo <- which( unlist( lapply(x,FUN=function(x){is.null(x)}) ) )
    if ( length(es.nulo)>0 ){
      x <- x[-es.nulo]
    }
  x.raster <- do.call(what=stack,args=x)
  
  promedio <- calc(x.raster,fun = mean,na.rm=TRUE)
  names(promedio) <- mes.nombre
  return(promedio)
  }else{
    return(NULL)
  }
}

##############################

rasterVelocidad2ggplot <- function(rasterStack){
  lon.min <- rasterStack@extent@xmin
  lon.max <- rasterStack@extent@xmax
  nfilas <- rasterStack@nrows
  
  lat.min <- rasterStack@extent@ymin
  lat.max <- rasterStack@extent@ymax
  ncolumnas <- rasterStack@ncols
  
  longitud <- seq(lon.min,lon.max,length.out = ncolumnas)
  latitud <-  seq(lat.min,lat.max,length.out = nfilas)
  
  
  malla <- expand.grid(longitud,latitud)
  ncapas <- length(rasterStack@layers)
  mitad.capas <- ncapas/2
  
  campo <- vector(mode='list',length=mitad.capas)
  
  for (ii in 1:mitad.capas){
    u <- rasterStack@layers[[ii]]@data@values
    v <- rasterStack@layers[[ii+mitad.capas]]@data@values
    mag <- sqrt(u^2 + v^2)
    campo[[ii]] <- data.frame(lat=malla$Var2,lon=rev(malla$Var1),
                            U=rev(u),
                            V=rev(v),
                            Z=rev(mag))
  }
  return(campo)
}

#################################################

rasterStack2ggplot <- function(rasterStack){
  lon.min <- rasterStack@extent@xmin
  lon.max <- rasterStack@extent@xmax
  nfilas <- rasterStack@nrows
  
  lat.min <- rasterStack@extent@ymin
  lat.max <- rasterStack@extent@ymax
  ncolumnas <- rasterStack@ncols
  
  longitud <- seq(lon.min,lon.max,length.out = ncolumnas)
  latitud <-  seq(lat.min,lat.max,length.out = nfilas)
  
  
  malla <- expand.grid(longitud,latitud)
  ncapas <- length(rasterStack@layers)
  
  campo <- vector(mode='list',length=ncapas)
  
  for (ii in 1:ncapas){
    z <- rasterStack@layers[[ii]]@data@values
    campo[[ii]] <- data.frame(lat=malla$Var2,lon=rev(malla$Var1),
                              Z=rev(z))
  }
  return(campo)
}

###########################################


raster2ggplot <- function(raster){
  lon.min <- raster@extent@xmin
  lon.max <- raster@extent@xmax
  nfilas <- raster@nrows
  
  lat.min <- raster@extent@ymin
  lat.max <- raster@extent@ymax
  ncolumnas <- raster@ncols
  
  longitud <- seq(lon.min,lon.max,length.out = ncolumnas)
  latitud <-  seq(lat.min,lat.max,length.out = nfilas)
  
  
  malla <- expand.grid(longitud,latitud)
  
    z <- raster@data@values
    campo <- data.frame(lat=malla$Var2,lon=(rev(malla$Var1)),
                              z=rev(z))

  return(campo)
}

##########################

suavizar <- function(df){
  
  malla <- df[,c('lon','lat')]
  
  suave <- mgcv::gam(z~te(lon,lat,k=c(10,7)),data=df)
  
  pred <- predict(suave,newdata = malla)
  Z <- as.matrix( pred )
  mapa <- data.frame(     lon = malla$lon,
                          lat = malla$lat,
                          z = Z)
  return(mapa)
}