# library(RNetCDF)
# library(tidyverse)
cat('\014')
rm(list=ls())
options(encoding = "UTF-8")
################

Malla_escalar <- setRefClass("Malla_escalar",
                             fields = list(url="character",
                                           usuario="character",
                                           contra="character",
                                           limites="data.frame",
                                           fecha="character",
                                           profundidad = "numeric",
                                           variable="character",
                                           nombreslonlat = "character",
                                           origen = 'character',
                                           unidad_t = "numeric"))

#########################################

Malla_escalar$methods( # ruta
  ruta = function(lonini, lonfin, latini, latfin, tini, tfin){
    raiz <- paste0('https://',.self$usuario,':',.self$contra,'@',.self$url)
    if(is.nan(profundidad)){
      ruta1 <- sprintf(fmt='%s?%s[%d:1:%d][%d:1:%d][%d:1:%d]',
                       raiz,
                       .self$variable,
                       tini, tfin,
                       latini,latfin,
                       lonini, lonfin)
    }else{
      ruta1 <- sprintf(fmt='%s?%s[%d:1:%d][%d:1:%d][%d:1:%d][%d:1:%d]',
                       raiz,
                       .self$variable,
                       tini, tfin,
                       .self$profundidad, .self$profundidad,
                       latini,latfin,
                       lonini, lonfin)  
    }
    ruta2 <- sprintf(',%s[%d:1:%d],%s[%d:1:%d],%s[%d:1:%d]',
                     .self$nombreslonlat[1], lonini, lonfin,
                     .self$nombreslonlat[2], latini, latfin,
                     'time',tini,tfin)
    
    return(paste0(ruta1,ruta2))
  },
  ruta_t = function(){
    raiz <- paste0('https://',.self$usuario,':',.self$contra,'@',.self$url,'?time')
    return(raiz)
  },
  ruta_lon = function(nombre){
    raiz <- paste0('https://',.self$usuario,':',.self$contra,'@',.self$url,'?', nombre)
    return(raiz)
  },
  ruta_lat = function(nombre){
    raiz <- paste0('https://',.self$usuario,':',.self$contra,'@',.self$url,'?', nombre)
    return(raiz)
  }
)

##############################################

Malla_escalar$methods( # indices_lon
  indice_lon = function(){
    openD <- ruta_lon(.self$nombreslonlat[1])
    
    con <- RNetCDF::open.nc(con = openD)
    
    longitude <- RNetCDF::var.get.nc(ncfile = con, variable = .self$nombreslonlat[1])    
    RNetCDF::close.nc(con)
    
    if (min(longitude)<0){
      return( c( 0, length(longitude)-1, -1 ) )
    }else{
      indcX <- which( (longitude>=.self$limites$lon[1]) &  (longitude<=.self$limites$lon[2]) )
      return( c( min(indcX), max(indcX), 1) )  
    }
  }  )

####################################################

Malla_escalar$methods(
  indice_lat=function(){
    openD <- ruta_lat(.self$nombreslonlat[2])
    
    con <- RNetCDF::open.nc(con = openD)
    
    latitud <- RNetCDF::var.get.nc(ncfile = con, variable = .self$nombreslonlat[2] )
    
    RNetCDF::close.nc(con)
    
    indcY <- which( (latitud>=.self$limites$lat[1]) &  (latitud<=.self$limites$lat[2]) )  
    
    return( c( min(indcY), max(indcY) ) )
  }
)

######################################################

Malla_escalar$methods( # indice_tiempo
  indice_tiempo = function(){
    openD <- ruta_t( )
    fecha_ini <- as.POSIXct(as.Date(.self$fecha[1]), origin=.self$origen)
    fecha_fin <- as.POSIXct(as.Date(.self$fecha[2]), origin=.self$origen)
    con <- RNetCDF::open.nc(con = openD)
    
    tmpo <- as.POSIXct(
      RNetCDF::var.get.nc(ncfile = con, variable = 'time',1,NA)*.self$unidad_t,
      origin=.self$origen)
    RNetCDF::close.nc(con)
    cuales <- which( tmpo <= .self$fecha[1] )
    
    if( length(cuales)>0 ) {
      index_ini <- max(cuales)    
    }else{
      index_ini <- 1
    }
    
    cuales <- which( tmpo >= .self$fecha[2] )
    
    if( length(cuales)>0 ) {
      index_fin <- min(cuales)
    }else{
      index_fin <- length(tmpo)
    }
    
    return( c(index_ini, index_fin) )
    
  }
)

#############################################

Malla_escalar$methods(
  centrar_pacifico = function(tiempo, lon, lat, valor, centrar){
    valor <- apply( valor, c(1,3), t  )
    valor <- apply( valor, c(2,3), rev  )
    r <- raster::brick(valor, 
                       xmn = range(lon)[1], xmx=range(lon)[2],
                       ymn = range(lat)[1], ymx=range(lat)[2],
                       crs = sp::CRS("+proj=longlat +datum=WGS84"),
                       transpose = FALSE)
    
    rasterlims <- raster::extent(r)
    
    if ( centrar < 0 ){
      x1 <- raster::crop(r, raster::extent(
          rasterlims[1], (raster::xres(r)*ncol(r)/2  + rasterlims[1]),
          rasterlims[3], rasterlims[4]) )
      
      x2 <- raster::crop(r,
            raster::extent((raster::xres(r)*ncol(r)/2 + rasterlims[1] ), rasterlims[2],
            range(lat)[1], range(lat)[2] ) ) 
      
      raster::extent(x1) <- c( raster::xres(r)*(ncol(r))/2, raster::xres(r)*(ncol(r)),
                               range(lat)[1], range(lat)[2])
      
      origen1 <- raster::origin(x1)
      origen2 <- raster::origin(x2)
      
      raster::origin(x1) <- c(0,origen1[2])
      raster::origin(x2) <- c(0,origen2[2])
      
      r2 <- raster::merge(x1, x2)
      
      r2 <- raster::crop(r2, raster::extent( .self$limites$lon[1], .self$limites$lon[2],
                                             .self$limites$lat[1], .self$limites$lat[2]))
      
    }else{
      r2 <- r  
    }
    r2 <- raster::setZ(r2, tiempo)
    return(r2)
  }
  
)

################################################

Malla_escalar$methods(# Bajar Datos
  bajar_datos = function(){
    
    indice_lat <- .self$indice_lat()
    indice_lon <- .self$indice_lon()
    
    tindc <- .self$indice_tiempo( )
    
    raiz <- ruta(indice_lon[1], indice_lon[2],
                 indice_lat[1], indice_lat[2],
                 tindc[1], tindc[2])
    
    # print(raiz)
    
    con <- RNetCDF::open.nc(con = raiz)
    
    # RNetCDF::print.nc(con)
    
    tiempo <- as.POSIXct(
                  RNetCDF::var.get.nc(    ncfile = con ,
                                       variable = 'time')*.self$unidad_t ,
                          origin = .self$origen)
    
    lon <- RNetCDF::var.get.nc(    ncfile = con, 
                                 variable = .self$nombreslonlat[1])
    
    lat <- RNetCDF::var.get.nc(    ncfile = con,
                                 variable = .self$nombreslonlat[2])
    
    valor <- RNetCDF::var.get.nc(    ncfile = con,
                                   variable = .self$variable, 
                                     unpack = TRUE) 
    
    RNetCDF::print.nc(con)
    
    RNetCDF::close.nc(con)
    
    valor <- .self$centrar_pacifico( tiempo, lon, lat, valor, indice_lon[3])
    
    valor <- raster::setZ( valor, tiempo )
    
    return( valor )
    
  }
)
#######################
# Malla_escalar$methods(
#   guarda_datos = function(datos){
#     return(datos)
#   }
# )

##########################

Malla_escalar$methods(
  promedio_en_tiempo = function(datos){
    promedio <- raster::calc(datos, fun = mean, na.rm = T)
    return(promedio)
  }
)

Malla_escalar$methods(
  como_dataframe = function(datos){
    return( as.data.frame( raster::rasterToPoints( datos ) ) )
  }
)

######################
source('E:/programasR/boletin/boletin_graficos_mapa_oop.R',encoding = "UTF-8")
##################

# copernicus_sal <- Malla_escalar$new(url='nrt.cmems-du.eu/thredds/dodsC/global-analysis-forecast-phy-001-024-3dinst-so',
#                                   usuario='gramirez2',
#                                   contra='$boletinDHN2018',
#                                   limites=data.frame( lon=c(270, 290),
#                                                       lat=c(-25, 5)  ),
#                                   fecha=c( '2022-04-01' ,
#                                            '2022-04-08'
#                                   ),
#                                   profundidad = 0,
#                                   variable='so',
#                                   nombreslonlat = c('longitude', 'latitude') ,
#                                   origen='1950-01-01',
#                                   unidad_t = 3600)
# 
# salinidad <- copernicus_sal$bajar_datos()

####################

# promedio <- copernicus_sal$promedio_en_tiempo(salinidad)
# 
# promedio <- copernicus_sal$como_dataframe( promedio )
# 

##############

# source('E:/programasR/boletin/parametros_ggplot_sal.R', encoding = "UTF-8")
# 
# parametros_mapa <- list(
#            ancho = 1200,
#             alto = 1200,
#   archivo_salida = archivo_salida,
#           titulo = titulo,
#        subtitulo = subtitulo,
#         creditos = creditos,
#     paleta_color = paleta_color,
#          niveles = niveles,
#    niveles_barra = niveles_barra,
#            costa = shore,
#        fronteras = fronteras,
#         marcas_x = marcas_x,
#         marcas_y = marcas_y,
#      etiquetas_x = etiquetas_x,
#      etiquetas_y = etiquetas_y,
#         unidades = unidades
# )
# 
# mapa_sal <- Mapa_boletin$new( malla = promedio,
#                               parametros = parametros_mapa)
# 
# mapa_sal$crear_mapa()


########

# copernicus_temp <- Malla_escalar$new(url='nrt.cmems-du.eu/thredds/dodsC/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2',
#                                      usuario='gramirez2',
#                                      contra='$boletinDHN2018',
#                                      limites=data.frame( lon=c(270, 290),
#                                                          lat=c(-25, 5)  ),
#                                      fecha=c( '2022-05-01',
#                                               '2022-05-08'
#                                      ),
#                                      profundidad= NaN,
#                                      variable='analysed_sst',
#                                      nombreslonlat = c('lon', 'lat') ,
#                                      origen='1981-01-01',
#                                      unidad_t = 1)
# 
# temperatura <- copernicus_temp$bajar_datos()
# 
# promedio <- copernicus_temp$promedio_en_tiempo(temperatura)
# 
# promedio <- copernicus_temp$como_dataframe( promedio - 273.149993896484 )
# # # - 273.149993896484
# 
# source('E:/programasR/boletin/parametros_ggplot_temp.R', encoding = "UTF-8")
# 
# parametros_mapa <- list(
#   ancho = 1200,
#   alto = 1200,
#   archivo_salida = archivo_salida,
#   titulo = titulo,
#   subtitulo = subtitulo,
#   creditos = creditos,
#   paleta_color = paleta_color,
#   niveles = niveles,
#   niveles_barra = niveles_barra,
#   costa = shore,
#   fronteras = fronteras,
#   marcas_x = marcas_x,
#   marcas_y = marcas_y,
#   etiquetas_x = etiquetas_x,
#   etiquetas_y = etiquetas_y,
#   unidades = unidades
# )
# 
# mapa_temperatura <- Mapa_boletin$new( malla = promedio,
#                               parametros = parametros_mapa)
# 
# mapa_temperatura$crear_mapa()


########################################
# lim.lon <- c(120,290)
# lim.lat <- c(-25,25)
#''nrt.cmems-du.eu/thredds/dodsC/METOFFICE-GLO-SST-L4-NRT-OBS-ANOM-V2'

copernicus_temp_a <- Malla_escalar$new(    url = 'nrt.cmems-du.eu/thredds/dodsC/METOFFICE-GLO-SST-L4-NRT-OBS-ANOM-V2',
                                       usuario = 'gramirez2',
                                        contra = '$boletinDHN2018',
                                       limites = data.frame( lon = c(270,292),
                                                             lat = c(-20, 5)  ),
                                         fecha = c( '2022-04-01' ,
                                                    '2022-05-01' ),
                                   profundidad = NaN ,
                                      variable = 'sst_anomaly' ,
                                 nombreslonlat = c( 'lon', 'lat' ) ,
                                        origen = '1981-01-01' ,
                                      unidad_t = 1 )

temperatura_a <- copernicus_temp_a$bajar_datos()
# 
promedio <- copernicus_temp_a$promedio_en_tiempo(temperatura_a)

promedio <- copernicus_temp_a$como_dataframe( promedio )
#

source('E:/programasR/boletin/parametros_ggplot_temp_a.R', encoding = "UTF-8")

parametros_mapa <- list(
  ancho = 1200,
  alto = 1200,
  archivo_salida = archivo_salida,
  titulo = titulo,
  subtitulo = subtitulo,
  creditos = creditos,
  paleta_color = paleta_color,
  niveles = niveles,
  niveles_barra = niveles_barra,
  costa = shore,
  fronteras = fronteras,
  marcas_x = marcas_x,
  marcas_y = marcas_y,
  etiquetas_x = etiquetas_x,
  etiquetas_y = etiquetas_y,
  unidades = unidades
)

source('E:/programasR/boletin/boletin_graficos_mapa_oop.R',encoding = "UTF-8")
mapa_temperatura_a <- Mapa_boletin$new( malla = list(promedio),
                              parametros = parametros_mapa)

mapa_temperatura_a$crear_mapa()

# # ######################################
# # 
# 
# paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
# niveles <- seq(-10,10,by=1)
# 
# windows(width=1200, height=650)
# pp <- ggplot(data=promedio, aes(x=x, y=y, fill=layer)) +
#   geom_raster(interpolate=TRUE) +
#   scale_fill_gradientn(colours = paleta_color,limits = range(niveles)) +
#   geom_contour(aes(z=layer),
#                linetype = 1 ,
#                col = 'black',
#                breaks = niveles)
# 
# plot(pp)

# 
# #################
# copernicus_sla <- Malla_escalar$new(url='nrt.cmems-du.eu/thredds/dodsC/dataset-duacs-nrt-global-merged-allsat-phy-l4',
#                                     usuario='gramirez2',
#                                     contra='$boletinDHN2018',
#                                     limites=data.frame( lon=c(120, 293),
#                                                         lat=c(-20, 20)  ),
#                                     fecha=c( '2022-04-04',
#                                              '2022-05-04' ),
#                                     profundidad = NaN,
#                                     variable='sla',
#                                     nombreslonlat = c('longitude', 'latitude') ,
#                                     origen='1950-01-01',
#                                     unidad_t = 86400)
# 
# sla <- copernicus_sla$bajar_datos()
# promedio_sla <- raster::calc(sla*100, fun = mean, na.rm = T)
# #####################
# copernicus_ugosa <- Malla_escalar$new(url='nrt.cmems-du.eu/thredds/dodsC/dataset-duacs-nrt-global-merged-allsat-phy-l4',
#                                     usuario='gramirez2',
#                                     contra='$boletinDHN2018',
#                                     limites=data.frame( lon=c(120, 293),
#                                                         lat=c(-20, 20)  ),
#                                     fecha=c( '2022-04-04',
#                                              '2022-05-04' ),
#                                     profundidad = NaN,
#                                     variable='ugosa',
#                                     nombreslonlat = c('longitude', 'latitude') ,
#                                     origen='1950-01-01',
#                                     unidad_t = 86400)
# 
# ugosa <- copernicus_ugosa$bajar_datos()
# 
# ########################################################
# copernicus_vgosa <- Malla_escalar$new(url='nrt.cmems-du.eu/thredds/dodsC/dataset-duacs-nrt-global-merged-allsat-phy-l4',
#                                       usuario='gramirez2',
#                                       contra='$boletinDHN2018',
#                                       limites=data.frame( lon=c(120, 293),
#                                                           lat=c(-20, 20)  ),
#                                       fecha=c( '2022-04-04',
#                                                '2022-05-04' ),
#                                       profundidad = NaN,
#                                       variable='vgosa',
#                                       nombreslonlat = c('longitude', 'latitude') ,
#                                       origen='1950-01-01',
#                                       unidad_t = 86400)
# 
# vgosa <- copernicus_vgosa$bajar_datos()
# 
# ########################################################
# 
# promedio_ugosa <- raster::calc(ugosa*100, fun = mean, na.rm = T)
# 
# promedio_vgosa <- raster::calc(vgosa*100, fun = mean, na.rm = T)
# 
# magnitud <- sqrt( promedio_ugosa^2 + promedio_vgosa^2 )
# 
# geostrofia <- raster::brick( promedio_ugosa/magnitud,
#                              promedio_vgosa/magnitud  )
# 
# #########################################################

# ##################
# windows()
# raster::plot(promedio,
#              col = terrain.colors(15),
#              colNA = 'darkgrey',
#              ylim=c(-20,5))
#############################
# rasterVis::levelplot(promedio_sla,
#                      margin  = FALSE,
#                      contour = TRUE,
#                      par.settings = tema_temp,
#                      xlab='Longitud', ylab='Latitud', main=titulo,
#                      scales = list(x = list(at=xmrks),
#                                    y = list(at=ymrks) ),
#                      at = cmrks,
#                      colorkey=list(width = 1.0,
#                                    labels = list(at = cmrks))) +