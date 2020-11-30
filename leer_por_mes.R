leer_por_mes <- function(directorio,lista_archivos,variable,inicio,cuantos){
  # 'eastward_wind'
  
  var_por_mes <- lapply(lista_archivos,function(x){
    #***********************************************************
                    parcial <-  lapply(paste0(directorio,x),function(y){
                     tryCatch({
                      nc.ID <-  open.nc(y) 
                      vel <- var.get.nc(nc.ID,variable,
                             start=c(cuales.lon[1],cuales.lat[1],1,1),
                             count=c(length(cuales.lon),length(cuales.lat),1,1))
                     close.nc(nc.ID)
                     return(vel)},error=function(e){print('No existe el archivo')
                       NULL})
                })
  #*********************************************************             
                    y <- parcial[[1]]
                    
                    lapply(seq_along(parcial)[-1], function(ii){
                      if (!is.null(parcial[[ii]])){
                            y <<- y + parcial[[ii]] }
                    })
                    return(0.01*y/length(parcial))# PROMEDIO MENSUAL
                })
  return(var_por_mes)
}