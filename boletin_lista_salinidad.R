boletin_listaArchivos <- function( ruta.ftp
                                   ,ruta.salida
                                   ,fecha.actual
                                   ,fecha.anterior
                                   ,cuantos.dias
                                   ,variable){
  
  require('RCurl')
  print('PREPARANDO LISTA DE ARCHIVOS')
  mes.actual <- substring(fecha.actual,6,7)
  
  mes.antes <- substring(fecha.anterior - cuantos.dias,6,7)
  
  fechas <- seq( from=fecha.anterior,to=fecha.actual,by='day' )
  
  fechas2 <- gsub('-','',fechas)
  
  anio <- str_sub(string=fechas2,start = 1,end = 4)
  mes <- str_sub(string=fechas2,start = 5,end = 6)
  
  sufjo = 'mercatorpsy4v3r1_gl12_so_'

  listaArchivos <- paste0(paste('E:/boletin/salinidad/',anio,mes,fechas2,sep = '/'),sufijo)
  
  existe <-unlist( lapply(listaArchivos, FUN=function(x){
    file.exists(x)
  }))
  
  indcBajar <- which(!existe)
  
  
  if (length(indcBajar)==0 ){
    return(list(listaArchivos, paste0('E:/boletin/datos/temp/',last(anio),'/figuras/')  ))
  }else{
    listaArchivosBajar <- paste0(ruta.ftp,anio[indcBajar],'/',mes[indcBajar],'/',fechas2[indcBajar],sufijo)
    
    mapply(function(x,y){
      carpeta <- dirname(x)
      if (!dir.exists(carpeta)){ dir.create(carpeta,recursive = TRUE)}
      download.file( url = y, destfile = x , method = "auto",
                     quiet = FALSE, mode="wb", cacheOK = TRUE  )
    },x=rev(listaArchivos[indcBajar]),y=rev(listaArchivosBajar) )
    
    return(list(listaArchivos, paste0('E:/boletin/datos/temp/',last(anio),'/figuras/')  ))
    
  }
  
 
}


