boletin_lista_salinidad <- function( dias, raiz){
  
  require('RCurl')
  print('PREPARANDO LISTA DE ARCHIVOS')
  
  
  ftp = 'ftp://gramirez2:$boletinDHN2018@nrt.cmems-du.eu/Core/GLOBAL_ANALYSIS_FORECAST_PHY_001_024/global-analysis-forecast-phy-001-024-3dinst-so'
  prefijo <- 'mercatorpsy4v3r1_gl12_so_'
  
  lista_archivos <- lapply(dias, FUN=function(x){
    anio <- lubridate::year(x)
    mes <- sprintf(lubridate::month(x),fmt = '%02d')
    dia <- sprintf(lubridate::day(x),fmt='%02d')
    dia2 <- sprintf(lubridate::day(x)+1,fmt='%02d')
    # dia_fin<- sprintf(lubridate::day(x)+1,fmt='%02d')
    arch1 <- paste0( raiz,prefijo,anio,mes,dia,'_12h_R',anio,mes,dia2,'.nc')
    # arch6 <- paste0( raiz,prefijo,anio,mes,dia_ini,'_06h_R',anio,mes,dia_fin,'.nc')
    # arch12 <- paste0( raiz,prefijo,anio,mes,dia_ini,'_12h_R',anio,mes,dia_fin,'.nc')
    # arch18 <- paste0( raiz,prefijo,anio,mes,dia_ini,'_18h_R',anio,mes,dia_fin,'.nc')
    # resultado <- rbind(arch1,arch6,arch12,arch18)
    resultado <- rbind(arch1)
    return( resultado )
  })
  
  lista_archivos <- do.call(what = rbind, args = lista_archivos )
  
  # lista_archivos
  
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
        # e <- simpleError("test error")
        tryCatch( download.file( url = archivo_descarga, destfile = x , method = "auto",
                                 quiet = FALSE, mode="wb", cacheOK = TRUE  ),
                  error = function(e){
                    print(x)
                    if (file.exists(x)){
                    file.remove(x)}
                  }, 
                  finally = print("Hello") )  }
      
    } ) }
  
  ###################################################################################
  
  indices_no_hay <- lapply(lista_archivos,FUN = function(x){
    file.exists(x)
  })
  
  indices_no_hay <- which(!do.call(what = rbind, args = indices_no_hay ))
  
  if ( length(indices_no_hay) ){
    lista_archivos <- lista_archivos[-indices_no_hay]
  }
  
  return( lista_archivos )
 
}


