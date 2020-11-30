bajarCopernico <- function( ruta.ftp
                           ,ruta.salida
                           ,fecha.actual
                           ,fecha.anterior
                           ,cuantos.dias
                           ,variable){
  
require('RCurl')
  
  mes.actual <- substring(fecha.actual,6,7)
  
   mes.antes <- substring(fecha.anterior - cuantos.dias,6,7)
  
      fechas <- seq( from=fecha.anterior,to=fecha.actual,by='day' )
  
     fechas2 <- gsub('-','',fechas)
  
  
  ##############LISTA DE ARCHIVOS DE COPERNICO################
  
  sufijo <- unique(substring(gsub('-','/',fechas),1,8))
  prefijo.cop <- ruta.ftp

  copernico <- sapply(sufijo , function(x) paste0(prefijo.cop,x) )
  datos.dir <- sapply(sufijo , function(x) paste0(ruta.salida,paste0(variable,'/'),x) )
  boletin.dir <- paste0(ruta.salida,paste0(variable,'/'),substring(fecha.actual,1,4),'/','figuras/')
  
  if (!dir.exists( boletin.dir )  ){
    dir.create(boletin.dir,recursive=TRUE)
  }
  
  #### DESCARGA DE ARCHIVOS ##############
  
  narchivos <- matrix(nrow = length(copernico),ncol=1)
  archivos <- vector('list',1)
  
  for (ii in 1:length(copernico)){
    
    if (!dir.exists(datos.dir[[ii]])){
      dir.create( datos.dir[[ii]],recursive = TRUE )
    }
    
    archivos[[ii]] <- getURL(copernico[[ii]],dirlistonly = TRUE,verbose=TRUE)
    
    
    archivos[[ii]] <- unlist(strsplit(archivos[[ii]],'\r\n'))
    
    lapply( archivos[[ii]],  function(x) if (!file.exists(paste0(datos.dir[[ii]], x))) {download.file( paste0(copernico[[ii]], x), paste0(datos.dir[[ii]], x),method = "auto",
                                                                                                       quiet = FALSE, mode="wb", cacheOK = TRUE  )}   )
    
  }
  
  
  
  ######## VERIFICAR QUE NO HAYA ARCHIVOS REPETIDOS PARA NIVEL DEL MAR ###########
  if ( variable=='anm'  ){
    borrar.archs <- vector('list',length(datos.dir))
    
    for (ii in 1:length(datos.dir)){
      lista <- list.files( datos.dir[[ii]] )
      nombre <- substring(lista,26,33)
      indc <- which(duplicated(nombre,fromLast=TRUE))
      if (length(indc)>0){
        borrar.archs[[ii]] <- lapply(lista[indc],function(x) paste0(datos.dir[[ii]], x ))
        lista <- lista[-indc] 
      }
      archivos[[ii]] <- lapply(lista,function(x) paste0(datos.dir[[ii]], x ))
    }
  }else{
    for (ii in 1:length(datos.dir)){
      lista <- list.files( datos.dir[[ii]] )
      archivos[[ii]] <- lapply(lista,function(x) paste0(datos.dir[[ii]], x ))
    }
  }
  ########### QUITAR DE LA LISTA ARCHIVOS CON MÁS TIEMPO DEL REQUERIDO #######
  
   archivos <- unlist(archivos)
  #   narchs <- length(archivos)
  #   dif <- abs( narchs - dias.atras )
  # if (narchs>dias.atras ){
  #   quitar <- 1:(dif)
  #   archivos <- archivos[-quitar]
  # }
  
  indc <- array(dim=length(fechas2))
  
  
  
  #######
  
  if ( variable=='anm'  ){
    
    for (ii in 1:length(fechas2)){
      try(indc[ii] <- grep(pattern = paste0('_',fechas2[ii],'_'),x=archivos,fixed = TRUE),silent=TRUE)
    }
    
    cuales <- which(is.na(indc))
    
    if (length(cuales)>0){
      indc <- indc[ -cuales ]
    }
    
    if ( length(indc)>0 ){
      salida <- list( archivos[indc],boletin.dir )
    }else{
      salida <- list(archivos,boletin.dir)   
    }
    
  }else{
    for (ii in 1:length(fechas2)){
      try(indc[ii] <- grep(pattern = fechas2[ii],x=archivos,fixed = TRUE),silent=TRUE)
    }
    cuales <- which(is.na(indc))
    if (length(cuales)>0){
      indc <- indc[ -cuales ]}
    
    if ( length(indc)>0 ){
      salida <- list( archivos[indc],boletin.dir )
    }else{
      salida <- list(archivos,boletin.dir)   
    }
  }
  
    return( salida)
  
}


