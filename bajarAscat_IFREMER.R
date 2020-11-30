cat('\014') 
graphics.off()
rm(list=ls())
library('RCurl')

anio <- '2019'

fecha.actual <- lubridate::today() - 2

dias.atras <- 5

fecha.anterior <- fecha.actual - dias.atras


lista.dias <- seq(from=fecha.anterior,to=fecha.actual,by='day')

datos.dir <- paste0('C:/Users/gramirez/ANM/boletin/viento/datos/')

ftp.dir <- lapply(lista.dias,function(x) paste0('ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/MWF/L3/ASCAT/Daily/Netcdf/',
                                      substring(x,1,4),'/',substring(x,6,7),'/',substring(x,9,10),'/'))


            lapply( ftp.dir[[1]],function(x) {
                         nombre.arch <- strsplit(getURL(x,dirlistonly = TRUE,verbose=TRUE),'\r\n')
                         bajar <- paste0(x,nombre.arch)
                         download.file( url=bajar, destfile = paste0(datos.dir,nombre.arch),
                                        method = "auto",
                                        quiet = FALSE, mode="wb", cacheOK = TRUE  )
                         } )
            
            # 
            # download.file( bajar, paste0(datos.dir,nombre.arch),
            #                method = "auto",
            #                quiet = FALSE, mode="wb", cacheOK = TRUE  )
            # R.utils::bunzip2(paste0(datos.dir,nombre.arch),
            #                  destname=paste0(datos.dir,str_extract(lista,pattern='.*[^.bz2]')),
            #                  skip  =FALSE,
            #                  remove=TRUE,
            #                  overwrite=TRUE)  
            
closeAllConnections()

lista.dias <- lista <- unlist(strsplit(lista.meses,'\r\n'))



carpeta <- paste0('C:/Users/gramirez/ANM/boletin/viento/',anio,'/',mes,'/')
figuras <- paste0('C:/Users/gramirez/ANM/boletin/viento/',anio,'/figuras/')

if (!dir.exists(carpeta)){
  dir.create(carpeta,recursive = TRUE)
}


