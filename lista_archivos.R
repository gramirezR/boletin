cat('\014')
rm(list=ls())
directorio <- 'D:/boletinDatos/'
variable <- 'viento'

dia.ini <- as.Date('2019-02-21')
dia.fin <- as.Date('2019-08-06')

lista.fechas <- format(seq(from=dia.ini,to=dia.fin,by='day'),'%Y%m%d')

switch(variable,
        viento ={
          horas <- c('06','12','18','00')
          f <- function(x,output){
            a <- x[1]
            b <- x[2]
            paste0(a,b)
          }
          M <- expand.grid(lista.fechas,horas)
          lista.fechas <- sort(apply(M,1,f))
          
          rm(list=c('M','horas'))
          patron <- '(-|_).+.nc'
        },
        temperatura={
          lista.fechas <- unlist(lapply(lista.fechas,function(x) paste0(x,'120000')))
          patron <- '-UKMO-.+.nc'
        },
        anm={
          patron <- '_[0-9]+.nc'
        })

lista.archivos <- list.files(path=paste0(directorio,variable),
                                  recursive = TRUE,full.names = TRUE)

presentes <- lapply(lista.fechas,
                    function(x) {
                      y <- grep(pattern = paste0(x,patron),x = lista.archivos)
                      if (length(y)>0){
                        y
                      }else{
                        'falta'
                      }
                    })

faltan <- which(unlist(lapply(presentes,function(x) any(x[1] == 'falta') )))

lista.fechas[faltan]

formato <- '2019073000-IFR-L4-EWSB-BlendedWind-GLO-025-6H-NRTv6-20190801T033931-fv1.0.nc'

