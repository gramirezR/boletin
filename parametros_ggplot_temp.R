
fecha <- copernicus_temp$fecha[2]

archivo_salida <- paste0( 'E:/boletin/datos/temp/2022/figuras/mapa_costero_tsm_mesual_',fecha,'.png'  )

########################

titulo <- paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                 'Dpto. de Oceanografía')

subtitulo <- paste0('Temperatura Superficial del Mar: del ' ,
                    format( copernicus_temp$fecha[1], format='%d-%B'),' al ',
                    format( copernicus_temp$fecha[2], format='%d-%B-%Y') )

creditos <- 'Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 1981-2009'

#########################

paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')

niveles <- seq(10,30,by=1)

niveles_barra <- seq(10,30,by=1)
################

load('E:/programasR/boletin/costa_Peru_202.RData') 

marcas_x <- seq(copernicus_temp$limites$lon[1],
                copernicus_temp$limites$lon[2],by=5)

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

marcas_y <- seq(from=copernicus_temp$limites$lat[1],
                  to=copernicus_temp$limites$lat[2],by=5)

etiquetas_y <- unlist( lapply( as.vector(marcas_y),
                              function(x) if( x<0 ) {paste0(-x,'S')}else if(x>0){paste0(x,'N')}else{x} )   )


unidades <- '°C'
#############



