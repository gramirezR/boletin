
fecha <- copernicus_sal$fecha[2]

archivo_salida <- paste0( 'E:/boletin/salinidad/figuras/salinidad_',fecha,'.png'  )

########################

titulo <- paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                 'Dpto. de Oceanografía')

subtitulo <- paste0('Salinidad: del ' ,
                            format( copernicus_sal$fecha[1], format='%d-%B'),' al ',
                            format( copernicus_sal$fecha[2], format='%d-%B-%Y') )

creditos <- 'Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 1981-2009'

#########################

paleta01 <- colorRampPalette(colors = c("#FF0000","#FF0200","#FF0500","#FF0700","#FF0A00","#FF0C00","#FF0F00","#FF1200","#FF1400",
                                        "#FF1700","#FF1900","#FF1C00","#FF1E00","#FF2100","#FF2400","#FF2600","#FF2900","#FF2B00",
                                        "#FF2E00","#FF3000","#FF3300","#FF3600","#FF3800","#FF3B00","#FF3D00","#FF4000","#FF4200",
                                        "#FF4500","#FF4800","#FF4A00","#FF4D00","#FF4F00","#FF5200","#FF5500","#FF5700","#FF5A00",
                                        "#FF5C00","#FF5F00","#FF6100","#FF6400","#FF6700","#FF6900","#FF6C00","#FF6E00","#FF7100",
                                        "#FF7300","#FF7600","#FF7900","#FF7B00","#FF7E00","#FF8000","#FF8300","#FF8500","#FF8800",
                                        "#FF8B00","#FF8D00","#FF9000","#FF9200","#FF9500","#FF9700","#FF9A00","#FF9D00","#FF9F00",
                                        "#FFA200","#FFA400","#FFA700","#FFAA00","#FFAC00","#FFAF00","#FFB100","#FFB400","#FFB600",
                                        "#FFB900","#FFBC00","#FFBE00","#FFC100","#FFC300","#FFC600","#FFC800","#FFCB00","#FFCE00",
                                        "#FFD000","#FFD300","#FFD500","#FFD800","#FFDA00","#FFDD00","#FFE000","#FFE200","#FFE500",
                                        "#FFE700","#FFEA00","#FFEC00","#FFEF00","#FFF200","#FFF400","#FFF700","#FFF900","#FFFC00",
                                        "#FFFF00"))

paleta02 <- colorRampPalette(colors = c('#00BFFF','#0000CD'))

paleta03 <- colorRampPalette(colors = c("#007F66","#028066","#058166","#078266","#0A8366","#0C8566","#0F8666","#118766","#148866",
                                        "#168A66","#198B66","#1B8D66","#1E8E66","#208F66","#239166","#259266","#289366","#2B9566",
                                        "#2E9666","#309766","#339966","#369A66","#389B66","#3B9C66","#3D9E66","#409F66","#42A066",
                                        "#45A166","#47A366","#4AA466","#4CA566","#4FA766","#51A866","#54AA66","#56AB66","#59AC66",
                                        "#5CAD66","#5EAE66","#61B066","#64B166","#67B266","#69B366","#6CB466","#6EB666","#71B766",
                                        "#73B866","#76B966","#78BB66","#7BBC66","#7DBD66","#80BF66","#82C066","#85C166","#87C366",
                                        "#8AC466","#8DC666","#8FC766","#92C866","#95CA66","#97CB66","#9ACC66","#9DCD66","#9FCF66",
                                        "#A2D066","#A4D166","#A7D266","#AAD466","#ACD566","#AED666","#B1D866","#B3D966","#B6DA66",
                                        "#B8DC66","#BBDD66","#BDDE66","#C0DF66","#C3E166","#C6E266","#C8E366","#CBE466","#CEE566",
                                        "#D0E766","#D3E866","#D5E966","#D8EA66","#DAEC66","#DDED66","#DFEE66","#E2F066","#E4F166",
                                        "#E7F266","#E9F466","#ECF566","#EEF666","#F1F866","#F4F966","#F6FA66","#F9FC66","#FCFD66",
                                        "#FFFF66"))

paleta_color <- c(rev(paleta03(38)), paleta02(4), paleta01(11))

niveles <- c(seq(31,34.7,by=0.1),seq(34.8,35.1,by=0.1),seq(35.2,36.2,by=0.1))

niveles_barra <- c(seq(from=31,to=34.7,by=0.4),c(34.8,35.1),seq(35.4,36.2,by=0.4))

################

load('E:/programasR/boletin/costa_Peru_202.RData') 

marcas_x <- seq(from = copernicus_sal$limites$lon[1],
                to = copernicus_sal$limites$lon[2],
                by = 5)

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

marcas_y <- seq(from = copernicus_sal$limites$lat[1],
                to = copernicus_sal$limites$lat[2],
                by = 5)

etiquetas_y <- unlist(lapply( as.vector(marcas_y),
                              function(x) {
                                if( x<0 ) {
                                  paste0(-x,'S') 
                                }else{
                                  if(x>0){ paste0(x,'N') 
                                    }else{ x }
                                     } }
                              )   )

unidades <- 'UPS'
#############



