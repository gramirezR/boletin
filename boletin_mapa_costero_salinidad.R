boletin_mapa_costero_salinidad <- function(lista_archivos,fecha.inicial,fecha.final){

lim.lon <- c(270, 290) - 360
lim.lat <- c(-20, 2)

boyas <- data.frame(x=c(-85,-85),y=c(-5,-8))

ncin <- open.nc( lista_archivos[1] )

# print.nc(ncin)

lon.p <- var.get.nc(ncin,'longitude')
lat.p <- var.get.nc(ncin,'latitude')

indc.lon <- which(lon.p >= lim.lon[1] & lon.p <= lim.lon[2])
indc.lat <- which(lat.p >= lim.lat[1] & lat.p <= lim.lat[2])

lon <- lon.p[indc.lon]
lat <- lat.p[indc.lat]

rm('lon.p')
rm('lat.p')

A <- var.get.nc(ncin,'so',start = c(indc.lon[1],indc.lat[2],1,1),
                count = c(length(indc.lon), length(indc.lat), 1, 1), collapse = TRUE, unpack = TRUE)
############### PROMEDIO DE LOS ARCHIVOS #######################  
close.nc(ncin)
.GlobalEnv$narchs <- length(lista_archivos)
# narchs <- length(lista_archivos)
if (narchs>1){
  for (ii in 2:narchs){
    
    arch <- lista_archivos[ii]
    tryCatch(
      {
      ncin <- open.nc(arch)
      A <- var.get.nc(ncin,'so',start = c(indc.lon[1],indc.lat[2],1,1),
                      count = c(length(indc.lon), length(indc.lat), 1, 1), collapse = TRUE, unpack = TRUE) + A
      close.nc(ncin)
      },
      error = function(e) .GlobalEnv$narchs <- .GlobalEnv$narchs - 1 ,
      finally = print("Hello")
    )
  }}

A <- A/.GlobalEnv$narchs
#########################################

malla <- expand.grid(lon=lon,lat=lat)

Z <- stack( as.data.frame(A) )

mapa <- data.frame( lon = as.matrix(malla$lon),
                    lat = as.matrix(malla$lat),
                    z = as.matrix(Z$values)  )

#######################################
load('costa_Peru_202.RDat') 
Dt <- 0.5
npts <- 10
tamanio <- 10

suave <- mgcv::gam(z~te(lon,lat,k=c(15,15)),data=mapa,na.action = 'na.omit')

pred <- predict(suave,newdata = as.data.frame(malla))
Z <- as.matrix( pred )
mapa <- data.frame(     lon = malla$lon,
                        lat = malla$lat,
                        z = Z)

costa <- shore[ shore$group=='3.1', ]

indc <- which( point.in.polygon( malla$lon,malla$lat,
                                 costa$long-360,costa$lat )==1 )

mapa$z[indc] <- NA

rm('pred')
rm('suave')
##########################################

niveles <- c(seq(31,34.7,by=0.1),seq(34.8,35.1,by=0.1),seq(35.2,36.2,by=0.1))
niveles_barra <- c(seq(from=31,to=34.7,by=0.4),c(34.8,35.1),seq(35.4,36.2,by=0.4))
subtitulo.graf <- 'Salinidad'
titulo.barra <- 'Salinidad (UPS)'

subtitulo.grafico <- paste0(subtitulo.graf,': del ',
                            format(fecha.inicial, format='%d-%B'),' al ',
                            format(fecha.final, format='%d-%B-%Y'))

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
# paleta03 <- colorRampPalette(colors = c("#88A52F","#7FAC25","#B9E1AC"))(11)
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
  
  
# paleta_color <- cptcity::cpt('ncl_StepSeq25')#[20:100]
# paleta_color <- cptcity::cpt('ncl_StepSeq25')
# paleta_color <- cptcity::cpt('cb_div_BrBG_11')
# paleta_color <- 
#paleta_color <- cptcity::cpt('cmocean_haline')
##########################################


if ( exists('pp')){
  rm(pp)  
}
marcas_x <- seq(lim.lon[1],lim.lon[2],by=5)

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

marcas_y <- seq(from=lim.lat[1],to=lim.lat[2],by=5)
etiquetas_y <- unlist(lapply( as.vector(marcas_y),
                              function(x) if( x<0 ) {paste0(-x,'S')}else if(x>0){paste0(x,'N')}else{x} )   )
##################################
png(width=1200, height=1200, filename = paste0(raiz,'figuras/salinidad',fecha.final,'.png'  ))
pp <- ggplot( data=mapa,aes(x=lon,y=lat,fill=z) )
pp <- pp + geom_raster(aes(fill = z),interpolate=TRUE,show.legend = TRUE  )
pp <- pp + scale_fill_gradientn( colours = paleta_color,
                                  limits = range(niveles),
                                  breaks = niveles_barra)
pp <- pp + stat_contour(data=mapa, aes(x=lon,y=lat,z=z),
                        linetype = 1 ,
                        col = 'black' ,
                        inherit.aes = FALSE,
                        breaks = niveles)

pp <- pp + geom_polygon( data=shore,aes(x=long-360,y=lat,group=group),color = 'black', fill = 'grey80',inherit.aes=FALSE  )
for (kk in 1:length(fronteras)){
  f <- as.data.frame(fronteras[[kk]])
  pp <- pp + geom_point(data=f,aes( x=X1-360,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
}

pp <- pp + geom_text_contour(aes(z=z),
                             stroke = 0.1,skip=0,min.size =2,size=tamanio,rotate = FALSE,
                             check_overlap=TRUE,breaks = niveles)


pp <- pp + geom_point(data=boyas,aes(x=x,y=y),color='black',size=5,inherit.aes=FALSE)  
pp <- pp + geom_text(data=boyas,aes(x=x,y=y-0.4,label=c('B1','B2')),
                     color='black',size=12,inherit.aes=FALSE)

pp <- pp + scale_x_continuous(limits = range(marcas_x),
                              expand = c(0,0),
                              breaks = marcas_x,
                              labels = etiquetas_x)
pp <- pp + scale_y_continuous(limits = lim.lat,
                              expand = c(0,0),
                              breaks = marcas_y,
                              labels = etiquetas_y)
pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                             'Dpto. de Oceanografía - Div. Oceanografía'),
                subtitle = subtitulo.grafico,
                caption = 'Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 1981-2009')
#pp <- pp + coord_cartesian(xlim=c(282,283),ylim=c(-13,-10))
pp <- pp + theme_bw(   )

pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                  axis.title.y = element_text( size=28,hjust=0.5  ),
                  axis.text = element_text(size=28,colour = 'black'),
                  title=element_text(size=28),
                  plot.subtitle=element_text(size=24),
                  plot.caption = element_text(size = 22,hjust = 0))

pp <- pp + guides( fill = guide_colorbar(  barheight = unit(20, "cm"),
                                           barwidth = unit(1.5,'cm'),
                                           label.theme = element_text(size=26),
                                           title = titulo.barra,
                                           title.theme = element_text(size=26)))

plot(pp)

dev.off()

}