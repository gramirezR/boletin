Hovmoller_ecuatorial_boletin = setRefClass("Hovmoller_ecuatorial_boletin",
                            fields = list(
                             malla = 'list',
                        parametros = 'list'
                           )
)


Hovmoller_ecuatorial_boletin$methods(
  crear_hov = function(){
    
    
    rstr <- .self$malla[[1]]
    rstr <- raster::crop(rstr, raster::extent( .self$parametros$limites  )  )
    
    capas <- as.list(.self$parametros$factor_unidades*rstr)
    
    # print( capas[[1]]  ) 
    
    proms <- lapply( capas,
                     FUN = function(x) apply( as.matrix(x), 2 , mean, na.rm = TRUE) )
    
    proms <- do.call( cbind, proms )
    
    proms <- apply( proms , MARGIN = 1,
                    FUN=function(x) {
                      y_wavelet <-   WaveletComp::analyze.wavelet(my.data = data.frame(x=x),
                                                                  loess.span = 0.4,
                                                                  dt = 1, dj = 1/250,
                                                                  lowerPeriod = 10,
                                                                  upperPeriod = 120,
                                                                  make.pval = FALSE,
                                                                  n.sim=10,
                                                                  verbose = FALSE)
                      
                      y_rec <- WaveletComp::reconstruct(y_wavelet,
                                                        plot.waves = FALSE,
                                                        verbose = FALSE )
                      graphics.off()
                      resultado <- y_rec$series$x.r
                      return( resultado )
                    }
    ) 
    
    proms <- stack(as.data.frame(t(proms)))$values
    
    coords <- raster::xyFromCell(object = rstr,
                                 cell = 1:ncol(capas[[1]]))[,1]
    
    tiempo <- raster::getZ( rstr )
    
    malla2 <- expand.grid( y=coords,
                           x=tiempo )
    
    hov <- cbind( malla2, proms )
    T <- hov$x
    
    hov$x <- as.numeric(hov$x)
    malla2$x <- hov$x
    
    modelo <- mgcv::gam(proms ~ te(x, y, k=c(30,30) ), data = hov)
    pred <- predict(modelo,newdata = as.data.frame(malla2))
    
    malla2$x <- T
    hov <- cbind( malla2, pred )
    
###################################
    
    fktr <- 10
    limites <- c(-1,1)*fktr
    marcas <- seq(-1, 1, length.out = 9)*fktr
    
    paleta_color <- cptcity::cpt(.self$parametros$paleta_color, n = 19)
    paleta_color <- c(paleta_color[1:10],
                      '#FFFFFF',
                      '#FFFFFF',
                      paleta_color[11:19])
    contornos <- seq( from=-10,to=10,by=1 )
    graphics.off()
    png(width = 2400,
       height = 1200,
         file = .self$parametros$png_salida )
    pp <- ggplot2::ggplot(data = hov,
                          ggplot2::aes(x = x, y = y, fill=pred, z=pred)) 
    pp <- pp + ggplot2::geom_raster() 
    pp <- pp + ggplot2::scale_fill_gradientn(
                                   colours = paleta_color,
                                    limits = limites,
                                    breaks = marcas)
    pp <- pp + ggplot2::stat_contour( breaks = seq(-12, 10, by = 1),
                                         col = 'black' )
    pp <- pp + ggplot2::scale_y_reverse(  expand = c(0,0) )
    pp <- pp + ggplot2::scale_x_datetime( expand = c(0,0),
                                          date_breaks = 'month',
                                          date_labels = '%b')
    pp <- pp + ggplot2::labs(  y = 'Longitud',
                               x = paste0('Fecha'),
                           title = paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                                     'Dpto. de Oceanografía - Div. Oceanografía'),
                      subtitle = expression( Zona~Ecuatorial~2*degree~N~a~2*degree~S),
                      caption = 'Fuente: COPERNICUS MARINE\nENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 1981-2009')
    
    pp <- pp + ggplot2::theme( 
                       axis.text.x = ggplot2::element_text( size = 34, color='black' ),
                       axis.text.y = ggplot2::element_text( size = 34, color='black', hjust = 0.5 ),
                      axis.title.x = ggplot2::element_text( size = 40 ),
                      axis.title.y = ggplot2::element_text( size = 40 ),
                             title = ggplot2::element_text( size = 38 ),
                      plot.caption = ggplot2::element_text( size = 28, hjust = 0))
    
    pp <- pp + ggplot2::guides( 
                       fill = ggplot2::guide_legend(barheight = ggplot2::unit(25, "cm"),
                                                      reverse = TRUE,
                                                     barwidth = ggplot2::unit(2,'cm'),
                                                  label.theme = ggplot2::element_text(size=30),
                                                        title = .self$parametros$titulo_barra,
                                                  title.theme = ggplot2::element_text(size=30)))  
    plot(pp)
    dev.off()
    
    
    
# FIN MODULO
    }

)

############################################


Hovmoller_costero_boletin = setRefClass("Hovmoller_costero_boletin",
                                                 fields = list(
                                                  malla = 'list',
                                             parametros = 'list'
                                           )
)


Hovmoller_costero_boletin$methods(
  crear_hov = function(){
    rstr <- .self$malla[[1]]
    poligono <- .self$parametros$poligono
    costa_raster <- raster::mask( rstr, poligono )
    costa_raster <- raster::crop(costa_raster, y=raster::extent(poligono) )
    
    capas <- as.list(100*costa_raster)
    
    proms <- lapply( capas,FUN = function(x) apply( as.matrix(x), 1 , mean, na.rm = TRUE) )
    
    proms <- do.call( rbind, proms )
    
    dim_proms <- dim(proms)
    
    proms <- stack(as.data.frame(t(proms)))$values
    
    coords <- raster::xyFromCell(object=costa_raster[[1]],
                                 cell = 1:raster::ncell(costa_raster[[1]]))
    
    coords <- unique(coords[,2])
    
    tiempo <- raster::getZ( rstr )
    
    malla2 <- expand.grid( y=coords, x=tiempo )
    
    hov <- cbind( malla2, proms )
    
    ##########################
    
    indcna <- which(is.na(hov$proms))
    
    T <- hov$x
    
    hov$x <- as.numeric(hov$x)
    malla2$x <- hov$x
    
    hov_mat <- matrix(data=hov$proms, ncol=dim_proms[1], nrow=dim_proms[2])
    
    # print(dim(hov_mat))
    
    hov_mat <- apply( hov_mat, 2,
                      FUN=function(x){
                        indc <- which( is.na(x)  )
                        if (length(indc)>0){
                          xp <- 1:length(x)
                          yy <- x[-indc]
                          xx <- xp[-indc]
                          df <- data.frame( xx=xx, yy=yy )
                          modelo <- loess(formula = yy~xx, data = df, span=0.05 )
                          resultado <- predict( modelo, newdata=data.frame( xx=xp) )
                          y <-  resultado
                        }else{
                          y <- x
                          # print(y)
                        }
                        return(y)
                      } )
    
    #####################################
    
    hov2 <- apply( hov_mat , MARGIN = 1,
                   FUN=function(x) {
                     y_wavelet <-   WaveletComp::analyze.wavelet(my.data = data.frame(x=x),
                                                                 loess.span = 0.4,
                                                                 dt = 1, dj = 1/250,
                                                                 lowerPeriod = 10,
                                                                 upperPeriod = 120,
                                                                 make.pval = FALSE,
                                                                 n.sim=10,
                                                                 verbose = FALSE)
                     
                     y_rec <- WaveletComp::reconstruct(y_wavelet,
                                                       plot.waves = FALSE,
                                                       verbose = FALSE )
                     graphics.off()
                     resultado <- y_rec$series$x.r
                     return( resultado )
                   }
    )
    
    ################################
    
    hov2 <- stack( as.data.frame( t(hov2) ) )$values
    
    # print( length(hov2) )
    # print( nrow(malla2) )
    
    hov2 <- cbind( malla2, hov2 )
    
    hov2$x <- as.POSIXct(hov2$x, origin = '1970-01-01')
    
    names(hov2) <- c('y', 'x', 'proms')
    
    ###############
    marcas <- seq(-10, 10, 2)
    limites <- range(marcas)
    paleta_color <- cptcity::cpt(.self$parametros$paleta_color, n= 10)
    paleta_color <- c(paleta_color[1:5],
                      '#FFFFFF',
                      paleta_color[6:10])
    graphics.off()
    png(width = 2400, height=1200, file = .self$parametros$png_salida)
    pp <- ggplot2::ggplot(data=hov2,
                          ggplot2::aes(x = x, y = y, fill=proms, z=proms) ) 
    pp <- pp + ggplot2::geom_raster(interpolate = TRUE) 
    pp <- pp + ggplot2::scale_fill_gradientn(colours = paleta_color,
                                              limits = limites,
                                              breaks = marcas )
    pp <- pp + ggplot2::stat_contour( breaks = seq(-10, 10, by = 1),
                             col = 'black' )
    
    pp <- pp + ggplot2::scale_x_datetime(date_breaks = 'month',
                                date_labels = '%b',
                                expand=c(0,0)
    )
    pp <- pp + ggplot2::scale_y_continuous(expand = c(0,0))
    
    pp <- pp + ggplot2::labs(  y = 'Latitud',
                      x = paste0('Fecha'),
                      title = paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                                     'Dpto. de Oceanografía - Div. Oceanografía'),
                      subtitle = 'Franja Costera de 50mi',
                      caption = 'Fuente: COPERNICUS MARINE\nENVIRONMENT MONITORING SERVICE (CMEMS v3.0).\nClimatología: 1981-2009')
    
    pp <- pp + ggplot2::theme( axis.text.x = ggplot2::element_text( size = 34, color='black' ),
                      axis.text.y = ggplot2::element_text( size = 34, color='black', hjust = 0.5 ),
                      axis.title.x = ggplot2::element_text( size = 40 ),
                      axis.title.y = ggplot2::element_text( size = 40 ),
                      title = ggplot2::element_text( size = 38 ),
                      plot.caption = ggplot2::element_text( size = 28, hjust = 0))
    
    pp <- pp + ggplot2::guides( fill = ggplot2::guide_legend(barheight = ggplot2::unit(25, "cm"),
                                           barwidth = ggplot2::unit(2,'cm'),
                                           label.theme = ggplot2::element_text(size=30),
                                           title = .self$parametros$titulo_barra,
                                           title.theme = ggplot2::element_text(size=30),
                                           reverse = TRUE) ) 
    
    
    plot(pp)
    dev.off()
  })





