#####################

Mapa_boletin = setRefClass("Mapa_boletin",
                           fields = list(
                                         malla = 'list',
                                    parametros = 'list'
                                        )
                           )


Mapa_boletin$methods(
  crear_mapa = function(){
    if ( ncol( .self$malla[[1]] ) == 5 ){
        datos <- .self$malla[[1]]
        names(datos) <- c('x','y','layer', 'U', 'V')
    }else{
      datos <- .self$malla[[1]]
      print(datos)
    }
    options(encoding = "UTF-8")
    print(.self$parametros$archivo_salida)
    pp <- ggplot2::ggplot( data = datos,
                           ggplot2::aes( x = x,
                                 y = y,
                                 z = layer,
                              fill = layer)
                            ) + 
      ggplot2::geom_raster( interpolate = TRUE,
                              show.legend = TRUE  ) + 
      ggplot2::scale_fill_gradientn( colours = .self$parametros$paleta_color,
                            limits = range(.self$parametros$niveles),
                            breaks = .self$parametros$niveles_barra) +
      ggplot2::stat_contour( linetype = 1 ,
                    col = 'black' ,
                    breaks = .self$parametros$niveles)
      if( ncol(datos)==5 ){
          ndts <- nrow(datos)
          indc2 <- seq(1,ncol(datos), by = 15)
          indc <- vector( mode = 'list', length = length(indc2) )
          for (ii in indc2 ){
            indc[[ii]] <- (ii-1)*ndts + seq(1, ndts, by = 30) 
          }
          indc <- do.call(what = rbind, args = indc)
          campo <- datos[indc,]
          pp <-  pp + ggquiver::geom_quiver(data = campo,
                                            ggplot2::aes(x=x, y=y, u = U, v = V ),
                                         vecsize = 5,
                                            size = 1,
                                     inherit.aes = FALSE) 
                                  }
      
pp <- pp + ggplot2::geom_polygon( data = .self$parametros$costa,
            ggplot2::aes( x = long,
                          y = lat,
                      group = group ),
                    color = 'black',
                    fill = 'grey80',
                    inherit.aes = FALSE  )
    
    for ( kk in 1:length( .self$parametros$fronteras )){
      f <- as.data.frame( .self$parametros$fronteras[[ kk ]] )
      pp <- pp + ggplot2::geom_point(data = f, ggplot2::aes( x = X1,
                                            y = X2 ),
                             col = 'grey30',
                             size = 0.05,
                             inherit.aes = FALSE )
    }
    
    pp <- pp + ggplot2::scale_x_continuous( limits = range( .self$parametros$marcas_x ),
                                   expand = c(0,0),
                                   breaks = .self$parametros$marcas_x,
                                   labels = .self$parametros$etiquetas_x
    ) + 
      ggplot2::scale_y_continuous( limits = range( .self$parametros$marcas_y ),
                          expand = c( 0, 0 ),
                          breaks = .self$parametros$marcas_y,
                          labels = .self$parametros$etiquetas_y ) +
      ggplot2::labs(   x = 'Longitud',
                       y = 'Latitud',
                   title = .self$parametros$titulo,
                subtitle = .self$parametros$subtitulo,
                 caption = .self$parametros$creditos
      ) +
      ggplot2::theme( axis.title.x = ggplot2::element_text( size = 28, hjust = 0.5 ),
             axis.title.y = ggplot2::element_text( size = 28, hjust = 0.5 ),
             axis.text = ggplot2::element_text( size = 24, colour = 'black'),
             title = ggplot2::element_text( size = 30 ),
             plot.subtitle = ggplot2::element_text( size = 32 ),
             plot.caption = ggplot2::element_text(  size = 22,
                                          hjust = 0 )
      ) +
      ggplot2::guides( fill = ggplot2::guide_colorbar(    barheight = ggplot2::unit(20, "cm"),
                                         barwidth = ggplot2::unit(1.5,'cm'),
                                      label.theme = ggplot2::element_text(size=26),
                                            title = .self$parametros$unidades,
                                      title.theme = ggplot2::element_text(size=26)
      )
      )
    png(   width = .self$parametros$ancho,
           height = .self$parametros$alto,
           filename = .self$parametros$archivo_salida
    )
    plot(pp)
    dev.off()
    print(paste0( 'SALIDA EN EL ARCHIVO: ', .self$parametros$archivo_salida ))

    }
)
###################################


