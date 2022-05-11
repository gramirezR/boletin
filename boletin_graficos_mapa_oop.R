#####################

Mapa_boletin = setRefClass("Mapa_boletin",
                           fields = list(
                                         malla = 'list',
                                    parametros = 'list'
                                        )
                           )


Mapa_boletin$methods(
  crear_mapa = function(){
    if (length(.self$malla)==3){
        datos <- .self$malla[[1]]
      datos$U <- .self$malla[[2]]$layer
      datos$V <- .self$malla[[3]]$layer
    }else{
      datos <- .self$malla[[1]]
      print(datos)
    }
    options(encoding = "UTF-8")
    print(.self$parametros$archivo_salida)
    pp <- ggplot2::ggplot( data = datos,
                            aes( x = x,
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
      if( length(.self$malla)==3 ){
        
          pp <-  pp + ggquiver::geom_quiver( aes( u = U, v = V ),
                                             vecsize = 2,
                                                size = 0.5  ) 
                                  }
      
pp <- pp + ggplot2::geom_polygon( data = .self$parametros$costa,
                     aes( x = long,
                          y = lat,
                      group = group ),
                    color = 'black',
                    fill = 'grey80',
                    inherit.aes = FALSE  )
    
    for ( kk in 1:length( .self$parametros$fronteras )){
      f <- as.data.frame( .self$parametros$fronteras[[ kk ]] )
      pp <- pp + ggplot2::geom_point(data = f, aes( x = X1,
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
      ggplot2::scale_y_continuous( limits = c( -20, 2 ),
                          expand = c( 0, 0 ),
                          breaks = .self$parametros$marcas_y,
                          labels = .self$parametros$etiquetas_y ) +
      ggplot2::labs(   x = 'Longitud',
                       y = 'Latitud',
                   title = .self$parametros$titulo,
                subtitle = .self$parametros$subtitulo,
                 caption = .self$parametros$creditos
      ) +
      ggplot2::theme( axis.title.x = element_text( size = 28,
                                                  hjust = 0.5 ),
             axis.title.y = element_text( size = 28,
                                         hjust = 0.5 ),
             axis.text = element_text( size = 28,
                                     colour = 'black'),
             title = element_text( size = 30 ),
             plot.subtitle = element_text( size = 32 ),
             plot.caption = element_text(  size = 22,
                                          hjust = 0 )
      ) +
      ggplot2::guides( fill = guide_colorbar(    barheight = unit(20, "cm"),
                                         barwidth = unit(1.5,'cm'),
                                      label.theme = element_text(size=26),
                                            title = .self$parametros$unidades,
                                      title.theme = element_text(size=26)
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


