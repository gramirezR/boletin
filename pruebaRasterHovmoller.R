library(raster)
r <- raster()
values(r) <-1:ncell(r)
zones <- init(r, v='y')
z <- zonal(r, zones, 'mean')
plot(z)

#################

zonas <- init(x.raster, fun='row')
plot(zonas)
z <- stackApply(x.raster,indices = 1:nlayers(x.raster),
                fun = function(x,na.rm=TRUE) zonal(x, zonas, 'mean'),
                filename='temporal')
plot(z)
