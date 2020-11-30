separar_por_mes <- function(lista_archivos){

# 2010012600_2010012700_daily-ifremer-L3-MWF-GLO-20110413053137-01.0
  lista_por_mes <- vector(mode='list',length=12)
  for (ii in 1:12) {
       indc <- which(str_sub(lista_archivos,5,6)==sprintf(ii,fmt = '%02g'))
       lista_por_mes[[ii]] <- lista_archivos[indc]
   }
   return(lista_por_mes) 
  }