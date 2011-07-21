netDesign <- function(inpol, n, type){
#cambio de sincronizacion
require(maptools)
#2o cambio de syncrOniz
if(type=="hexagonal") {
#cambio para merge
pts.est <- spsample(inpol, 1.2*n, type)
while (summary(pts.est)[5]$npoints != n) pts.est <- spsample(inpol, 1.2*n, type)
} else {
pts.est <- spsample(inpol, n, type)
while (summary(pts.est)[5]$npoints != n) pts.est <- spsample(inpol, n, type)
}
return(pts.est)
}

