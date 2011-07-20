netDesign <- function(inpol, n, type){
require(maptools)
#2 cambio
if(type=="hexagonal") {
pts.est <- spsample(inpol, 1.2*n, type)
while (summary(pts.est)[5]$npoints != n) pts.est <- spsample(inpol, 1.2*n, type)
} else {
#3 cambio
pts.est <- spsample(inpol, n, type)
while (summary(pts.est)[5]$npoints != n) pts.est <- spsample(inpol, n, type)
}
#posible cambio
return(pts.est)
}

