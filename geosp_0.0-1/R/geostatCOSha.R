## source myFunctions_0.0-1
# library(foreign) 
library(gstat)
setwd("D:/Tesis/Data/")
load("COSha2.Rdata")

load("ptspred.Rdata")
load("cveg07.Rdata")


# COSha102, se eliminan todas las observaciones de Bn
COSha103 <- COSha102[-(which(COSha102[,"COB1r"]=="Bn")),]
write.dbf(COSha103, file="COSha103.dbf")
COSha103 <- read.dbf("COSha103.dbf")
#COSha103 <- COSha103[-(which(COSha103=="S131")),]
myDat <- data.frame(id=COSha103$SITIO, x = COSha103$X, y = COSha103$Y, myVar = COSha103$CorT)
# se optimiza el ancho para estimar el variograma con la funcion opt.width.MIN.c0
# mejor lineal, rango 367.1513 m
ve <- variogram(myVar~1, loc=~x+y, data=myDat, width = 226.5936)
PSI <- 0.0006; RAN <- 1000; NUG <- 0.0004
# mejor gaussiano, rango 180.2469 m
ve <- variogram(myVar~1, loc=~x+y, data=myDat, width = 230.3071)
PSI <- 0.0006; RAN <- 1000; NUG <- 0.0004
# mejor exponencial, rango 148.6841 m, buen esferico
ve <- variogram(myVar~1, loc=~x+y, data=myDat, width =  225.3887)
PSI <- 0.0006; RAN <- 1000; NUG <- 0.0004
# mejor esferico, rango 434.0656 m, buen esferico
ve <- variogram(myVar~1, loc=~x+y, data=myDat, width =  226.4468)
PSI <- 0.0006; RAN <- 1000; NUG <- 0.0004


# COSha302, modelos resultantes Co < C1
myDat <- data.frame(id=COSha302$SITIO, x = COSha302$X, y = COSha302$Y, myVar = COSha302$CorT)
#ve <- variogram(myVar~1, loc=~x+y, data=myDat)
ve <- variogram(myVar~1, loc=~x+y, data=myDat, width = 130)
#PSI <- 0.4; RAN <- 976.3934; NUG <- 0.2
#PSI <- 0.0001; RAN <- 1000; NUG <- 0.00005
PSI <- 0.0003; RAN <- 1000; NUG <- 0.00001

plot(ve, plot.numbers=T, xlab="Distancia (m)", ylab="Semivarianza")
(max(dist(myDat[,2:3]))/3)/15


m.lin <- vgm(psill=PSI, model="Lin", range=RAN, nugget=NUG)	# Lineal
m.f.lin <- fit.variogram(object=ve, model=m.lin)
m.gaus <- vgm(PSI, "Gau", RAN, NUG)		# Gaussiano
m.f.gaus <- fit.variogram(ve, m.gaus)
m.exp <- vgm(PSI, "Exp", RAN, NUG)			# Exponencial
m.f.exp <- fit.variogram(ve, m.exp)
m.esf <- vgm(PSI, "Sph", RAN, NUG)			# Esférico
m.f.esf <- fit.variogram(ve, m.esf)

m.f.lin; m.f.gaus; m.f.exp; m.f.esf
m.f.esf[1,2]/sum(m.f.esf[2])


v.cl <- variogram(myVar~1, loc=~x+y, data=myDat, cloud=TRUE)
pp <- plot(v.cl, id=T)
plot(pp, data=myDat)

datos <- v.cl[order(v.cl$dist),]
pp <- plot(datos[1:100,], id=T)
plot(pp, data=myDat)

datos <- v.cl[order(v.cl$dist),]
datos[1:10,]
id.oliers.fc(COSha102, "CorT")
#### Graficación de los modelos de semivarianza
p1 <- plot(ve, model=m.f.lin, main="Modelo Lineal", xlab="Distancia (m)", ylab="Semivarianza")
p2 <- plot(ve, model=m.f.gaus, main="Modelo Gaussiano", xlab="Distancia (m)", ylab="Semivarianza")
p3 <- plot(ve, model=m.f.exp, main="Modelo Exponencial", xlab="Distancia (m)", ylab="Semivarianza")
p4 <- plot(ve, model=m.f.esf, main="Modelo Esférico", xlab="Distancia (m)", ylab="Semivarianza")
print(p1, split = c(1, 1, 2, 2), more = T)
print(p2, split = c(2, 1, 2, 2), more = T)
print(p3, split = c(1, 2, 2, 2), more = T)
print(p4, split = c(2, 2, 2, 2), more = F)

v.cl <- variogram(myVar~1, loc=~x+y, data=myDat, cloud=TRUE)

png("fig53da10.png", width=550, height=270)
p1 <- plot(v.cl, xlab="Distancia (m)", ylab="Semivarianza", col="black", cex.lab=2, pch=".")
p2 <- plot(ve, model=m.f.exp, main=NULL, xlab="Distancia (m)", ylab="Semivarianza", col="black")
print(p1, split = c(1, 1, 2, 1), more = T)
print(p2, split = c(2, 1, 2, 1), more = T)
dev.off()

#Validacion cruzada
KO.lin.cv <- krige.cv(myVar~ 1, loc=~ x+y, data=myDat, model=m.f.lin, nfold=nrow(myDat))
KO.gaus.cv <- krige.cv(myVar~ 1, ~ x+y, myDat, m.f.gaus)
KO.exp.cv <- krige.cv(myVar~ 1, ~ x+y, myDat, m.f.exp)
KO.esf.cv <- krige.cv(myVar~ 1, ~ x+y, myDat, m.f.esf)

#source myFunctions_0.0-1.r before
resultados.cv <- rbind(criteria.cv(KO.lin.cv), criteria.cv(KO.gaus.cv), criteria.cv(KO.exp.cv), criteria.cv(KO.esf.cv))
rownames(resultados.cv) <- c("KO.lin.cv", "KO.gaus.cv", "KO.exp.cv", "KO.esf.cv")
resultados.cv

write.dbf(resultados.cv, file="cvCO304bxcx2.dbf")
array.out(list(m.f.lin, m.f.gaus, m.f.exp,  m.f.esf), TRUE)

muestreo <- myDat[,2:3]; coordinates(muestreo) <- ~x+y; l1 = list("sp.points", muestreo, col="black", pch=20); l2=list("sp.lines", cveg07Lines)

spplot(DA10.10000, "var1.pred", main=NULL, col.regions=bpy.colors(100), scales = list(draw =T), xlab ="Este (m)", ylab = "Norte (m)", sp.layout=list(l1, l2))


