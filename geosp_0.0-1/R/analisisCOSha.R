library(foreign)
setwd("C:/amsantac/temp/amsantac/c/Geomatica/ProyectodeTesis/GA-K/Data")
load("dBase4bxcx.Rdata")
setwd("C:/amsantac/temp/amsantac/c/Geomatica/ProyectodeTesis/GA-K/Data/r241/data")
load("da104.Rdata")
load("co104.Rdata")
load("da304.Rdata")
load("co304.Rdata")

COSha10 <- daco104bxcx[,1:11]
COSha30 <- daco304bxcx[,1:11]

## Generacion de los datos de COS/ha
## COS/ha corregido: correccion 1 con la DA media de la cobertura
## correccion 2 con la DA de la observacion

dframe <- COSha10
groups <- "COB3" ## solo 4 grupos, Bn, Cp, Ct, P
attrs.da <- "DA10"
attrs.co <- "CO10"
depth <- 10

COShan <- paste("COSha", depth, sep='')
Corr1 <- paste(depth, "Cor1m", sep='')
Corr2 <- paste(depth, "Cor2i", sep='')
Corr3 <- paste(depth, "Cor3depthcorr", sep='')
Corr4 <- paste(depth, "Cor4DAidepthcorr", sep='')

dframe.Bn <- dframe[which(dframe[,groups]=="Bn"),]
meanDA.Bn <- mean(dframe.Bn[,attrs.da]) 
COSha <- data.frame(COSha=1:nrow(dframe))
colnames(COSha) <- COShan
Corr1m <- data.frame(Corr1=1:nrow(dframe))
colnames(Corr1m) <- Corr1
Corr2i <- data.frame(Corr2=1:nrow(dframe))
colnames(Corr2i) <- Corr2
Corr3.depthcorr <- data.frame(Corr3=1:nrow(dframe))
colnames(Corr3.depthcorr) <- Corr3
Corr4.DAi.depthcorr <- data.frame(Corr4=1:nrow(dframe))
colnames(Corr4.DAi.depthcorr) <- Corr4

for (i in 1:nrow(dframe)){
dframe2 <- dframe[which(dframe[,groups]==as.character(dframe[i,groups])),]
meanDAcob <- mean(dframe2[,attrs.da])
depth.corr <- (1-(meanDAcob-meanDA.Bn)/meanDA.Bn) * depth 
COSha[i,] <- dframe[i, attrs.co] * dframe[i, attrs.da] * depth
Corr1m[i,] <- dframe[i, attrs.co] * (1-(meanDAcob-meanDA.Bn)/meanDA.Bn) * depth
Corr2i[i,] <- dframe[i, attrs.co] * (1-(dframe[i, attrs.da]-meanDA.Bn)/meanDA.Bn) * depth
#
Corr3.depthcorr[i,] <- dframe[i, attrs.co] * meanDAcob * depth.corr
##
Corr4.DAi.depthcorr[i,] <- dframe[i, attrs.co] * dframe[i, attrs.da] * depth.corr
}
COSha101 <- cbind(dframe, COSha, Corr1m, Corr2i, Corr3.depthcorr, Corr4.DAi.depthcorr)
write.dbf(COSha10, "COSha10c")
save(COSha101, COSha301, file="COSha.Rdata")


## analisis estadistico de las variables Corr3.depthcorr, Corr4.DAi.depthcorr
#setwd("D:/Tesis/Data/")
setwd("C:/temp/Tesis/Data/")
load("COSha.Rdata")
## source myFunctions_0.0-1
mysumm.fc(COSha301, "COSha30", ofile="summCOSha301")                                                          
mysumm.fc(COSha301, "Cor3depthcorr", ofile="summCOSha301")
mysumm.fc(COSha301, "Cor4DAidepthcorr", ofile="summCOSha301")
myGraphs(COSha301, "COSha30", label="COSha")
myGraphs(COSha301, "Cor3depthcorr", label="COSha")
myGraphs(COSha301, "Cor4DAidepthcorr", label="COSha")
id.oliers.fc(COSha101, "COSha10")
id.oliers.fc(COSha301, "COSha30")
id.oliers.fc(COSha101, "Cor3depthcorr")
id.oliers.fc(COSha101, "Cor4DAidepthcorr")
id.oliers.fc(COSha301, "Cor4DAidepthcorr")

# analisis por cobertura
mysumm.fc(COSha301, "COSha30", "COB1r", ofile="summCOSha301Cob")
mysumm.fc(COSha301, "Cor3depthcorr", "COB1r", ofile="summCOSha301Cob")
mysumm.fc(COSha301, "Cor4DAidepthcorr", "COB1r", ofile="summCOSha301Cob")  
#myGraphs(COSha101, "Cor3depthcorr", label="COSha", groups="COB1r")
#myGraphs(COSha101, "Cor4DAidepthcorr", label="COSha", groups="COB1r")

# analisis por suelos
mysumm.fc(COSha301, "COSha30", "S_UDS_AG", ofile="summCOSha301UDS")
mysumm.fc(COSha301, "Cor3depthcorr", "S_UDS_AG", ofile="summCOSha301UDS")
mysumm.fc(COSha301, "Cor4DAidepthcorr", "S_UDS_AG", ofile="summCOSha301UDS")
#myGraphs(COSha101, "Cor3depthcorr", label="COSha", groups="S_UDS_AG")
#myGraphs(COSha101, "Cor4DAidepthcorr", label="COSha", groups="S_UDS_AG")

# pruebas de medias
# separacion de datos en dbf
sep.df.gr(COSha301, attrs="COSha30", groups="COB1r")
sep.df.gr(COSha301, attrs="Cor3depthcorr", groups="COB1r")
sep.df.gr(COSha301, attrs="Cor4DAidepthcorr", groups="COB1r")

sep.df.gr(COSha301, attrs="COSha30", groups="S_UDS_AG")
sep.df.gr(COSha301, attrs="Cor3depthcorr", groups="S_UDS_AG")
sep.df.gr(COSha301, attrs="Cor4DAidepthcorr", groups="S_UDS_AG")

