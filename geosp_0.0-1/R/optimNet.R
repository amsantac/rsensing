optimNet <- function(formula, locations, spDatF, fitmodel, n, popSize, generations, xmin, ymin, xmax, ymax){
	
	evaluate <- function(string=c()) {
		returnVal = NA;
		pts2 <- as.data.frame(matrix(0, ncol=2, nrow=n))
#las x
		for (i in 1:n){
			pts2[i,1] <- round(string[i], 1)
		}
#las y
		for (j in 1:n){
			pts2[j,2] <- round(string[n + j], 1)
		}
		
		names(pts2) <- c("x", "y")
		coordinates(pts2) = c("x", "y")
		
#plot(lalib2Lines, xlim=c(bbox(lalib2Lines)[1],bbox(lalib2Lines)[3]), ylim=c(bbox(lalib2Lines)[2],bbox(lalib2Lines)[4]))
#plot(pts.muestreo, pch=".", cex=2, add=T)
#plot(pts2, add=T)
		
#interpolar sobre los nuevos puntos
#kr <- krige(spVar~ 1, loc=~ x+y, data=spDatF, newdata=pts2, model=fitmodel) 
		interp <- krige(formula, locations, data=spDatF, newdata=pts2, model=fitmodel)
		
#if(file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
		
		returnVal <- sum(sqrt(interp[["var1.var"]]))/n
		returnVal
	}
	
	
	results <- rbga(as.matrix(c(rep(xmin,n), rep(ymin, n))), as.matrix(c(rep(xmax,n), rep(ymax, n))), popSize=popSize, evalFunc=evaluate, verbose=TRUE, iters=generations)
}

