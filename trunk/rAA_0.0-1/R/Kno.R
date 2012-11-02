Kno <- function(data){

res1 <- table(data)
ref2tot <- colSums(res1)
sumtot <- sum(ref2tot)

AIpQIp <- 1
AImQIm <- sum(diag(res1))/sumtot
AInQIn <- 1/nrow(res1)

Kno <- (AImQIm - AInQIn)/(AIpQIp - AInQIn)

return(Kno)
}
