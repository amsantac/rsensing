Kallocation <- function(data){

res1 <- table(data)

ref2tot <- colSums(res1)
sumtot <- sum(ref2tot)
ref2prop <- ref2tot/sumtot

min1prop <- apply(rbind(1/4, ref2prop), 2, min)

ref1tot <- rowSums(res1)
ref1prop <- ref1tot/sumtot

min2prop <- apply(rbind(ref1prop, ref2prop), 2, min)

prodprop <- ref1prop*ref2prop

AIpQIn <- sum(min1prop)
AIpQIm <- sum(min2prop)
AIpQIp <- 1

AImQIm <- sum(diag(res1))/sumtot

AInQIn <- 1/nrow(res1)
AInQIm <- sum(prodprop)
AInQIp <- ref2prop %*% ref2prop

Kallocation <- (AImQIm - AInQIm)/(AIpQIm - AInQIm)

return(Kallocation)
}