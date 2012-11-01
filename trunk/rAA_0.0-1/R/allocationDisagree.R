allocationDisagree <- function(data){

res1 <- table(data)

ref2tot <- colSums(res1)
sumtot <- sum(ref2tot)
ref2prop <- ref2tot/sumtot

min1prop <- apply(rbind(1/4, ref2prop), 2, min)

ref1tot <- rowSums(res1)
ref1prop <- ref1tot/sumtot

min2prop <- apply(rbind(ref1prop, ref2prop), 2, min)

prodprop <- ref1prop*ref2prop

persist <- diag(res1)/sumtot

loss <- ref1prop - persist

gain <- ref2prop - persist

gainIntens <- gain/ref2prop


AIpQIn <- sum(min1prop)
AIpQIm <- sum(min2prop)
AIpQIp <- 1

AImQIm <- sum(diag(res1))/sumtot

AInQIn <- 1/nrow(res1)
AInQIm <- sum(prodprop)
AInQIp <- ref2prop %*% ref2prop

Kallocation <- (AImQIm - AInQIm)/(AIpQIm - AInQIm)

AImQIn <- AInQIn + Kallocation * (AIpQIn - AInQIn)
AImQIp <- AInQIp + Kallocation * (AIpQIp - AInQIp)

Kno <- (AImQIm - AInQIn)/(AIpQIp - AInQIn)
Kquantity <- (AImQIm - AImQIn)/(AImQIp - AImQIn)
Khisto <- Khisto <- (AIpQIm - AInQIm)/(AIpQIp - AInQIm)
Kstandard <- (AImQIm - AInQIm)/(AIpQIp - AInQIm)

chanceAgreement <- 100 * min(AInQIn, AImQIm, AInQIm)
quantityAgreement <- ifelse(chanceAgreement==AInQIn, 100 * min(AInQIm - AInQIn, AImQIm - AInQIn), 0)
allocationAgreement <- 100 * max(AImQIm - AInQIm, 0)
allocationDisagreement <- 100 * (AIpQIm - AImQIm)
quantityDisagreement <- 100 * (AIpQIp - AIpQIm)
pierceSkillScoreBooleanCase <- 0
figureMeritBooleanCase <- 0

AIQImat <- matrix(c(AIpQIn, AImQIn, AInQIn, AIpQIm, AImQIm, AInQIm, AIpQIp, AImQIp, AInQIp), 3, 3)
rownames(AIQImat) <- c("AIperfect", "AImedium", "AIno")
colnames(AIQImat) <- c("QIno", "QImedium", "QIperfect")

res2 <- list(AIQImat = AIQImat, Kno = Kno, Kallocation = Kallocation, Kquantity = Kquantity[1], Khisto = Khisto, Kstandard = Kstandard, chanceAgreement = chanceAgreement, quantityAgreement = quantityAgreement, allocationAgreement = allocationAgreement, allocationDisagreement = allocationDisagreement, quantityDisagreement = quantityDisagreement, pierceSkillScoreBooleanCase = pierceSkillScoreBooleanCase, figureMeritBooleanCase = figureMeritBooleanCase)

return(res2)
}
