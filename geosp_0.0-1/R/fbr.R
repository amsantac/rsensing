fbr <-
function(sigma, var.reg, coordenadas, newdata, n.vec){
xy <- coordenadas
z <- var.reg
So <- newdata                                                         
sigma <- sigma                                                        
m.dist <- as.matrix(dist(rbind(xy,So)))                               
dist.So <- m.dist[nrow(m.dist),1:(ncol(m.dist)-1)]                    
vec.orden <- order(dist.So)                                           
dist.vec.cerca <- dist.So[vec.orden[1:n.vec]]                         
m.dist.vec <- as.matrix(dist(coordenadas))[vec.orden[1:n.vec], vec.orden[1:n.vec]]
phi <- sqrt(m.dist.vec^2+ sigma^2)
one = rep(1,nrow(phi))
PHI.Matriz <- rbind(cbind(phi, one),c(one,0))
PHI.Vector <- c(sqrt(dist.vec.cerca^2+ sigma^2),1)
W.fbr<-solve(PHI.Matriz, PHI.Vector)
FBR.pred <- W.fbr[-length(W.fbr)]%*%z[as.numeric(colnames(phi))]
FBR.pred
}

