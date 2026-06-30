library(FactoMineR)
data(decathlon)
head(decathlon)

X=as.matrix(decathlon[,1:10])
g=colMeans(X)
g
Y=sweep(x=X,2,g,FUN = '-')
round(colMeans(X),3)
round(colMeans(Y))
n=nrow(X)
p=ncol(X)
et=apply(Y, 2, function(x) sqrt(sum(x^2)/n))
et
Z=sweep(x=Y, 2, et, FUN = '/')
colMeans(Z^2)/n


M=diag(rep(1,p))
D=(1/n)*diag(rep(1,n))
R=t(Z)%*%D%*%Z
vp=eigen(R%*%M)
lambda=vp$values
lambda
U=vp$vectors
U
Psi=Z%*%U
etat <- sweep(U,2,sqrt(lambda),FUN='*')
etat
res.pca=PCA(decathlon,ncp=5,quanti.sup=11:12,quali.sup = 13,graph=F)
head(res.pca$eig)