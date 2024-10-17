#generujmey dane do próby uczącej
library(MASS)
set.seed(123)
k1=30 #liczba wektorów w klasie 1
k0=20 #liczba wektorów w klasie 0
G1=mvrnorm(k1,mu=c(0,0),Sigma=matrix(c(1,0.1,0.,1),nrow=2))
G0=mvrnorm(k0,mu=c(2,2),Sigma=matrix(c(1,0.1,0.1,1),nrow=2))
G=rbind(G1,G0)
G
klasa=c(rep(1,k1),rep(0,k0))

#tworzymmy ramkę danych 
daneL=data.frame("x1"=G[,1],"x2"=G[,2],"klasa"=klasa)
daneL

#Wwzualizujemy dane z róby uczącej z podziałem na klasy

plot(daneL$x1,daneL$x2,type="n",xlab="x1",ylab="x2",main="Przygotowanie do analizy dyskryminacyjnej LDA")
points(daneL$x1[daneL$klasa==1],daneL$x2[daneL$klasa==1], col="red",cex=0.5)
points(daneL$x1[daneL$klasa==0],daneL$x2[daneL$klasa==0], col="blue",cex=0.5)
