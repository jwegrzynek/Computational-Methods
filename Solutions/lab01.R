#Zadanie 1

#wykresy funkcji gęstości dla rozkładów normalnych

x<-seq(-5,15,by=0.001)
plot(x,dnorm(x),col="red",type="l",lwd=1.5, main="Rozkłady normalne - funkcje gęstości",ylim=c(0,1))
lines(x,dnorm(x,mean=2,sd=3),col="blue",type="l",lwd=1.5)
lines(x,dnorm(x,mean=0,sd=2),col="green",type="l",lwd=1.5)
lines(x,dnorm(x,mean=5,sd=0.5),col="purple",type="l",lwd=1.5)
legend(x=10,y=0.8,legend=c("N(0,1)","N(2,3)","N(0,2)","N(5,0.5"),fill=c("red","blue","green","purple"))

#wykresy dystrybuant dla rozkładów normalnych

plot(x,pnorm(x),col="red",type="l",lwd=1.5, main="Rozkłady normalne - dystrybuanty",ylim=c(0,1))
lines(x,pnorm(x,mean=2,sd=3),col="blue",type="l",lwd=1.5)
lines(x,pnorm(x,mean=0,sd=2),col="green",type="l",lwd=1.5)
lines(x,pnorm(x,mean=5,sd=0.5),col="purple",type="l",lwd=1.5)
legend(x=10,y=0.8,legend=c("N(0,1)","N(2,3)","N(0,2)","N(5,0.5"),fill=c("red","blue","green","purple"))

#inna legenda

plot(x,pnorm(x),col="red",type="l",lwd=1.5, main="Rozkłady normalne - dystrybuanty",ylim=c(0,1),lty=1)
lines(x,pnorm(x,mean=2,sd=3),col="blue",type="l",lwd=1.5,lty=2)
lines(x,pnorm(x,mean=0,sd=2),col="green",type="l",lwd=1.5)
lines(x,pnorm(x,mean=5,sd=0.5),col="purple",type="l",lwd=1.5,lty=2)
legend(x=7,y=0.8,legend=c("N(0,1)","N(2,3)","N(0,2)","N(5,0.5"),lty=c(1,2,1,2),col=c("red","blue","green","purple"))


dev.off() #usuwamy wykresy


#Zadanie 2

#wspólny wykres dystrybuanty i funkcja gęstości dla rozkłądu wykładniczego
y<-seq(0,10,by=0.001)
plot(y,dexp(y,0.3),col="red",type="l",lwd=2,xlab="x",ylab="Gęstość rozkładu", main="Rozkład wykładniczy - funkcja gęstości i dystrybuanta",ylim=c(0,1))
lines(y,pexp(y,0.3),col="blue",type="l",lwd=3,lty=2)
axis(side=4)
mtext("Dystrybuanta",side=4)

dev.off()



#Zadanie 3

#losowanie  obserwacji z rozkładu wykładniczego
k=1000
z=rexp(k)

#porównanie kwantyli teoretycznych i empirycznych

qq=seq(0.05,0.95,by=0.05)
n=length(qq)
kwant_emp=1:n
kwant_teo=1:n
for (i in 1:n){
  kwant_emp[i]=quantile(z,qq[i])
  kwant_teo[i]=qexp(qq[i])
}
kwant_teo
kwant_emp

#wykres punktowy

plot(1:n,kwant_emp,main="Porównanie kwantyli",axes=F,xlab="",ylab="",type="p",pch=20,col="red")
lines(1:n,kwant_teo,type="p",pch=20,col="blue")
axis(side=1,at=1:n,qq)
axis(side=2)
legend("topleft",legend=c("Empiryczne","Teoretyczne"),fill=c("red","blue"))

#wykres linia

plot(1:n,kwant_emp,main="Porównanie kwantyli",axes=F,xlab="",ylab="",type="l",col="red")
lines(1:n,kwant_teo,type="l",col="blue")
axis(side=1,at=1:n,qq)
axis(side=2)
legend("topleft",legend=c("Empiryczne","Teoretyczne"),fill=c("red","blue"))

#wyknujemy histogram

hist(z)
hist(z,freq=F,ylim=c(0,1.1))
rug(z)
curve(dexp(x),col="darkblue",type="l",add=T)


#porównnujemy dystrybuantę (empiryczną i teoretyczną) z wykorzystaniem curve

plot(ecdf(z),col="red",main="Dystrybuanty porównanie",ylab="Dystrybuanta")
curve(pexp(x),from=min(z),to=max(z),add=T)


#porównujemy dystrybuantę (empiryczną i teoretyczną) z wykorzystaniem lines

plot(ecdf(z),col="red",main="Dystrybuanty porównanie",ylab="Dystrybuanta")
lines(sort(z),pexp(sort(z)),col="blue",add=T)


# ZADANIE 4.

#wykonujemy 1000 replikacji testu Kołmogorowa-Smirnowa dla 100 wylosowanych liczb zgodnie z pexp (rozkład wykładniczy)

k=1000
n=100
p.value_exp=replicate(k,ks.test(rexp(n),pexp)$p.value)

#wykonujemy histogram p-wartości testów

H1=hist(p.value_exp,prob=T,breaks=seq(0,1,by=0.05),col="red",border="black")
H1$breaks
H1$counts
#większość p-wartości jest powyżej progu 0.05 (wartości poniżej 0.05 stanowią nie więcej niż 5%), zatem nie ma podstaw by odrzucić hipotezę zerową testu, orzekajacą iż badany rozkład był rozkładem wykładniczym


#powtarzamy procedurę replikacji testu Kołmogorowa-Smirnowa dla pnorm (rozkłąd normalny)

k=1000
n=100
p.value_exp=replicate(k,ks.test(rexp(n),pnorm)$p.value)

#wykonujemy histogram p-wartości testów

H1=hist(p.value_exp,prob=T,breaks=seq(0,1,by=0.05),col="blue",border="black")
H1$breaks
H1$counts
#(prawie) wszystkie p-wartości są poniżej progu 0.05, zatem możemy odrzucić hipotezę zerową testu, orzekajacą iż badany rozkład był rozkładem normalnym

#Zadanie 5

#wczytujemy dane

daneSoc<-read.csv('http://www.biecek.pl/R/dane/daneSoc.csv',sep=';')
head(daneSoc)

dev.off()

#wydzielamy miejsece na cztery wykresy

par(mfrow=c(2,2))

#sporządzamy histogramy

hist(daneSoc$cisnienie.rozkurczowe,xlab='ciśnienie rozkurczowe',main='Histogram ciśnienia rozkurczowego',label=TRUE)
rug(daneSoc$cisnienie.rozkurczowe,ticksize=0.08,col='red')

hist(daneSoc$cisnienie.skurczowe,xlab='ciśnienie skurczowe',
     main='Histogram ciśnienia skurczowego',label=TRUE)
rug(daneSoc$cisnienie.skurczowe,ticksize=0.08,col='red')

#sporządzamy wykresy (aproksymowanych) funkcji gęstości rozkładów odpowiadającym histogramom

density(daneSoc$cisnienie.rozkurczowe)
plot(density(daneSoc$cisnienie.rozkurczowe),main='Gęstość ciśnienia rozkurczowego')

density(daneSoc$cisnienie.skurczowe)
plot(density(daneSoc$cisnienie.skurczowe), main='Gęstość ciśnienia skurczowego')

dev.off() #usuamy wtkresy, kończymy opcję podziału przestrzeni wykresu na cztery wykresy


#Zadanie 6


# a) ciśnienie skurczowe vs. ciśnienie rozkurczowe

# wykonujemy wykres rozrzutu z prostą regresji (korzystamy z pakietu ggplot2)

library(ggplot2)
ggplot(daneSoc, aes(cisnienie.skurczowe, cisnienie.rozkurczowe)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Ciśnienie skurczowe") + 
  ylab("Ciśnienie rozkurczowe") +
  theme_bw()

# wykonujemy test korelacji, gdzie hipoteza zerowa orzeka, iż współczynnnik korelacji jest równy zero (brak związku pomiędzy zmiennymi)

test<-cor.test(daneSoc$cisnienie.skurczowe,daneSoc$cisnienie.rozkurczowe)
test
test$statistic
test$estimate
test$p.value

#p-wartość poniżej, 0.05, zatem możemy odrzucić hipotezę zerową

#konstruujemy model liniowy (model regresji liniowej)
cisnienie_model <- lm(formula = cisnienie.skurczowe~cisnienie.rozkurczowe, data = daneSoc)
summary(cisnienie_model)


# b) cisnienie (skurczowe) vs. płeć

#sporządzamy wykres boxplot ciśnienia skurczowego dla kobiet i mężczyzn (pakiet ggplot2)

ggplot(daneSoc,mapping=aes(x=plec,y=cisnienie.skurczowe,fill=plec)) +
  geom_boxplot()+
  xlab("Płeć")+
  ylab("Ciśnienie skurczowe")+
  scale_fill_discrete(name="Płeć")+
  ggtitle("Ciśnienie skurczowe kobiet i mężczyzn")


#generujemy statystyki opisowe (dla obu grup ososbno)

df<-split(daneSoc$cisnienie.skurczowe,daneSoc$plec) #rozbicie zmiennej ciśnienie na dwie grupy

cis_k<-df[[1]]
cis_m<-df[[2]]

summary(daneSoc$cisnienie.skurczowe)
summary(cis_k)
summary(cis_m)

#wykonujemy test Wilcoxona z hipotezą zerową orzekajacą brak różnicy w medianach rozkładów dla obu grup

wilcox.test(cis_k,cis_m)

#p-wartość jest większa niż 0.05, więc nie odrzucamy hipotezy zerowej


#stosując test Shapiro-Wilka sprawdzamy, czy rozkład ciśnienia skurczowego (w obu grupach) jest normalny:


shapiro.test(daneSoc$cisnienie.skurczowe)

shapiro.test(cis_k)

shapiro.test(cis_m)

#według testu Shapiro-Wilka nie ma postaw do odrzucenia hipotezy zerowej (p-wartości większe niż 0.05), zatem przyjmujemy, że ciśnienie skurczowe ma rozkład normalny.

#wykonujemy analizę wariancji przy pomocy funkcji anova (na mocy przyjętego założenia o normalności rozkładów)

(m<-anova(lm(cisnienie.skurczowe~plec, data=daneSoc)))

#p-wartość jest większa niż 0.05, zatem nie odrzucamy hipotezy zerowej - nie ma statystycznie istotnych różnic w grupach (rezultat analogiczny do wyniku testu Wilcoxona)






#w analogiczny sposób (do przypadku b)) analizujemy przypdek ciśnienia rozkurczowego oraz przypadki c) i d)  



# e) płeć vs. wykształcenie


#tworzymy wykres mozaikowy


mosaicplot(~plec+wyksztalcenie, data=daneSoc,border='white',col=topo.colors(4))

#sporządzamy tabelę rozkład liczebności

tab<-table(daneSoc$plec,daneSoc$wyksztalcenie) 
tab 

round(prop.table(tab),2) # procentowy  rozkład liczebności (zaokrąglenie do drugiego miejsca)

# wykonujemy test na niezależność zmiennych chi-kwadrat, gdzie hipoteza zerowa orzeka, iż zmienne są niezależne

chisq.test(tab)

#ponieważ p-wartość wynosi więcej niż 0.05, więc nie odrzucamy hipotezy zerowej












