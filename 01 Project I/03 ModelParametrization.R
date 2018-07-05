setwd("C:/Users/mchl/Dropbox/01 Studia/01 UWr/01 Szkoła - projekty/12 WAiF III/01 Projekt I")

kghm <- read.table("kghm.csv", header=TRUE, sep=",", dec=".") 
wig <- read.table("wig20.csv", header=TRUE, sep=",", dec=".") 
p<-matrix(2,6,2)
colnames(p)=c('KGHM','WIG20')
rownames(p)=c('Średnia roczna','Odchylenie roczne','Średnia całego okresu','Odchylenie całego okresu','Średnia ważona wykładniczo','Odchylenie ważone wykładniczo')
cor<-matrix(2,3,1)
colnames(cor)=c('Korelacja')
rownames(cor)=c('Roczna','Cały okres','Cały okres ważona')
#Jeden rok
kghm1<-kghm[-c(1:4433),]
rKGHM1<-log(1+(kghm1[,'Zamkniecie']-kghm1[,'Otwarcie'])/kghm1[,'Otwarcie'])
p[1,1]<-mean(rKGHM1)
p[2,1]<-sd(rKGHM1)
wig1<-wig[-c(1:5538),]
rWIG1<-log(1+(wig1[,'Zamkniecie']-wig1[,'Otwarcie'])/wig1[,'Otwarcie'])
p[1,2]<-mean(rWIG1)
p[2,2]<-sd(rWIG1)
cor[1,1]<-cor(rKGHM1,rWIG1)
#Pełny okres
srednia=function(tab)
{
  sum=0
  count=0
  for(i in 0:(length(tab)/252-1))
  {
    for(j in 1:252)
    {
      sum=sum+tab[252*i+j]/252*exp(-(length(tab)/252-i))
    }
    count=count+exp(-(length(tab)/252-i))
  }
  return(sum/count)
}
odchylenie=function(tab)
{
  sum=0
  count=0
  m<-srednia(tab)
  for(i in 0:(length(tab)/252-1))
  {
    for(j in 1:252)
    {
      sum=sum+(tab[252*i+j]-m)*(tab[252*i+j]-m)/252*exp(-(length(tab)/252-i))
    }
    count=count+exp(-(length(tab)/252-i))
  }
  return(sqrt(sum/count*length(tab)/(length(tab)-1)))
}
korelacja=function(tab1,tab2)
{
  sum=0
  count=0
  m1<-srednia(tab1)
  m2<-srednia(tab2)
  for(i in 0:(length(tab1)/252-1))
  {
    for(j in 1:252)
    {
      sum=sum+tab1[252*i+j]*tab2[252*i+j]/252*exp(-(length(tab1)/252-i))
    }
    count=count+exp(-(length(tab1)/252-i))
  }
  sum=sum/count
  sum=sum-m1*m2
  sum=sum/odchylenie(tab1)
  sum=sum/odchylenie(tab2)
  return(sum*length(tab1)/(length(tab1)-1))
}
kghm2<-kghm[-c(1:149),]
rKGHM2<-log(1+(kghm2[,'Zamkniecie']-kghm2[,'Otwarcie'])/kghm2[,'Otwarcie'])
p[3,1]<-mean(rKGHM2)
p[4,1]<-sd(rKGHM2)
p[5,1]<-srednia(rKGHM2)
p[6,1]<-odchylenie(rKGHM2)
wig2<-wig[-c(1:246),]
rWIG2<-log(1+(wig2[,'Zamkniecie']-wig2[,'Otwarcie'])/wig2[,'Otwarcie'])
p[3,2]<-mean(rWIG2)
p[4,2]<-sd(rWIG2)
p[5,2]<-srednia(rWIG2)
p[6,2]<-odchylenie(rWIG2)
rWIG3<-rWIG2[-c(1:1008)]
cor[2,1]<-cor(rKGHM2,rWIG3)
cor[3,1]<-korelacja(rKGHM2,rWIG3)

#Zabawa kopułami
library("mvtnorm")
library("CDVine")
#Jeden rok
F11<-ecdf(rWIG1)
F21<-ecdf(rKGHM1)
u1<-F11(rWIG1)
v1<-F21(rKGHM1)
kop1<-BiCopSelect(u1,v1,familyset=c(1:6))
sim1<-BiCopSim(10000,family=kop1$family,par=kop1$par,par2=kop1$par2)
plot(sim1)
points(u1,v1,col='red')
#Pełny okres
F12<-ecdf(rWIG3)
F22<-ecdf(rKGHM2)
u2<-F12(rWIG3)
v2<-F22(rKGHM2)
kop2<-BiCopSelect(u2,v2,familyset=c(1:6))
sim2<-BiCopSim(10000,family=kop2$family,par=kop2$par,par2=kop2$par2)
plot(sim2)
points(u2,v2,col='red')

#ecdf z wagami, coded by Jorge Luis Ojeda Cabrera
#Prof. Asoc. del Dep. Métodos Estadísticos.
#Univ. de Zaragoza.
newEcdf <- function (x,weig=rep(1,length(x)))
{
  n <- length(x)
  if (n < 1)
    stop("'x' have 1 or more non-missing values")
  if (n != length(weig))
    stop("'weig' must have same length than 'x'")
  xPerm <- order(x)
  weig <- weig/sum(weig)
  x <- x[xPerm]
  weig <- weig[xPerm]
  ndIdx <- !duplicated(x)
  vals <- x[ndIdx]
  weig <- sapply(x[ndIdx],function(xx) sum(weig[x==xx]))
  rval <- approxfun(vals, cumsum(weig),
                    method = "constant", yleft = 0, yright = 1, f = 0, ties = 
                      "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  attr(rval, "call") <- sys.call()
  rval
}
wagi=function(size)
{
  w<-c(1:size)
  for(i in 0:(size/252-1))
  {
    for(j in 1:252)
    {
      w[252*i+j]<-exp(-(size/252-i))
    }
  }
  return(w)
}
F13<-newEcdf(rWIG3,wagi(length(rWIG3)))
F23<-newEcdf(rKGHM2,wagi(length(rKGHM2)))
u3<-F13(rWIG3)
v3<-F23(rKGHM2)
kop3<-BiCopSelect(u3,v3,familyset=c(1:6))
sim3<-BiCopSim(10000,family=kop3$family,par=kop3$par,par2=kop3$par2)
plot(sim3)
points(u3,v3,col='red')