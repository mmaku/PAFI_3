
m<-c(p[5,2],p[5,1])
sigm<-matrix(c(1,cor[3,1],cor[3,1],1),2,2)
sigm[1,1]<-sigm[1,1]*p[6,2]*p[6,2]
sigm[1,2]<-sigm[1,2]*p[6,2]*p[6,1]
sigm[2,1]<-sigm[2,1]*p[6,2]*p[6,1]
sigm[2,2]<-sigm[2,2]*p[6,1]*p[6,1]

symuluj=function(w,km,l,mi,sigma){
  meanA<-0
  meanB<-0
  meanC<-0
  meanD<-0
  meanE<-0
  for(i in 1:l)
  {
    pom<-sciezka(w,km,mi,sigma)
    meanA<-meanA+opcjaA(pom)/l
    meanB<-meanB+opcjaB(pom)/l
    meanC<-meanC+opcjaC(pom)/l
    meanD<-meanD+opcjaD(pom)/l
    meanE<-meanE+opcjaE(pom)/l
    pom[1,1]<-1000
    pom[1,2]<-3000
    if(i==1)plot(pom[1,])
    else lines(pom[1,])
  }
  return(c(meanA,meanB,meanC,meanD,meanE))
}
symuluj(wig,kghm,100,m,sigm)
