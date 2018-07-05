path <- function(w, km, mi, sigma)
{
    v<-matrix(0,2,243)
    m<-rmvnorm(242,mi,sigma)
    
    rownames(v)=c("WIG20","KGHM")
    
    p1<-subset(w,Data=="2016-01-04")
    p2<-subset(km,Data=="2016-01-04")
    
    v[1,1]=p1[1,5]
    v[2,1]=p2[1,5]
    
    for(i in 2:243)
    {
        v[1,i]=exp(m[i-1,1])*v[1,i-1]
        v[2,i]=exp(m[i-1,2])*v[2,i-1]
    }
    
    return(v)
}

optionA <- function(sym, strike = 1800)
{
    return(max(-sym[1,243]+strike,0))
}

optionB <- function(sym)
{
  pom=0
  for(i in 1:243)
    if(sym[1,i]>2000)pom=1
  if(pom==1)return(0)
    else return(max(-sym[1,243]+1800,0))
}

optionC <- function(sym){
  pom=0
  for(i in 1:243)
    if(sym[1,i]>2000)pom=1
  if(pom==0)return(0)
    else return(max(-sym[1,243]+1800,0))
}

optionD <- function(sym){
  pom=0
  for(i in 1:243)
  {
    if(pom>=10)break
    if(sym[1,i]>2000)pom=pom+1
    else pom=0
  }
    if(pom<10)return(0)
    else return(max(-sym[1,243]+1800,0))
}

optionE <- function(sym){
  pom=0
  for(i in 1:243)
    pom=pom+sym[1,i]/243
  if(sym[2,243]<=sym[2,1])return(0)
  else return(max(pom-1800,0))
}