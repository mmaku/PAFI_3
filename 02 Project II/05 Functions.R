#### FUNKCJE DO OBSŁUGI DRZEW ###############################################

ind <- function (okres, wys) 
{
    (okres+1)*okres/2+wys+1 # istotne: 0<=wys<=okres
}

nods <- function(l_dni) 
{
    (l_dni+1)*(l_dni+2)/2 # zwraca liczbę węzłów drzewa l_dni-okresowego
}

drzewo_cen <- function(m,s,r,l_dni,s0) 
{
    drzewo           <- rep (0, nods(l_dni))
    drzewo[ind(0,0)] <- s0
    
    for(i in 1: l_dni)
    {
        for(j in 0:(i-1))
        {
            drzewo[ind(i,j)] <- drzewo[ind(i-1, j)] * exp(s+m)
        }
        drzewo[ind(i,i)] <- drzewo[ind(i-1, i-1)] * exp(s-m)
    }
    
    return(drzewo)
}

drzewo_cen_dywidendy <- function(drzewo_cen, d_days, d_height)
{
    for(i in 1:length(d_days))
    {
        drzewo_cen[ind(d_days[i],0):length(drzewo_cen)] <- drzewo_cen[ind(d_days[i],0):length(drzewo_cen)] - drzewo_cen[ind(d_days[i],0):length(drzewo_cen)]*d_height[i]
    }
    
    return(drzewo_cen)
}

drzewo_okres <- function(l_dni)
{
    drzewo <- rep(0, nods(l_dni))
    
    for(i in 0 : l_dni)
    {
        for(j in 0 : i)
        {
            drzewo[ind(i,j)] <- i
        }
    }
    
    return (drzewo)
}

drzewo_poziom <- function(l_dni)
{
    drzewo <- rep(0, nods(l_dni))
    
    for(i in 0 : l_dni)
    {
        for(j in 0 : i)
        {
            drzewo[ind(i,j)] <- j
        }
    }
    
    return (drzewo)
}

drzewo_numeraire <- function(r, l_dni, s0) 
{
    drzewo           <- rep(s0, nods(l_dni))

    for(i in 1: l_dni)
    {
        for(j in 0:i)
        {
            drzewo[ind(i,j)] <- s0*exp(i/252*r)
        }
    }
    return (drzewo)
}

payoff <- function(stock, strike, call = TRUE, upbar = Inf, downbar = 0) #payoff opcji barierowej wypłacającej iff cena aktywa utrzymuje się w przedziale [downbar,upbar]
{
    if(call)
    {
        return(abs(stock>strike) * (stock-strike) * abs(stock<upbar) * abs(stock>downbar))
    }
    else
    {
        return(abs(stock<strike) * (-stock+strike) * abs(stock<upbar) * abs(stock>downbar))
    }
}