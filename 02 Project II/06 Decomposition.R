otoczka_snella <- function(p, h, l_dni)
{
    drzewo_u                                <- rep(0, nods(l_dni))
    drzewo_u[ind(l_dni,0):ind(l_dni,l_dni)] <- h[ind(l_dni,0):ind(l_dni,l_dni)]
    
    for (i in (l_dni-1):0)
    {
        for (j in 0:i)
        {
            drzewo_u[ind(i,j)] <- max(p*drzewo_u[ind(i+1,j)] + (1-p)*drzewo_u[ind(i+1,j+1)], h[ind(i,j)])
        }
    }
    return(drzewo_u)
}

#zwraca drzewo A_t
delta_a <- function(p, u, l_dni)
{
    drzewo_delta_a <- rep(0, nods(l_dni))
    
    for (i in 0:(l_dni-1))
    {
        for (j in 0:i)
        {
            drzewo_delta_a[ind(i,j)] <- u[ind(i,j)] - (p*u[ind(i+1,j)] + (1-p)*u[ind(i+1,j+1)])
        }
        
    }
    return(drzewo_delta_a)
}
