drzewo_hedging <- function(price, u, numeraire, l_dni)
{
    x   <- price/numeraire
    S_0 <- numeraire[1]
    drzewo_ilosc_akcji    <- rep(0, nods(l_dni))
    drzewo_ilosc_gotowki  <- rep(0, nods(l_dni))
    
    for (i in 0:(l_dni-1))
    {
        for (j in 0:i)
        {
            drzewo_ilosc_gotowki[ind(i,j)] <- (u[ind(1+i,j+1)]-u[ind(1+i,j)]*x[ind(i+1,j+1)]/x[ind(i+1,j)])/(S_0 - S_0*x[ind(i+1,j+1)]/x[ind(i+1,j)])
            drzewo_ilosc_akcji[ind(i,j)]   <- (u[ind(i+1,j)] - drzewo_ilosc_gotowki[ind(i,j)] * S_0) / x[ind(i+1,j)] 
            
#             drzewo_ilosc_akcji[ind(i,j)]   <- (u[ind(i+1,j+1)] - u[ind(i+1,j)])/(x[ind(i+1,j+1)] - x[ind(i+1,j)])
#             drzewo_ilosc_gotowki[ind(i,j)] <- u[ind(i+1,j)] - drzewo_ilosc_akcji[ind(i,j)]*x[ind(i+1,j)]
        }
        
    }
    
    return(cbind(drzewo_ilosc_akcji,drzewo_ilosc_gotowki))
}

# hedging_plot_alpha <- function (drzewo_ilosc_akcji, drzewo_ilosc_gotowki, price, czas, a, l_dni)
# {
#     drzewo_ilosc_akcji   <- drzewo_ilosc_akcji[1:(ind(l_dni-1, 1)-1)]
#     drzewo_ilosc_gotowki <- drzewo_ilosc_gotowki[1:(ind(l_dni-1, 1)-1)]
#     price                <- price[1:(ind(l_dni-1, 1)-1)]
#     czas                 <- czas[1:(ind(l_dni-1, 1)-1)]
# 
#         plot3d (z = c(drzewo_ilosc_akcji,drzewo_ilosc_gotowki/price), x = c(czas,czas), y = c(price, price),
#             type = "s", radius = 1,
#             col = c(ifelse(a==0, "blue", "green") , ifelse(a==0, "purple", "gray")), 
#             ylab = "Cena ", xlab = "Czas", zlab = "Ilosc poszczegolnych aktywow",
#             main= "Wizualizacja hedgingu")
#     
# }
# 
# hedging_plot_bis <- function (drzewo_ilosc, price, czas, a, l_dni)
# {
#     drzewo_ilosc <- drzewo_ilosc[1:(ind(l_dni-1, 1)-1)]
#     price        <- price[1:(ind(l_dni-1, 1)-1)]
#     czas         <- czas[1:(ind(l_dni-1, 1)-1)]
#     
#     plot3d (x = czas, y = price, z = drzewo_ilosc,
#             type = "s", radius = 1,
#             col = ifelse(a==0, "blue", "green"), 
#             ylab = "Cena ", xlab = "Czas", zlab = "Ilosc poszczegolnych aktywow",
#             main= "Wizualizacja hedgingu")
# }
# 
# hedging_plot_cis <- function(drzewo_ilosc, price, time, tytul)
# {
#     d <- data.frame (Czas = time, Cena = price, ilosc= drzewo_ilosc)
#     return (ggplot(data = d, mapping = aes(x = Czas, y = Cena)) +  geom_point(aes(colour = drzewo_ilosc)) +
#                 scale_colour_gradient(low = "blue",  high ="red") 
#             + labs(title=tytul)  + theme(text = element_text(size=20)
#             ))
# }

hedging_plot <- function (drzewo_ilosc_akcji, drzewo_ilosc_gotowki, price, czas, a, l_dni)
{
    drzewo_ilosc_akcji   <- drzewo_ilosc_akcji[1:(ind(l_dni-1, 1)-1)]
    drzewo_ilosc_gotowki <- drzewo_ilosc_gotowki[1:(ind(l_dni-1, 1)-1)]
    price                <- price[1:(ind(l_dni-1, 1)-1)]
    czas                 <- czas[1:(ind(l_dni-1, 1)-1)]
    
    plot3d (z = c(drzewo_ilosc_akcji,drzewo_ilosc_gotowki/price), x = c(czas,czas), y = c(price, price),
            type = "p", size = 3,
            col = c(ifelse(drzewo_ilosc_akcji == 0, "white", "red"),ifelse(drzewo_ilosc_gotowki == 0, "white", "blue") ), 
            alpha = c(ifelse(drzewo_ilosc_akcji == 0, 0, 1),ifelse(drzewo_ilosc_gotowki == 0, 0, 1)), 
            ylab = "Cena ", xlab = "Czas", zlab = "Ilosc poszczegolnych aktywow",
            main= "Wizualizacja hedgingu")
    
}

