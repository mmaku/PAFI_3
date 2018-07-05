# funkcja do wizualizacji procesu ceny:
# u - wartości otoczki snella
# h - zdyskontowane roszczenia
# price - cena aktywa bazowego
# a- drzewo procesu a
# time - drzewo okresów (patrz funkcja drzewo_okres)
# aktywo - string z nazwą aktywa
# strike - cena wykonania
# call - TRUE / FALSE W zależności od typu opcji (call/ put)

price_plot <- function(price, time, level, aktywo, strike, call)
{
    plot3d (y = level, x = time, z = price,
            type = "s",radius=1,
            col = ifelse(price>strike, "green", "red"), 
            ylab = paste("Cena ", aktywo), xlab = "Czas", zlab = "Otoczka Snella",
            main= paste("Otoczka Snella opcji", ifelse(call, "call", "put"),"na aktywo", aktywo),
            sub = paste ("Cena wykonania: ", strike))
}


snell_plot <- function(u, h, price, a, time, aktywo, strike, call)
{
    aktywo0 <- substr(aktywo, 0, 4)
    plot3d (y = price, x = time, z = u,
            type = "s",radius=1,
            col  = ifelse(a!=0,"blue",ifelse(u==h, "green", "red")), 
            ylab = paste("Cena ", aktywo), xlab = "Czas", zlab = "Otoczka Snella",
            main = paste("Otoczka Snella opcji", ifelse(call, "call", "put"),"na aktywo", aktywo0),
            sub  = paste ("Cena wykonania:", strike))
}


doob_plot <- function(u, h, price, a, time, aktywo, strike, call)
{
    aktywo0 <- substr(aktywo, 0, 4)
    plot3d (y = price, x = time, z = a,
            type = "s",radius=1,
            col = ifelse(u==h, "green", "red"),
            ylab = paste("Cena ", aktywo), xlab = "Czas", zlab = "Otoczka Snella",
            main= paste("Delta A opcji", ifelse(call, "call", "put"),"na aktywo", aktywo0),
            sub = paste ("Cena wykonania:", strike))
}

doob_plot2d <- function(u, h, price, a, time, aktywo, strike, call)
{
    aktywo0 <- substr(aktywo, 0, 4)
    library("ggplot2")
    d <- data.frame (Czas = time, Cena = price, Strata=a)
    return (ggplot(data = d, mapping = aes(x = Czas, y = Cena)) +  geom_point(aes(colour = Strata)) +
                scale_colour_gradient(low = "blue",  high ="red") 
            + labs(title=aktywo)  + theme(text = element_text(size=20)
            ))
}



# price_plot(goog_price, czas, poziom, "GOOG", goog_strike, TRUE)
# price_plot(msft_price, czas, poziom, "GOOG", msft_strike, TRUE)
# 
# snell_plot(u_goog_put, h_goog_put, goog_price,a_goog_put, czas, "GOOG", goog_strike, FALSE)
# snell_plot(u_msft_call, h_msft_call, msft_price,a_msft_call, czas, "msft", msft_strike, TRUE)
# snell_plot(u_msft_put, h_msft_put, msft_price,a_msft_put, czas, "msft", msft_strike, FALSE)
