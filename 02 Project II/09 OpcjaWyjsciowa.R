d1 <- ymd("2010-05-18")
d2 <- ymd("2010-08-17")
d3 <- ymd("2010-11-16")
d1 <- round((yday(d1) - yday(begin) + (year(d1)-year(begin)) * 365) * 252/365)
d2 <- round((yday(d2) - yday(begin) + (year(d2)-year(begin)) * 365) * 252/365)
d3 <- round((yday(d3) - yday(begin) + (year(d3)-year(begin)) * 365) * 252/365)

#policzenie po ilu okresach płaci się dywidendy


d_days <- c(d1,d2,d3)
d_height <- c(0.13,0.13,0.16)

drzewo_cen_komp <- function (drzewo_cen, d_days, d_height)
{
    drzewo_up   <- drzewo_cen
    drzewo_down <- drzewo_cen
  
     for (i in 1:length(d_days))
    {
        divup   <- d_height[i]/drzewo_cen[ind(d_days[i],0)]
        divdown <- d_height[i]/drzewo_cen[ind(d_days[i],d_days[i])]
        
        drzewo_up[ind(d_days[i],0):length(drzewo_cen)]   <- drzewo_up[ind(d_days[i],0):length(drzewo_cen)]-drzewo_up[ind(d_days[i],0):length(drzewo_cen)]*divup
        drzewo_down[ind(d_days[i],0):length(drzewo_cen)] <- drzewo_down[ind(d_days[i],0):length(drzewo_cen)]-drzewo_down[ind(d_days[i],0):length(drzewo_cen)]*divdown
    }
  
    return(cbind(drzewo_up,drzewo_down))
}

#wywoływanie
msft_price <- drzewo_cen(m_msft, s_msft, r, l_dni, s0_msft)
msft_price <- drzewo_cen_komp(msft_price, d_days, d_height)
msft_price_down <- msft_price[,2]
msft_price_up <- msft_price[,1]
msft_strike <- 30
#payoffy
payoff_msft_put_up <- payoff(msft_price_up, msft_strike, FALSE)
payoff_msft_put_down <- payoff(msft_price_down, msft_strike, FALSE)

#zdyskontowane payoffy

h_msft_put_down <- payoff_msft_put_down / numeraire
h_msft_put_up <- payoff_msft_put_up / numeraire

#otoczka snella

u_msft_put_down  <- otoczka_snella(p_msft, h_msft_put_down, l_dni)
u_msft_put_up  <- otoczka_snella(p_msft, h_msft_put_up, l_dni)


snell_plot_1 <- function (u_1, u_2, price_1, price_2, time, strike)
{
    plot3d (y = c(price_1, price_2), x = c(time,time), z = c(u_1, u_2),
            type = "p",
            col = c(rep("yellow", length(u_1)), rep("purple", length(u_2))), 
            ylab = "Cena ", xlab = "Czas", zlab = "Otoczka Snella",
            main= "Porownanie otoczki Snella liczonej dla maksymalnej i minimalnej dywidendy")
}


# snell_plot_1(u_msft_put_down, u_msft_put_up, msft_price_down, msft_price_up, czas, 25)
