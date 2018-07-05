# OPCJA MSFT110121C0002700
# Equity: MSFT
# Maturity date: 2011-01-21
# Strike: 27.00
# Type: Call


# CALCULATIONS
nazwa <- "MSFT110121C0002700"
time       <- drzewo_okres(l_dni)
poziom     <- drzewo_poziom(l_dni)

d1 <- ymd("2010-05-18")
d2 <- ymd("2010-08-17")
d3 <- ymd("2010-11-16")
d1 <- round((yday(d1) - yday(begin) + (year(d1)-year(begin)) * 365) *252/365)
d2 <- round((yday(d2) - yday(begin) + (year(d2)-year(begin)) * 365) *252/365)
d3 <- round((yday(d3) - yday(begin) + (year(d3)-year(begin)) * 365) *252/365)
d_days <- c(d1,d2,d3)
d_height <- c(0.13,0.13,0.16)/ (s0_msft*exp(d_days/252*r))/5

msft_price_1 <- drzewo_cen_dywidendy(drzewo_cen(m_msft, s_msft, r, l_dni, s0_msft), d_days, d_height)
numeraire  <- drzewo_numeraire(r, l_dni, 1)

msft_strike_1 <- 27

payoff_msft_call_1 <- payoff(msft_price_1, msft_strike_1, T)
h_msft_call_1      <- payoff_msft_call_1 / numeraire
u_msft_call_1      <- otoczka_snella(p_msft, h_msft_call_1, l_dni)
a_msft_call_1      <- delta_a(p_msft, u_msft_call_1,l_dni)
msft_hedge_1      <- drzewo_hedging(msft_price_1, u_msft_call_1, numeraire, l_dni)


# VISUALIZATIONS

# snell_plot(u_msft_call_1, h_msft_call_1, msft_price, a_msft_call_1, time, nazwa, msft_strike_1, T)
# rgl.snapshot("u_msftC27_1.png")

#lower
d_height <- c(0.13,0.13,0.16)/ (s0_msft*exp(d_days/252*r))/10

msft_price_3 <- drzewo_cen_dywidendy(drzewo_cen(m_msft, s_msft, r, l_dni, s0_msft), d_days, d_height)
numeraire  <- drzewo_numeraire(r, l_dni, 1)

msft_strike_1 <- 27

payoff_msft_call_3 <- payoff(msft_price_3, msft_strike_1, T)
h_msft_call_3      <- payoff_msft_call_3 / numeraire
u_msft_call_3      <- otoczka_snella(p_msft, h_msft_call_3, l_dni)
a_msft_call_3      <- delta_a(p_msft, u_msft_call_3,l_dni)
msft_hedge_3      <- drzewo_hedging(msft_price_3, u_msft_call_3, numeraire, l_dni)


# snell_plot(u_msft_call_3, h_msft_call_3, msft_price, a_msft_call_3, time, nazwa, msft_strike_1, T)
# rgl.snapshot("u_msftC27_2.png")

