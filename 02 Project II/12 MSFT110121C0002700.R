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
d_height <- c(0.13,0.13,0.16)/ (s0_msft*exp(d_days/252*r))

msft_price <- drzewo_cen_dywidendy(drzewo_cen(m_msft, s_msft, r, l_dni, s0_msft), d_days, d_height)
numeraire  <- drzewo_numeraire(r, l_dni, 1)

msft_strike_1 <- 27

payoff_msft_call_1 <- payoff(msft_price, msft_strike_1, T)
h_msft_call_1      <- payoff_msft_call_1 / numeraire
u_msft_call_1      <- otoczka_snella(p_msft, h_msft_call_1, l_dni)
a_msft_call_1      <- delta_a(p_msft, u_msft_call_1,l_dni)
msft_hedge_1      <- drzewo_hedging(msft_price, u_msft_call, numeraire, l_dni)


# VISUALIZATIONS

# snell_plot(u_msft_call_1, h_msft_call_1, msft_price, a_msft_call_1, time, nazwa, msft_strike_1, T)
# rgl.snapshot("u_msftC27.png")
# doob_plot(u_msft_call_1, h_msft_call_1, msft_price, a_msft_call_1, time, nazwa, msft_strike_1, T)
# doob_plot2d(u_msft_call_1, h_msft_call_1, msft_price, a_msft_call_1, time, nazwa, msft_strike_1, T)
# hedging_plot(msft_hedge_1[,1], msft_hedge_1[,2], msft_price, time, a_msft_call_1, l_dni)
# rgl.snapshot("hedge_msftC27.png")
