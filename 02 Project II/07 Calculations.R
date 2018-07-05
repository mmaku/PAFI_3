#śmieci potrzebne do działania
czas <- drzewo_okres(l_dni)
poziom <- drzewo_poziom(l_dni)

#ustawienia dywidend
d1 <- ymd("2010-05-18")
d2 <- ymd("2010-08-17")
d3 <- ymd("2010-11-16")
d1 <- round((yday(d1) - yday(begin) + (year(d1)-year(begin)) * 365) *252/365)
d2 <- round((yday(d2) - yday(begin) + (year(d2)-year(begin)) * 365) *252/365)
d3 <- round((yday(d3) - yday(begin) + (year(d3)-year(begin)) * 365) *252/365)
d_days <- c(d1,d2,d3)
d_height <- c(0.13,0.13,0.16)/ (s0_msft*exp(d_days/252*r))

#drzewka cen aktyw
goog_price <- drzewo_cen(m_goog, s_goog, r, l_dni, s0_goog)
msft_price <- drzewo_cen_dywidendy(drzewo_cen(m_msft, s_msft, r, l_dni, s0_msft), d_days, d_height)
numeraire <- drzewo_numeraire(r, l_dni, 1)

#ustawienie strike'ów dla obu opcji
goog_strike <- 450
msft_strike <- 27

#payoffy
payoff_goog_call <- payoff(goog_price, goog_strike, TRUE)
payoff_goog_put <- payoff(goog_price, goog_strike, FALSE)

payoff_msft_call <- payoff(msft_price, msft_strike, TRUE)
payoff_msft_put <- payoff(msft_price, msft_strike, FALSE)

# zdyskontowane payoffy
h_goog_call <- payoff_goog_call / numeraire
h_goog_put <- payoff_goog_put / numeraire

h_msft_call <- payoff_msft_call / numeraire
h_msft_put <- payoff_msft_put / numeraire

# otoczka snella
u_goog_call <- otoczka_snella(p_goog, h_goog_call, l_dni)
u_goog_put  <- otoczka_snella(p_goog, h_goog_put,  l_dni)

u_msft_call <- otoczka_snella(p_msft, h_msft_call, l_dni)
u_msft_put  <- otoczka_snella(p_msft, h_msft_put,  l_dni)

# nadwyżki
a_goog_call <- delta_a(p_goog, u_goog_call,l_dni)
a_goog_put <- delta_a(p_goog, u_goog_put,l_dni)

a_msft_call <- delta_a(p_msft, u_msft_call,l_dni)
a_msft_put <- delta_a(p_msft, u_msft_put,l_dni)

# hedging
msft_hedge_put  <- drzewo_hedging(msft_price, u_msft_put, numeraire, l_dni)
msft_hedge_call <- drzewo_hedging(msft_price, u_msft_call, numeraire, l_dni)


