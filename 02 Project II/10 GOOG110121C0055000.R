# OPCJA GOOG110121C0055000
# Equity: GOOG
# Maturity date: 2011-01-21
# Strike: 550.00
# Type: Call


# CALCULATIONS

time       <- drzewo_okres(l_dni)
poziom     <- drzewo_poziom(l_dni)

goog_price <- drzewo_cen(m_goog, s_goog, r, l_dni, s0_goog)
numeraire  <- drzewo_numeraire(r, l_dni, 1)

goog_strike_1 <- 500

payoff_goog_call_1 <- payoff(goog_price, goog_strike_1, T)
h_goog_call_1      <- payoff_goog_call_1 / numeraire
u_goog_call_1      <- otoczka_snella(p_goog, h_goog_call_1, l_dni)
a_goog_call_1      <- delta_a(p_goog, u_goog_call_1,l_dni)
goog_hedge_1       <- drzewo_hedging(goog_price, u_goog_call_1, numeraire, l_dni)