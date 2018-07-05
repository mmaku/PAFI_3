rm(list = ls()) 

Timer <- proc.time()

# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("lubridate")
# install.packages("rgl")

# library(ggplot2)
# library(plotly)
library(lubridate)
library(rgl)


#### WORKING DIRECTORY ######################################

MyPath         <- "C:/Users/mchl/Dropbox/01 Studia/01 UWr/01 Szkoła - projekty/12 WAiF III/02 Projekt II/" 
MyData         <- paste(MyPath,"01 Data/", sep="") 
MyDeliverables <- paste(MyPath,"02 Deliverables/", sep="") 
setwd(MyPath)


#### PARAMETERS ##############################################

# r     <- 0.009044
r     <- 0.004
dt    <- 1/252
expiry<- dmy("21-01-2011")
begin <- dmy("14-05-2010")
l_dni <- round((yday(expiry) - yday(begin) + (year(expiry)-year(begin)) * 365) *252/365)


#### SOURCE FILES ############################################

# source("04 Calibration CRR.R")
source("04 Calibration JR-RN.R")
source("05 Functions.R")
source("06 Decomposition.R")
source("06+ Hedging.R")
source("07 Calculations.R")
source("08 Visualisations.R")

#### OPTIONS MAKU ############################################

source("10 GOOG110121C0055000.R")
source("11 GOOG110121P0050000.R")
# VISUALIZATIONS MAKU ########################################

snell_plot(u_goog_call_1, h_goog_call_1, goog_price, a_goog_call_1, time, "GOOG110121C0050000", goog_strike_1, T)
hedging_plot(goog_hedge_1[,1], goog_hedge_1[,2], goog_price, time, a_goog_call_1, l_dni)


snell_plot(u_goog_put_1, h_goog_put_1, goog_price, a_goog_put_1, time, "GOOG110121P0050000", goog_strike_1, F)
hedging_plot(goog_hedge_1[,1], goog_hedge_1[,2], goog_price, time, a_goog_put_1, l_dni)


# VISUALIZATIONS KUBA ########################################

source("09 OpcjaWyjsciowa.R")
snell_plot_1(u_msft_put_down, u_msft_put_up, msft_price_down, msft_price_up, czas, 25)


source("12 MSFT110121C0002700.R")
snell_plot(u_msft_call_1, h_msft_call_1, msft_price, a_msft_call_1, time, nazwa, msft_strike_1, T)
hedging_plot(msft_hedge_1[,1], msft_hedge_1[,2], msft_price, time, a_msft_call_1, l_dni)

source("12 MSFT110121C0002700 - new divs.R")
snell_plot(u_msft_call_1, h_msft_call_1, msft_price_1, a_msft_call_1, time, nazwa, msft_strike_1, T)
snell_plot(u_msft_call_3, h_msft_call_3, msft_price_3, a_msft_call_3, time, nazwa, msft_strike_1, T)

source("13 MSFT110121P0003000.R")
snell_plot(u_msft_put_1, h_msft_put_1, msft_price, a_msft_put_1, time, "MSFT110121P0003000", msft_strike_1, F)
hedging_plot(msft_hedge_1[,1], msft_hedge_1[,2], msft_price, time, a_msft_put_1, l_dni)

source("13 MSFT110121P0003000 - new divs.R")
snell_plot(u_msft_put_1, h_msft_put_1, msft_price, a_msft_put_1, time, "MSFT110121P0003000", msft_strike_1, F)


