setwd(MyData)

goog_his    <- read.csv("goog_his.csv")
goog_curr   <- read.csv("goog_curr.csv")
msft_his    <- read.csv("msft_his.csv")
msft_curr   <- read.csv("msft_curr.csv")

setwd(MyPath)


#### ESTIMATION ##############################################

log_goog_his  <- log(head(goog_his$Close,-1)/tail(goog_his$Close,-1))
log_goog_curr <- log(head(goog_curr$Close,-1)/tail(goog_curr$Close,-1))
log_msft_his  <- log(head(msft_his$Close,-1)/tail(msft_his$Close,-1))
log_msft_curr <- log(head(msft_curr$Close,-1)/tail(msft_curr$Close,-1))
vol_goog_his  <- sd(log_goog_his)
vol_goog_curr <- sd(log_goog_curr)
vol_msft_his  <- sd(log_msft_his)
vol_msft_curr <- sd(log_msft_curr)
s0_goog       <- head(goog_his$Close,1)
s0_msft       <- head(msft_his$Close,1)


#### CRR CALIBRATION #######################################

s_goog <- (r - vol_goog_his^2/2)*dt #parametry nazywają się tak jak zakłada projekt
m_goog <- vol_goog_his*sqrt(dt)
p_goog <- (exp(r*dt) - exp(s_goog-m_goog))/(exp(s_goog+m_goog)-exp(s_goog-m_goog))
s_msft <- (log(25)-log(s0_msft))/l_dni 
m_msft <- vol_msft_his*sqrt(dt)
p_msft <- (exp(r*dt) - exp(s_msft-m_msft))/(exp(s_msft+m_msft)-exp(s_msft-m_msft))

