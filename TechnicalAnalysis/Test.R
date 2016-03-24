require(tseries)
require(quantmod)
da <- read.csv("BehaviouralFinance/Data/GSPC.csv")
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
da <- da[rev(rownames(da)),]
da$NC <- (da$Close/Lag(da$Close, k = 1)) * 100 - 100
da$MA3 <- SMA(da$Close, n = 3) 
da$MA20 <- SMA(da$Close, n = 20)
da1 <- da[(da$Date  > as.Date("2014-12-25", format = "%Y-%m-%d") 
           & da$Date < as.Date("2015-02-15", format = "%Y-%m-%d")),]
# create net change oscilator

dat <- as.ts(da1[,c(2:5)])
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plotOHLC(dat, origin = "2014-12-25", ylab = "SP500", main = "Christmas 2014", xlab = "", xaxt = "n") 
plot(da1$Date, da1$NC, type = 'l', ylab = "Net Change Oscillator", xlab = "Date")
abline(h = 1, col = 'red', lty = 2)
abline(h = -1, col = 'red', lty = 2)
#------------------------------------
head(da, n = 11)
da$SI = NA
da$B <- da$MA3 > da$MA20 & Lag(da$MA3, 1) < Lag(da$MA20, 1) 
da$S <- da$MA3 < da$MA20 & Lag(da$MA3, 1) < Lag(da$MA20, 1) 
tail(da)
tail(da$MA3 > da$MA20 & Lag(da$MA3, 1) < Lag(da$MA20, 1))
if(da$da$B == TRUE){
  da$Pos <- 1
} else if (da$S == TRUE){
  da$Pos <- -1
} else 
  da$Pos <- 0
}  
da$B == TRUE
