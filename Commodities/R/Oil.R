da <- read.csv('http://www.quandl.com/api/v1/datasets/CFTC/CL_F_L_ALL.csv?&trim_start=2000-02-08&trim_end=2014-11-19&sort_order=desc', colClasses=c('Date'='Date'))
da$spec <- (da[,3] - da[,4])/(da[,3] + da[,4])
da$hedge <- da[,6] - da[,7]

plot(da$spec ~ da$Date, type = 'l', main = "Spculative oil positions", 
     xlab = "Date", ylab = "Spec Positions")
# Prevent the spec data being erased
par(new = T)
db <- read.csv('http://www.quandl.com/api/v1/datasets/OFDP/FUTURE_CL1.csv?&auth_token=mUCjthkJFQDsYVrFh4Gh&trim_start=2000-02-08&trim_end=2014-11-19&collapse=monthly&sort_order=desc', colClasses=c('Date'='Date'))
# axes = F prevents the axis being shown
plot(db$Settle ~ db$Date, axes = F, type = 'l', col = 'red', lty = 2, xlab = "", ylab = "")
axis(side = 4)
mtext("Spot oil", side = 4, line = 2, col = 4)
