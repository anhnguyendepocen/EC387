# Here are the data
# https://www.ecb.europa.eu/stats/money/long/html/index.en.html
cols <- c("Date1", "Belgium", "Germany", "Ireland", "Greece", "Spain", "France", "Italy", 
"Cyprus", "Latvia", "Luxembourg", "Malta", "Netherlands", "Austria", "Portugal",
"Slovenia", "Slovakia", "Finland", "Bulgaria", "Czech Republic", "Denmark", 
"Croatia", "Lithuania", "Hungary", "Poland", "Romania", "Sweden", "Sweden2", "UK") 
da <- read.csv("Bond/Data/irs.csv", skip = 7, col.names = cols, strip.white = TRUE, stringsAsFactors = FALSE)
da$Date <- as.Date(da$Date1, format = "%b-%Y")
# http://stackoverflow.com/questions/10446833/
# how-to-convert-a-character-string-date-to-date-class-if-day-value-is-missing
da$Date <- as.Date(as.yearmon(da$Date1, format = "%b-%Y"))

head(da)
tail(da)
str(da)
plot(da$Date, da$Germany, type = 'l', main = "German 10-year", xlab = "Date", 
     ylab = "Yield")
plot(da$Date, da$Greece-da$Germany, type = 'l', main = "Greek Risk Premium", 
     xlab = "Date",  ylab = "Yield")
plot(da$Date, da$Italy-da$Germany, type = 'l', main = "Italy Risk Premium", 
     xlab = "Date",  ylab = "Yield")
#--------------------------------------------------------------------
# http://research.stlouisfed.org/fred2/series/BAMLC0A4CBBB/downloaddata
da <- read.csv("Bond/Data/BAMLC0A4CBBB.csv", stringsAsFactors = FALSE)
da$Date <- as.Date(da$DATE, format = "%d/%m/%Y")
tail(da)
str(da)
plot(da$Date, da$VALUE, type = 'l', xlab = "Date", ylab = "Bp over treasuries", main = "BofA Merrill Lynch BBB 
     bond spread")
#---------------------
# http://isomorphism.es/post/101890975168/treasury-yield-curve-from-the-volcker-era-through
require(YieldCurve)
data(FedYieldCurve)
maturities <- c(3/12,6/12,1,2,3,5,7,10)
howmany=NROW(FedYieldCurve)
require(animation)
saveGIF({
  for (i in 1:howmany) {
    plot(maturities, FedYieldCurve[i,], type="o",lwd=3, col="#333333", xlab="Maturities structure in years", ylab="Interest rates values",ylim=c(0,15) )
    title(main=paste("Federal Reserve yield curve observed at", time(FedYieldCurve[i])))
    grid()
  }
},interval=.05,movie.name="yield curve evolution.gif", ani.width=300,ani.height=300)
#--------------ECB
data(ECBYieldCurve)
first(ECBYieldCurve,'3 day')
last(ECBYieldCurve,'3 day')

mat.ECB <- tau <- c(3/12,6/12,1:30)


par(mfrow=c(2,3))
for( i in c(1,2,3,653,654,655) ){
  plot(mat.ECB, ECBYieldCurve[i,], type="o", xlab="Maturities structure in years", ylab="Interest rates values")
  title(main=paste("European Central Bank yield curve obeserved at",time(ECBYieldCurve[i], sep=" ") ))
  grid()
}


