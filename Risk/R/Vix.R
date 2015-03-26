# looking at relationship between SPX HV, RV & VIX
# Peter Werner, 2013
##
library(quantmod)
library(ggplot2)
library(reshape2)
library(gridExtra)
#create functions------------------------------------------
rsq <- function(y,f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }

build.rets <- function(x, k=1)
{
  r <- ROC(x,k)
  return(as.vector(r))
}

#realized vol
#passed a series of log rets
rv <- function(x, dpy=252)
{
  n <- length(x)
  #assume mean ret is 0
  meanret <- 0
  r <- r * dpy
  r <- sum((x - meanret)^2, na.rm=TRUE)/(n - 1)
  return(100 * sqrt(r))
}
# calculate the proportional distribution
prop.dist <- function(x)
{
  tgt <- x[length(x)]
  minval <- min(x)
  maxval <- max(x)
  rg <- max(x) - min(x)
  d <- (tgt - min(x)) / rg
  return(d)
}

maptobin <- function(x, minval=0L, maxval=1L, nbins=5)
{
  if (is.na(x))
    return(x)
  if (x < minval || x > maxval) {
    stop(cat(sprintf("x outside range %.2f min %.2f max %.2f\n", x, minval, maxval)))
  }
  #capture the upside boundary condition
  if (x == maxval) 
    return(nbins)
  #work out the size of each bin
  binsize <- (maxval - minval)/nbins
  #see where the value lies
  binidx <- floor(x / binsize)
  #add plus 1 so they are indexed from 1-nbins
  return(binidx + 1)
}
# Get the VIX and GSCP data
getSymbols(c("^GSPC", "^VIX"), from="2000-01-01")
spx <- GSPC
vix <- VIX
#spx <- GSPC["2011/2012"]
#vix <- VIX["2011/2012"]
r <- build.rets(Cl(spx))
#Me----------------------------------------------------
# This is added by me.  It is to plot the index.  I want S&P as well.
head(vix)
par(mfrow = c(1,2))
plot(vix[,4], main = "VIX Index")
plot(spx[,4], main = "S&P 500")
eq1 <- lm(spx[,4] ~ vix[,4])
# rv and hv calculations----------------------------------
#the # of periods (not cal days) to calc vol
n <- 21
#we use na.omit as the first item of r will be NA
#so for forward looking realized vol it will be r[2:11] with n=10
#but cause r[1] is na, it looks like r[1:10].
#for hist vol, we use the returns up to and including the current day.
#this is because VIX[i] is the vix close for that day, not the 
#previous day. We want to look at what hv was on that day, so we 
#should include the current day as the VIX would have factored that in
#n.b. we could add a 1 day lag (by using n+1 in the rep for histvol), 
#and see if there is any material difference
voltmp <- rollapply(na.omit(r), n, rv)
realvol <- c(voltmp, rep(NA, n))
histvol <- c(rep(NA, n), voltmp)
iv <- Cl(vix)
spxvals <- cbind(Cl(spx), histvol, realvol, iv)
names(spxvals) <- c("C", "hv", "rv", "iv")
spxvals <- na.omit(spxvals)
nrow(spxvals)

cor(spxvals$iv, spxvals$hv)
cor(spxvals$iv, spxvals$rv)
cor(spxvals$hv, spxvals$rv)
rsq(spxvals$iv, spxvals$hv)
rsq(spxvals$iv, spxvals$rv)
rsq(spxvals$hv, spxvals$rv)
tab1<- cor(spxvals[,2:4])
require("xtable")
# Create the table for tex
xtable(tab1)
###
#look at distributions of next day returns for some relative measure of iv
###
r <- build.rets(Cl(spx))
iv <- Cl(vix)
n <- 252 #lookback period
iv.prop <- rollapply(iv, n, prop.dist, align='right', fill=NA)
tail(iv.prop)
ivrel <- cbind(Next(r), iv, as.vector(iv.prop))
names(ivrel) <- c('nd', 'iv', 'ivp')
head(ivrel)
nrow(ivrel)
nbins <- 5
ivrel$bin <- sapply(as.vector(ivrel$ivp), maptobin)

binnedrets <- list()
for (i in 1:nbins) {
  #get the next day returns for the given bin
  rets <- ivrel[ivrel$bin == i]$nd
  binnedrets[[i]] <- as.vector(rets)
}
#with lookback of 252 it shows vol increasing
#we can see the mean next day return is still 0, regardless of where vol was
#iv is not necessarily a good predictor of next day returns
#however is a good indicator of higher volatility
par(mfrow = c(1,1))
boxplot(binnedrets, main = 'VIX Next Day Return Distributions', 
        xlab = 'Quintiles', ylab = 'Return')
abline(h = 0)
tmp <- melt(binnedrets)
title <- 'VIX Next Day Return Distributions'
qplot(L1, y=value, data=tmp, geom="boxplot", group=L1)
p <- ggplot(tmp, aes(x=L1, y=value, group=L1)) + geom_boxplot()
p <- p + labs(list(x="Relative VIX Quintile", y="Return"))

png('vix.ndr.rel.png')
p
dev.off()

###
# plots of vix vs spx rv/hv, use a subset 2011 and 2012
###
spxvals <- spxvals['2011/2012']
# my plots-----------------------------------
# all
#  pdf link does not work
# pdf("./pictures")
plot(as.vector(spxvals$hv), type='l', main = "Historic, implied and realised vol",
     ylab = "volatility")
lines(1:nrow(spxvals),as.vector(spxvals$iv), col='red')
lines(1:nrow(spxvals),as.vector(spxvals$rv), col='blue')
legend(x= 18, y = 48, legend=c("hv", "iv", "rv"), lty = 1, 
       col=c("black", "red", "blue"))
# dev.off()
# iv and hv
plot(as.vector(spxvals$hv), type='l', main = "Historic, and implied vol",
     ylab = "volatility")
lines(1:nrow(spxvals),as.vector(spxvals$iv), col='red', ylab = "volatility")
legend(x= 18, y = 48, legend=c("hv", "iv"), lty = 1, 
       col=c("black", "red"))
#iv rv
plot(as.vector(spxvals$rv), type='l', main = "Realised, and implied vol",
     ylab = "volatility")
lines(1:nrow(spxvals),as.vector(spxvals$iv), col='red', ylab = "volatility")
legend(x= 18, y = 48, legend=c("rv", "iv"), lty = 1, 
       col=c("black", "red"))


plot(as.vector(spxvals$rv), type='l')
lines(1:nrow(spxvals),as.vector(spxvals$iv), col='red')

head(spxvals)

##ggplot of vix vs spx rv
df <- data.frame(date=index(spxvals), RzdVol=as.vector(spxvals[,3]), VIX=as.vector(spxvals[,4]))
df <- melt(df, id="date")
title <- 'VIX vs. SPX Realized Vol\n2011-2012'
p <- ggplot(df, aes(x=date, y=value, group=variable)) + geom_line(aes(color=variable, name='a'))  
#p + theme(title=title, plot.title=theme_text(size=20)) 
p <- p + labs(list(x="", y="Value")) + scale_color_hue(name='Data')
p + theme(legend.position='top')
p <- p + theme(legend.position=c(0.9, 0.85)) 
p <- p + theme(legend.background=element_rect(fill='white'))
p <- p + labs(title=title) 

##My plot of difference between two------------------------------
diffvals1 <- as.vector(spxvals$iv - spxvals$rv)
plot(diffvals, type = 'l', main = "Spread between implied and realised vol",
     ylab = "volatility")
abline(h = 0)
hist(diffvals1, prob = TRUE)
lines(density(diffvals1))
diffvals2 <- as.vector(spxvals$rv - spxvals$hv)
plot(diffvals, type = 'l', main = "Spread between realised and historic vol",
     ylab = "volatility")
abline(h = 0)
hist(diffvals2)
diffvals3 <- as.vector(spxvals$iv - spxvals$hv)
plot(diffvals, type = 'l', main = "Spread between implied and historic vol",
     ylab = "volatility")
abline(h = 0)
hist(diffvals3)



qplot(diffvals, geom='density', binwidth=5)
diff_df <- data.frame(date=index(spxvals), diff=diffvals)
head(diff_df)
#qplot(x=date, y=diff, data=df, geom='line')
#qplot(x=date, y=diff, data=df, geom='bar', stat='identity')
#ggplot(df, aes(x=date, y=diff)) + geom_bar(stat='identity')
p2 <- ggplot(diff_df, aes(x=date, y=diff)) + geom_line(aes(colour=diff)) + 
  theme(legend.position="none")
p2 <- p2 + geom_abline(intercept=0, slope=0, color='red') 
p2 <- p2 + geom_hline(yintercept=0, color='red', alpha=0.7) 
p2 <- p2 + labs(list(x='Date', y='Difference'))
p2 <- p2 + labs(title='Difference')

#grid.arrange(p, p2, heights = c(0.9, 0.5), widths=c(0.9, 0.9), main=title)
png('vix.vs.spx.rv.png')
grid.arrange(p, p2, heights = c(0.9, 0.5), widths=c(0.9, 0.9))
dev.off()

###end plot of vix vs spx rv

####ggplot of vix spx hv
df <- data.frame(date=index(spxvals), HistVol=as.vector(spxvals[,2]), VIX=as.vector(spxvals[,4]))
df <- melt(df, id="date")
title <- 'VIX vs. SPX Historical Vol\n2011-2012'
p <- ggplot(df, aes(x=date, y=value, group=variable)) + geom_line(aes(color=variable, name='a'))  
#p + opts(title=title, plot.title=theme_text(size=20)) 
p <- p + labs(list(x="", y="Value")) + scale_color_hue(name='Data')
#p + opts(legend.position='top')
p <- p + opts(legend.position=c(0.9, 0.85)) 
p <- p + opts(legend.background=theme_rect(fill='white'))
p <- p + opts(title=title) 

##ggplot of difference between two
diffvals <- as.vector(spxvals$iv - spxvals$hv)
#hist(diffvals)
#qplot(diffvals, geom='density', binwidth=5)
diff_df <- data.frame(date=index(spxvals), diff=diffvals)
head(diff_df)
#qplot(x=date, y=diff, data=df, geom='line')
#qplot(x=date, y=diff, data=df, geom='bar', stat='identity')
#ggplot(df, aes(x=date, y=diff)) + geom_bar(stat='identity')
p2 <- ggplot(diff_df, aes(x=date, y=diff)) + geom_line(aes(colour=diff)) + opts(legend.position="none")
#p2 <- p2 + geom_abline(intercept=0, slope=0, color='red') 
p2 <- p2 + geom_hline(yintercept=0, color='red', alpha=0.7) 
p2 <- p2 + labs(list(x='Date', y='Difference'))
#p2 <- p2 + opts(title='Difference')

#grid.arrange(p, p2, heights = c(0.9, 0.5), widths=c(0.9, 0.9), main=title)
png('vix.vs.spx.hv.png')
grid.arrange(p, p2, heights = c(0.9, 0.5), widths=c(0.9, 0.9))
dev.off()

### end plot of vix vs spx hv
