 names <- c("2-year", "10-year", "Date")
 da <- read.csv("Pairs/Data/Yield.csv", stringsAsFactors = FALSE, skip = 12, 
                na.strings = "#N/A")
 da$Date <- as.Date(da[,1], format = "%Y-%m-%d")
 da <- da[,c(2, 3, 4)]
 names(da) <- names
 head(da)
 tail(da)
 str(da)
 plot(da$Date, da[,1], main = "US 2 and 10 year yields", ylab = "GRY", 
      xlab = "Date", type = 'l', lty = 1, ylim = c(0, 5))
 lines(da$Date, da[,2], lty = 2, type = 'l', col = "Dark Green")
 legend("topright", legend = c("2-year", "10-year"), col = c("black", "Dark Green"), 
        lty = c(1, 2))
library(xtable)
eq1 <- lm(da[,2] ~ da[,1])
rownames(summary(eq1)) <- c("Intercept", "2-year")
tab2 <- as.matrix(tab1)

rownames(xtable(summary(eq1))) <- c("intercept", "2-year")
names(eq1)
plot(da$Date, eq1$residuals, type = 'l')
  #Obviously this does not work well
plot(da[,1] - da[,2], type = 'l')
length(da$Date)
length(eq1$residuals)
head(da)
tail(da)
head(eq1$residuals)
tail(eq1$residuals)
