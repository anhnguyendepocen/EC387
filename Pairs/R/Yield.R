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
eq1 <- lm(da[,2] ~ da[,1])
summary(eq1)
names(eq1)
plot(eq1$residuals, na.rm = TRUE, type = 'l')
  #Obviously this does not work well
plot(da[,1] - da[,2], type = 'l')
