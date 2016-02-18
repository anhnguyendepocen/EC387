 names <- c("Date", "2-year", "10-year")
 da <- read.csv("Pairs/Data/Yield.csv", stringsAsFactors = FALSE, skip = 12, 
                na.strings = "#N/A")
 da$Date <- as.Date(da[,1], format = "%Y-%m-%d")
 da <- da[,c(4, 2, 3)]
 names(da) <- names
 head(da)
 tail(da)
 #get rid of days that have NAs
 da <- da[-c(which(is.na(da$`2-year`))),]
 str(da)
 plot(da$Date, da$`2-year`, main = "US 2 and 10 year yields", ylab = "GRY", 
      xlab = "Date", type = 'l', lty = 1, ylim = c(0, 5))
 lines(da$Date, da$`10-year`, lty = 2, type = 'l', col = "Dark Green")
 legend("topright", legend = c("2-year", "10-year"), col = c("black", "Dark Green"), 
        lty = c(1, 2))
library(xtable)
eq1 <- lm(`10-year` ~ `2-year`, data = da)
summary(eq1)
eq1
library(urca)
unityield <- ur.df(eq1$residuals, type = 'trend', lags = 2)
summary(unityield)
plot(da$Date, eq1$residuals, type = 'l')
plot(da$`10-year` - da$`2-year`, type = 'l')
da$R2 <- da$`2-year`/lag(da$`2-year`, 1) -1
da$R10 <- da$`10-year`/lag(da$`10-year`, 1) -1
da$LR2 <- lag(da$R2)
da$L2R2 <- lag(da$R2, 2)
da$LR10 <- lag(da$R10)
da$L2R10 <- lag(da$R10, 2)
da$Lerror <- lag(eq1$residuals)

# error-correction model      
eq2 <- lm(da$R10 ~ da$LR10 + da$Lerror + da$L2R10)
summary(eq2)
