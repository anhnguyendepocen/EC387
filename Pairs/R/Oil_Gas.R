library(dplyr)
library(tidyr)
names <- c("Date", "Brent")
da <- read.csv("Pairs/Data/CHRIS-ICE_B1.csv")
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
da <- da[,c(1, 5)]
names2 <- c("Date", "Gas")
names(da) <- names
da2 <- read.csv("Pairs/Data/CHRIS-ICE_G1.csv")
da2$Date <- as.Date(da2$Date, format = "%d/%m/%Y")
da2 <- da2[,c(1, 5)]
names(da2) <- names2
tail(da2)
tail(da)
str(da2)
da3 <- left_join(da, da2, na.rm = TRUE)
tail(da3)
# These next two lines find the NA and 0 figures or gas and then remove.
da3[is.na(da3$Gas),]
da3[da3$Gas == 0,]
da3 <- da3[-c(2135,2052),]
da3$Spread <- da3$Gas - da3$Brent  
da3$Ratio <- da3$Gas/da3$Brent
plot(da3$Date, da3$Brent, type = 'l', main = "Oil and Gas", xlab = "Date", 
     ylab = "Price")
par(new = T)
plot(da3$Gas ~ da3$Date, axes = F, type = 'l', col = 'Dark Green', xlab = "", 
     ylab = "", lty = 2)
axis(side = 4)
legend("topleft", legend = c("Brent", "Gas"), col = c("black", "Dark Green"), 
       lty = c(1, 2))
plot(da3$Date, da3$Spread, type = 'l', main = "Gas minus Brent")
plot(da3$Date, da3$Ratio, type = 'l', main = "Ratio of Gas to Brent")
eq1 <- lm(log(da3$Gas) ~ log(da3$Brent))
summary(eq1)
plot(eq1$residuals, type = 'l')
hist(eq1$residuals)

eq1 <- lm(log(da3$Gas) ~ log(da3$Brent))