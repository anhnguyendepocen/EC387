names <- c("Date", "Brent")
da <- read.csv("Data/CHRIS-ICE_B1.csv")
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
da <- da[,c(1, 5)]
names2 <- c("Date", "Gas")
names(da) <- names
da2 <- read.csv("Data/CHRIS-ICE_G1.csv")
da2$Date <- as.Date(da2$Date, format = "%d/%m/%Y")
da2 <- da2[,c(1, 5)]
names(da2) <- names2
da3 <- left_join(da, da2, na.rm = TRUE)
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