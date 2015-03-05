library(tidyr)
library(dplyr)
library(RColorBrewer)
colors <- brewer.pal(n = 5, "Greens")
dates <- (2015:2018)
C <- c(25, 30, 40, 34)
G <- c(5, 5, 2, 5)
I <- c(25, 25, 25, 22)
X <- c(15, 14, 15, 19)
M <- c(20, 20, 1, 4)
NX <- X-M
NX
GDP <- C + I + G + NX
LGDP <- lag(GDP)
LGDP
da <- data.frame(dates, GDP, C, I, G, NX, LGDP)
# Function Cont calculates the contrbution of each component
cont <- function(x, base){
 c = (x - lag(x))/base  
 }
#-------------------------------------------
dam <- as.matrix(da)
dam
dams <- apply(dam, 2, FUN = cont, base = LGDP)


dams <- dams[(dim(dam)[1]-1):dim(dam)[1],-c(1, 2, 7)]
dams 
damt <- t(dams)
damt
damt1 <- damt[-1,]
damt[1,]
colnames(damt1) <- c(2014, 2015)
dim(dam)[2] - dim(damt1)[2]
col <- c('red', 'blue', 'dark green', 'yellow', 'black')
barplot(damt1, beside  = TRUE, main = "Contributions to GDP growth", col = colors)
legend("topright", legend = c("C", "G", "I", "X"), fill = colors)
#------------------
colors4 <- brewer.pal(n = 4, "Greens")
da1 <- data.frame(C, G, I, NX)
da1m <- as.matrix(da1)
da1mt <- t(da1m)
colnames(da1mt) <- dates
barplot(da1mt, beside = FALSE, main = "GDP components", col = colors4)
legend("topright", legend = c("C", "G", "I", "NX"), fill = colors4)
#---------------
stocks <- data.frame(
    time = as.Date('2009-01-01') + 0:9,
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
  )
stocks
stocksm <- stocks %>% gather(stock, price, -time)
stocksm
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)  
#=======
dates <- (2015:2017)
C <- c(25, 30, 40)
G <- c(5, 5, 2)
I <- c(25, 25, 25)
X <- c(15, 14, 15)
M <- c(20, 20, 1)
da <- data.frame(dates, C, G, I, X, M)
dam <- as.matrix(da)
damt <- t(dam)
damt
damt1 <- damt[-1,]
colnames(damt1) <- dates
barplot(damt1, beside  = TRUE, main = "Contributions to GDP growth")
legend("topleft", legend = c("C", "G", "I", "X"), fill = )
# ----------
library(tidyr)
library(RColorBrewer)
colors <- brewer.pal(n = 5, "Greens")
dates <- (2015:2018)
C <- c(25, 30, 40, 34)
G <- c(5, 5, 2, 5)
I <- c(25, 25, 25, 22)
X <- c(15, 14, 15, 19)
M <- c(20, 20, 1, 4)
NX <- X-M
NX
GDP <- C + I + G + NX
LGDP <- lag(GDP)
LGDP
da <- data.frame(dates, GDP, C, I, G, NX, LGDP)
# Function Cont calculates the contrbution of each component
cont <- function(x, base){
 c = (x - lag(x))/base  
 }
#-------------------------------------------
dam <- as.matrix(da)
dam
dams <- apply(dam, 2, FUN = cont, base = LGDP)


dams <- dams[(dim(dam)[1]-1):dim(dam)[1],-c(1, 2, 7)]
dams 
damt <- t(dams)
damt
damt1 <- damt[-1,]
damt[1,]
colnames(damt1) <- c(2014, 2015)
dim(dam)[2] - dim(damt1)[2]
col <- c('red', 'blue', 'dark green', 'yellow', 'black')
barplot(damt1, beside  = TRUE, main = "Contributions to GDP growth", col = colors)
legend("topright", legend = c("C", "G", "I", "X"), fill = colors)
#------------------
colors4 <- brewer.pal(n = 4, "Greens")
da1 <- data.frame(C, G, I, NX)
da1m <- as.matrix(da1)
da1mt <- t(da1m)
colnames(da1mt) <- dates
barplot(da1mt, beside = FALSE, main = "GDP components", col = colors4)
legend("topright", legend = c("C", "G", "I", "NX"), fill = colors4)
#---------------
stocks <- data.frame(
    time = as.Date('2009-01-01') + 0:9,
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
  )
stocks
stocksm <- stocks %>% gather(stock, price, -time)
stocksm
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)  

