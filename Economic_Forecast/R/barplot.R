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