BAC <- read.csv("Returns/Data/BAC.csv")
BAC$Date <- as.Date(BAC$Date, format = "%d/%m/%Y")
BAC <- BAC[,c(1, 7)]
colnames(BAC) <- c("Date", "Close")
BAC <- BAC[rev(rownames(BAC)),]
BACR <- diff(BAC$Close) / BAC$Close[-length(BAC$Close)] 
BACr <- diff(log(BAC$Close))
BAC <- cbind(BAC[-1,], BACR, BACr)
head(BAC)
# Descriptive statistics of RR-------------------------------
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  md <- median(x)
  s <- sd(x) 
  t <- m/s
  n <- length(x)
  skew <- sum((x-m)^3/s^3)/n
  ps <- ((6*n*(n-1))/((n-1)*(n+1)*(n+3)))^0.5
  kurt <- sum((x-m)^4/s^4)/n - 3
  pk <- ((n^2-1)/((n-3)*(n + 5)))^0.5
  max <- max(x)
  min <- min(x)
  return(c(n=n, mean=m, t= t, median = md, stdev=s, skew=skew, ses = 2 * ps, 
           kurtosis=kurt, sek  = 2 * pk, max = max, min = min))
}
round(mystats(BACr, na.omit = TRUE), 4)     
#--------------------------------------
# Sample
n <- 1000 # sample size
MeanSample <- c(rep(NA, length(BACr)))
for(i in 1:n){
  temp <- (matrix(NA, nrow = length(BACr), ncol = n))
  temp[,i] <- sample(BACr, size = length(BACr), replace = TRUE)
  MeanSample[i] <- mean(temp[,i], na.rm = TRUE)
}
hist(MeanSample)
# this code is not good. It is too slow.  The large matrix is not good.