T <- matrix(c(0.8, 0.2, 0.1, 0.9), byrow = T, nrow = 2)
D <- c(0.1, 0.8)
D
default <- function(D, T){
  DR <- matrix(rep(NA, 20), ncol = 10)
  DR[,1] <- D 
    for(i in 2:10){
      DR[i] <- T%*%DR[,i-1]
    }
  return(DR)
    }
default(D, T)

