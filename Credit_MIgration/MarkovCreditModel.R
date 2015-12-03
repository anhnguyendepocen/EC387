T <- matrix(c(0.7, 0.3, 0, 0.2, 0.6, 0.2, 0, 0, 1), byrow = T, nrow = 3)
T
D <- c(0.1, 0.4, 1)
D
D%*%t(T)
default <- function(D, T){
  DR <- matrix(rep(NA, 30), ncol = 10)
  DR[,1] <- D 
    for(i in 2:10){
      DR[,i] <- DR[,i-1]%*%t(T)
    }
  return(DR)
    }
default(D, T)
  
