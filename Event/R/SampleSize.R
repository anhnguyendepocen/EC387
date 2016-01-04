par(mfrow = c(2, 2))
sequence <- c(10, 100, 1000, 10000)
df <- matrix(nrow = 4, ncol = 2)
colnames(df) <- c("mean", "Var")
rownames(df) <- sequence
j = 1
for(i in sequence){
rand <- rnorm(i, 0, 1)
hist(rand)
df[j, 1] <- mean(rand)
df[j, 2] <- var(rand)
j = j + 1 
}
df
