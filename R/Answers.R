# Here are the answers. 
# You can run this line-by-line by pressing CONTROL R from the beginning
(25 + 31 + 3 + 5 - 30)/345
# Make sure that you use the braket
#---------------
(25 * 400)^0.5
#^0.5 is the square root
sqrt(25 * 400) 
# is the same
#---------------
B <- 7.4/4.2
B/10
C <- B + 4
C
#----------------
Answer <- 7.4/4.2
B <- Answer + B
B > Answer
# Is B bigger than Answer? 
B
Answer
#------------------------------
Tom <- c(5, 6, 7, 8)
Tom + 2
# 2 is added to each element of the vector
Tom * 2
#------------------------------
newVector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
newVectorSimple <- seq(1, 10, 1)
newSeq <- seq(10, 100, by = 10)
newSeq <- seq(10, 100, length = 10) 
#-----------------------------
Id <- matrix(c(1, 0, 0, 1), nrow = 2)
Id
#----------------------
Tom <- matrix(seq(1, 9, 1), nrow = 3)
Tom
# Old Tom is gone
Tom <- matrix(seq(1, 9, 1), nrow = 3, byrow = TRUE)
Tom
#-----------------------------------
x <- 1:10
y <- 2 * x
plot(x, y)
plot(x, y, type = 'l')
plot(x, y = x^2, type = 'l')
plot(x, y = 15 - 5 * x + x^2, type = 'l')
?plot
plot(x, y = 15 - 5 * x + x^2, type = 'l', main = "My graph")

