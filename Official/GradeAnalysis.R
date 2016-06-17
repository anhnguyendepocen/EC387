#Grade analysis 
da <- read.csv("Official/Marks2016.csv", stringsAsFactors = FALSE)
head(da, n = 25)
# Calculates the total when there are only three assignments (for comparison)
#da$T1 <- da$HF * .6/3 + da$Event * .6/3 + da$Technical * .6/3 + da$Present *.4
da$Total <- round(da$HF * .6/4 + da$Event * .6/4 + da$Technical * .6/4 + da$Four * .6/4 + da$Present *.4, 1)
# Next 3 lines for Donna. 
da$Assignment <- (da$HF + da$Event + da$Technical + da$Four)/4
#------------------------------
a <- print.data.frame(da[,c(1:5, 7, 8, 6)], row.names = FALSE, digits = 1)
write.table(a, "C://Users/rh49/Desktop/EC387Marks.csv", sep = ",", row.names = FALSE)
# Now print but getting da2 
head(da)
#--------------------------------
myStats <- function(x, na.omit=FALSE){
  if (na.omit)
  x <- x[!is.na(x)]
  m <- mean(x)
  md <- median(x)
  s <- sd(x) 
  max <- max(x)
  min <- min(x)
  n <- length(x)
  return(c(n=n, mean=m, median = md, stdev=s, max = max, min = min))
}
Stats <- apply(da[,c(3, 4, 5, 7, 6, 9)], 2, FUN = myStats, na.omit = TRUE)
round(Stats, 2)
#--------------------------------

boxplot(da[,c(2:5, 7)], main = "Boxplot of assignment results")  
=======
#----Boxplots
boxplot(da[,"HF"], da[,"Event"], da[,"Technical"], da[,"Four"], 
        main = "Boxplot of Four Assignments", col = "lightblue", 
        names = c("HF", "Event", "Technical", "Four"))
# Boxplot 2
boxplot(da[,"HF"], da[,"Event"], da[,"Technical"], da[,"Four"], 
        da[, "Present"], da[, "T2"], 
        main = "Boxplot of Assignments", col = "lightblue", 
        names = c("HF", "Event", "Technical", "Four", "Presentation", "Total"))

#----------------------------barplots of grades
par(mfrow = c(2,2))
assignments <- c("HF", "Event", "Technical")
breaks <- c(20, 30, 40, 50, 60, 70, 80, 90, 100)
for(i in assignments){
barplot(da[order(-da[i]),i], names.arg = da[order(-da[i]), "Last.Name"],
        las = 2, main = paste("EC387 Order of", i, 
        "Assignment", sep = " "), col = "lightblue")
# Obviously supress names for publication.
abline(h = mean(da[,i], na.rm = TRUE))
text(x = 23, y = 70, labels = paste("Mean Grade","=", 
            round(mean(da[,i], na.rm = TRUE), 2), sep = " "))
}
# maybe make these the same xaxis or same bins in future.
par(mfrow = c(2,2))
for(i in assignments){
hist(da[,i], probability = TRUE,  main = paste("Dispursion of grades for", i,
     "Assignment", sep = " "), breaks = breaks, col = "lightblue", xlab = "")
}

