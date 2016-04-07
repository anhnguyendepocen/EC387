#Grade analysis 
da <- read.csv("Official/Marks2016.csv", stringsAsFactors = FALSE)
head(da)
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

