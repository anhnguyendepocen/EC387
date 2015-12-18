#Grade analysis 
da <- read.csv("Official/Marks2016.csv", stringsAsFactors = FALSE)
head(da)
barplot(da$Grade[order(-da$Grade)], names.arg = da$Last.Name
        [order(-da$Grade)], las = 2, main = "EC387 Order of first 
        Assignment", col = "lightblue")
# Obviously supress names for publication.
abline(h = mean(da$Grade))
text(x = 23, y = 64, labels = paste("Mean Grade","=", 
            round(mean(da$Grade), 2), sep = " "))
hist(da$Grade, probability = TRUE,  main = "Dispursion of grades", 
     breaks = 10, col = "lightblue")
