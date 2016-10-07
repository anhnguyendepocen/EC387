# this comes from 
#http://rcrastinate.blogspot.co.uk/2016/07/troubles-with-cell-labels-in-mosaic.html
tab <- rbind(c(450, 230), c(200, 600))
library(vcd)
mosaic(tab, shade = T)
lab.tab <- rbind(c("++", "--"), c("--", "++"))
mosaic(tab, labeling = labeling_cells(text = lab.tab, shade = T))
dimnames(tab) <- list(VarA = c("levelA", "levelB"), VarB = c("levelA", "levelB"))
#-----------------------------------------
library(RColorBrewer)
colours <- brewer.pal(n = 4, "Greens")
GreenCab <- rep("Green", 20*17)
BlueCab <- rep("Blue", 3*20)
da1 <- (c(GreenCab, BlueCab))
x <- table(da1)
# off will determine the split between the two categories
mosaicplot(x, color = colours, xlab = "", 
           main = "Proportion of cars in town", off = 0)

ID <- rep(1, length(da1))
da2 <- data.frame(da1, ID)
for(i in 1:51){
  da2$ID[i] <- 0
} 
for(i in 341:349){
  da2$ID[i] <- 0
}
x2 <- table(da2)
mosaicplot(x2, main = "Blue and Green Cabs correct and incorrect identification",   
           xlab = "Cab colour", ylab = "Identification", color = colours) 

