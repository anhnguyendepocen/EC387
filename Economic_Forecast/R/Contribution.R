library(dplyr)
library(tidyr)
library(RColorBrewer)
colors <- brewer.pal(n = 4, "Greens")
names  <- c("Country", "Currency", "Year", "Final_Consumption", "Consumption",
            "Government", "GCF", "GFCF", "Inventories", "Exports", "Imports", "GDP")
da <- read.csv("../ECM04/Financial_System/Data/UNGNP.csv")
da <- da[,-13]
colnames(da) <- names
# Change the country for savings ratio
da <- filter(da, Country == "Germany") 
da <- mutate(da, CC = (Consumption - lag(Consumption))/lag(GDP))
da <- mutate(da, CG = (Government - lag(Government))/lag(GDP))
da <- mutate(da, CI = (GFCF - lag(GFCF))/lag(GDP))
da <- mutate(da, CNX = ((Exports-Imports) - (lag(Exports) - lag(Imports)))/
               lag(GDP))
head(da)
da <- select(da, Year, CC, CG, CI, CNX) 
#select the last three years
da <- da[(dim(da)[1]-3):dim(da)[1], ]
dam <- as.matrix(da)
damt <- t(da)
damt1 <- damt[-1,]
colnames(damt1) <- damt[1,]
barplot(damt1, beside  = TRUE, main = "Components of German GDP growth", 
        col = colors)
legend("topright", legend = c("C", "G", "I", "NX"), fill = colors)
@