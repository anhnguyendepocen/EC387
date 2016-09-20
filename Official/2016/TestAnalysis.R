# The file has to be downloaded with commar from blackboard by 
# question and user for all attempts.  This is saved as a txt file, 
# opened as excel and saved as a csv 
library(dplyr)
da <- read.csv("./Official/test.csv", stringsAsFactors = FALSE)
head(da)
dat <- da[, c(4, 8, 9)]
colnames(dat) <- c("Question", "Auto", "Man")
head(dat)
# replace anything that is not a number with just the number. 
dat[,1] <- as.numeric(gsub("[^0-9]", "", dat[,1]))
dat[,2] <- as.numeric(dat[,2])
dat[,3] <- as.numeric(dat[.3])
str(dat)
# turn all dat into zero
dat[is.na(dat)] <- 0
dat$Total <- dat$Auto + dat$Man
# complete
group_by(dat, Question)%>%
  summarise(round(mean(Total, na.rm = TRUE), 2)) %>% 
  head(n = 25) 
#---------------------------
head(dat)
table(dat$Total[which(dat$Question == 3)])
Marks <- rep(NA, 25)
for(i in 1:25){
Marks[i] <- mean(dat$Total[which(dat$Question == i)], na.rm = TRUE)
}
Marks
