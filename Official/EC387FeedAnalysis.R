da <- read.csv("Official/EC387Feedback.csv", stringsAsFactors = FALSE)
# Reduce the questions to one word
Questions <- c("Overall", "Organised", "Informed", "Changes", "Contract", "Teaching", "Stimulated", "Assessment", 
               "Feedback-time", "Feedback-use", "Resources")
# These levels are actually made up.  I think that they are abaut right.
# Wrap two word labesl so that they fit without overlap.
Level <- c(paste("Strongly", "Agree", sep = "\n"), "Agree", "Unsure", "Disagree", paste("Strongly", "Disagree", sep = "\n"))
barplot(da[1,])
# Search for Joshua Callarman <J.Callarman@brighton.ac.uk> in email for full results with comments. 
str(da)
head(da, n  = 20)
#-------------------------------------
# Simple questions
row.names(da) <- Questions
#Four charts
par(mfrow = c(2, 2))
# Create room for title. 
par(oma = c(1, 0, 2, 0)) # overall margin = bottom, left, top, right?
reqQuest <- c("Overall", "Teaching", "Stimulated", "Resources")
head1 <- "Overall Satisfaction"
head2 <- "Teaching is good"
head3 <- "The module has stimulated my interest"
head4 <- "There are good learning resources"
heads <- c(head1, head2, head3, head4)
for(i in 1:4){
# Need to convert the rows to numeric to ensure that it works
# http://stackoverflow.com/questions/14484728/convert-a-row-of-a-data-frame-to-vector
  barplot(as.numeric(da[reqQuest[i], 2:6]), names.arg = Level, main = heads[i], col = "lightblue", las = 2) 
}
# Overall title for the four charts. 
# http://sphaerula.com/legacy/R/multiplePlotFigure.html
title("Student Feedback Survey", outer = TRUE)

