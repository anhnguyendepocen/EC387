# This is a sample for simulating for confidence levels. 
require("eventstudies")
set.seed(5)
# credit 1000 random returns
rets <- rnorm(1000, 0.01, 0.05)
# create a sequence of dates
date <- seq(from = as.Date("2000-01-01"),  by = 1, length.out = 1000)
retsx <- as.xts(rets, order.by = date)
#create an xts object
head(retsx)
colnames(retsx) <- "BAC"
eventDates <- sample(date, 10)
length(eventDates)
BAC <- rep("BAC", 10)
BAC
#create the required dataframe
eventdf <- data.frame(unit = BAC, when = eventDates)
eventdf
es <- phys2eventtime(retsx, events = eventdf, width = 5)
es.w <- window(es$z, start = -5, end = 5)
head(es.w)
tail(es.w)
es.cs <-remap.cumsum(es.w, is.pc = FALSE, base = 0)
result <- inference.Ecar(es.cs, to.plot = TRUE)
