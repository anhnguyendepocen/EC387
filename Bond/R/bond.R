# Here are the data
# https://www.ecb.europa.eu/stats/money/long/html/index.en.html
cols <- c("Belgium", "Germany", "Ireland", "Greece", "Spain", "France", "Italy", 
"Cyprus", "Latvia", "Luxembourg", "Malta", "Netherlands", "Austria", "Portugal",
"Slovenia", "Slovakia", "Finland", "Bulgaria", "Czech Republic", "Denmark", 
"Croatia", "Lithuania", "Hungary", "Poland", "Romania", "Sweden", "Sweden2", "UK") 
da <- read.csv("Bond/Data/irs.csv", skip = 7, col.names = cols, strip.white = TRUE)
da$Date <- as.Date(row.names(da), format = "%b-%Y")
# http://stackoverflow.com/questions/10446833/
# how-to-convert-a-character-string-date-to-date-class-if-day-value-is-missing
da$Date <- as.Date(as.yearmon(rownames(da), format = "%b-%Y"))

head(da)
tail(da)
str(da)
plot(da$Date, da$Germany, type = 'l', main = "German 10-year", xlab = "Date", 
     ylab = "Yield")
plot(da$Date, da$Greece-da$Germany, type = 'l', main = "Greek Risk Premium", 
     xlab = "Date",  ylab = "Yield")
plot(da$Date, da$Italy-da$Germany, type = 'l', main = "Italy Risk Premium", 
     xlab = "Date",  ylab = "Yield")
