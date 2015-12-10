require(tseries)
require(quantmod)
da <- read.csv("BehaviouralFinance/Data/GSPC.csv")
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
da1 <- da[(da$Date  > as.Date("2014-12-25", format = "%Y-%m-%d") 
           & da$Date < as.Date("2015-02-15", format = "%Y-%m-%d")),]
da1 <- da1[rev(rownames(da1)),]
# create net change oscilator
da1$NC <- da1$Close / Lag(da1$Close, k = 1) *100 - 100
dat <- as.ts(da1[,c(2:5)])
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plotOHLC(dat, origin = "2014-12-25", ylab = "SP500", main = "Christmas 2014", xlab = "", xaxt = "n") 
plot(da1$Date, da1$NC, type = 'l', ylab = "Net Change Oscillator", xlab = "Date")
abline(h = 1, col = 'red', lty = 2)
abline(h = -1, col = 'red', lty = 2)
#------------------------------------
da$MA3 <- SMA(da$Close, 3)
da$MA20 <- SMA(da$Close, 20)
head(da)
