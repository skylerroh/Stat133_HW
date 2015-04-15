fileNames = c("f.csv", "gm.csv", "ibm.csv", "intc.csv")
fileURLs = paste("http://www.stat.berkeley.edu/~nolan/data/stocks/", fileNames, sep = "")


readData = function(fileName, dateFormat = "%m/%d/%y"){
  stockData = read.csv(fileName, colClasses = c("character", NA, NA, NA, NA, NA, "numeric"))
  stockData$Date = as.Date(stockData$Date, dateFormat)
  stockData = stockData[order(stockData$Date), ]
  return (stockData[c("Date", "Adj.Close")])
}

stockList = lapply(fileURLs, readData)
names(stockList) = c("Ford", "GM", "IBM", "Intel")

combine2stocks = function(stockA, stockB){
  stockA = stockA[stockA$Date %in% stockB$Date, ]
  stockB = stockB[stockB$Date %in% stockA$Date, ]
  stockAB = data.frame(stockA$Date, stockA$Adj.Close, stockB$Adj.Close, stockA$Adj.Close / stockB$Adj.Close)
  names(stockAB) = c("Date", "StockAAdj.Close", "StockBAdj.Close", "Ratio")
  return (stockAB)
}

plotRatio = function(ratio, k = 1, date = seq(along = ratio), ...){
  plot(ratio ~ date, type = "l", xlab = "Date", ylab = "Ratio")
  abline(h = mean(ratio), lty = "dashed", col = "green")
  abline(h = mean(ratio) - (c(-k, k) * sd(ratio)), lty = "dashed", col = "red")
}

showPosition = function(pos, ratios, col = c("green", "red"), radius = 100){
  symbols(x = pos, y = ratios, circles = rep(radius, times = length(pos)), , fg = col, inches = FALSE, add = TRUE)
}
  
#used for tests
#IBMIntel = combine2stocks(stockList$IBM, stockList$Intel)
#plotRatio(IBMIntel$Ratio, date = IBMIntel$Date)
#showPosition(pos = IBMIntel$Date[c(280, 770)], ratios = IBMIntel$Ratio[c(280, 770)])
  
#plotRatio(IBMIntel$Ratio)
#showPosition(pos = c(280, 770), ratios = IBMIntel$Ratio[c(280, 770)])

