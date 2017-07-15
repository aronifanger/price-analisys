
filesPath = "D:/projects/PriceAnalisys/PriceAnalisys"
setwd(filesPath)

source("priceAnalisys.R")
source("formatData.R")
source("plotData.R")

PlotAnalisys = function(n = 1){
  head = n[1]
  tail = n[-1]
  
  data = LoadData(head)
  PlotHistory(data)
  
  goodPrice1 = GoodPrice(data,120,20, TRUE)
  
  flags = cbind(goodPrice1)
  
  PlotGoodPrices(data, mergeFlags(flags))
  
  if(length(tail)>0)
    PlotAnalisys(tail)
}

#par(mfrow = c(3,4))
#PlotAnalisys(1:12)
#dev.off()

PlotBuySell = function(n = 1, days = 30, perc = 20){
  head = n[1]
  tail = n[-1]
  
  data = LoadData(head)
  PlotHistory(data)
  
  buyPrice = BuyPrice(data,days,perc)
  sellPrice = SellPrice(data,days,100-perc)
  
  buySell = BuySell(data, buyPrice, sellPrice)
  
  PlotBuyPrices(data, buySell == -1, alpha = 0.8)
  PlotSellPrices(data, buySell == 1, alpha = 0.8)
  
  if(length(tail)>0)
    PlotAnalisys(tail)
}

TestGain = function(n = 1, days = 30, perc = 20){
  data = LoadData(n)
  buyPrice = BuyPrice(data,days,perc)
  sellPrice = SellPrice(data,days,100-perc)
  EvaluateGain(data,buyPrice,sellPrice)
}

#for(i in 1:10) print(TestGain(i,days = 10, perc = 20))

PlotBuySell(20, days = 30, perc = 20)
