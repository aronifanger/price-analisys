library(forecast)
library("Metrics")

filesPath = "D:/projects/PriceAnalisys/PriceAnalisys"
setwd(filesPath)

source("formatData.R")
source("plotData.R")

filesPath = "D:/projects/PriceAnalisys/PriceAnalisys/prices"
setwd(filesPath)

### Load dataset
data = LoadData(12)

### Range config
days = 145
ptrain = 0.7

### Control parameters
nfinal = nrow(data$table)
ninit = nfinal - days
nsplit = ninit + floor(days*ptrain)

ymax = max(data$table$price)
ymin = min(data$table$price)
yrange = ymax - ymin

### Training dataset
prices = data$table$price

### Train ARIMA
ts = as.ts(prices[1:nsplit])
fit = auto.arima(ts)

plot(forecast(fit,h=nfinal-nsplit))



KFoldLimits = function(nRow, numSplits = 10, pTrain = 0.7, pDiscard = 0.2){
  nDiscard = max(floor(nRow*pDiscard),numSplits)
  
  nFold = nRow - nDiscard
  init  = ceiling(nDiscard*(1:numSplits)/numSplits)
  end   = init + nFold
  split = init + floor(nFold*pTrain)
  
  cbind(init,split,end)
}

TestForecasting = function(train, test){
  ts    = as.ts(train$price)
  model = auto.arima(ts)
  forec = forecast(model,nrow(test))
  mse(forec$mean,test$price)
}

