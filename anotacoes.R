library(forecast)


filesPath = "D:/projects/PriceAnalisys/PriceAnalisys"
setwd(filesPath)

source("priceAnalisys.R")
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

### Plot original data
plot(data$table$price, type="l", xlim = c(1, nfinal), ylim = c(ymin - yrange/2, ymax + yrange/2),
     xlab = "", ylab = "")

par(new = TRUE)
plot(forecast(fit,h=nfinal-nsplit), xlim = c(1, nfinal), ylim = c(ymin - yrange/2, ymax + yrange/2))
lines(nsplit:nfinal,data$table$price[nsplit:nfinal])
#abline(h=mean(ts))

#ts.diff = diff(ts)
#par(mfrow=c(2,1))
#plot(ts)
#plot(ts.diff)
#par(mfrow=c(1,1))



gfit.ts <- garch(sp5.ret[,1])
coef(gfit.ts)
plot(sqrt(252) * gfit.ts$fitted.values[, 1], type="l")
