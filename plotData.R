library(ggplot2)

PlotHistory = function(data){
  ggplot(data$table,
         aes(date,price)) +
  geom_line(colour='darkblue') + 
    theme_bw() +
    theme(panel.grid = element_blank()) +
    xlab("Date") + 
    ylab("Price") +
    ggtitle("Price Analisys") +
  #geom_point(aes(date,price))
}

PlotGoodPrices = function(data, goodPrice){
  points(data$table$date[goodPrice], 
         data$table$price[goodPrice],
         pch = 16,
         cex = 1,
         col = rgb(1,0,0,0.5))
}

PlotBuyPrices = function(data, buyPrice, alpha = 0.5){
  points(data$table$date[buyPrice], 
         data$table$price[buyPrice],
         pch = 16,
         cex = 1,
         col = rgb(1,0,0,alpha))
}

PlotSellPrices = function(data, sellPrice, alpha = 0.5){
  points(data$table$date[sellPrice], 
         data$table$price[sellPrice],
         pch = 16,
         cex = 1,
         col = rgb(0,0,1,alpha))
}

mergeFlags = function(matrix){
  apply(matrix, 1,function(x) sum(x)>0.5)
}