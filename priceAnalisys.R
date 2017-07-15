
BuySell = function(data, buy, sell){
  buySell = data$table$price*0
  
  isMine = FALSE
  for(day in 1:data$nrow){
    if(!isMine){
      if(buy[day]){
        isMine = TRUE
        buySell[day] = -1
      }
    } else {
      if(sell[day]){
        isMine = FALSE
        buySell[day] = 1
      }
    }
  }
  
  if(isMine)
    buySell[day] = 1
  
  buySell
}

EvaluateGain = function(data, buy, sell){
  sum(BuySell(data, buy, sell) * 
    data$table$price/
    data$initialPrice)  
}

BuyPrice = function(data, days = 30, percent = 20){
  
  begin = min(data$table$date) + days
  
  evaluate = function(referenceDate){
    
    dateCondition = TRUE #referenceDate>=begin
    
    if(dateCondition){
      
      interval = GetInterval(data$table, referenceDate, days)
      rows = nrow(interval)
      
      referenceValue = data$table$price[match(referenceDate,data$table$date)]
      posValue = sum(interval$price<referenceValue)+1
      
      ### 
      if(percent == 0)
        posValue == 1
      else
        posValue/rows < percent/100
      
    } else {
      FALSE
    }
  }
  
  sapply(data$table$date, evaluate)
  
}

SellPrice = function(data, days = 30, percent = 80){
  
  begin = min(data$table$date) + days
  
  evaluate = function(referenceDate){
    
    dateCondition = TRUE #referenceDate>=begin
    
    if(dateCondition){
      
      interval = GetInterval(data$table, referenceDate, days)
      rows = nrow(interval)

      referenceValue = data$table$price[match(referenceDate,data$table$date)]
      posValue = sum(interval$price<referenceValue)+1
      
      ### 
      if(percent == 100)
        posValue == days
      else
        posValue/rows > percent/100
      
    } else {
      FALSE
    }
  }
  
  sapply(data$table$date, evaluate)
  
}

GoodPrice = function(data, days = 30, percent = 20, useRegression = FALSE){
  
  begin = min(data$table$date) + days
  
  evaluate = function(referenceDate){
    
    dateCondition = TRUE #referenceDate>=begin
    
    if(dateCondition){
      
      interval = GetInterval(data$table, referenceDate, days)
      rows = nrow(interval)
      
      if(useRegression) {
        predValue = RegressionAnalisys(interval, rows, referenceDate)
      } else { 
        predValue = data$initialPrice
      }
      
      referenceValue = data$table$price[match(referenceDate,data$table$date)]
      posValue = sum(interval$price<referenceValue)+1
      
      if(percent == 0)
        minimal = posValue == 1
      else
        minimal = posValue/rows < percent/100
      
      ### 
      minimal && referenceValue <= min(data$initialPrice, predValue)
      
    } else {
      FALSE
    }
  }
  
  sapply(data$table$date, evaluate)

}

GetInterval = function(table, today, days = 30){
  compare = function(date) 
    date > (today - days) && date <= today
  
  table[sapply(table$date,compare),]
}

RegressionAnalisys = function(interval, rows, referenceDate){
  if(rows>10){
    model = lm(price~date, interval)
    predValue = predict.lm(model,data.frame(date = referenceDate))
  } else predValue = 0
}
