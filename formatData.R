
dataDir = "D:/projects/PriceAnalisys/PriceAnalisys/prices"

dateFormat = "%d/%m/%Y"

FormatTable = function(table){
  fmt = data.frame(as.Date(table$data,dateFormat))
  names(fmt) = "date"
  fmt$price = table$precomin
  fmt
}

LoadData = function(n = 1){
  if(is.character(n)){
    files = paste(dataDir,"/",n,sep="")
    table = read.csv(files,sep = ";")
  } else{
    files = paste(dataDir,"/",list.files(dataDir),sep="")
    table = read.csv(files[n],sep = ";")
  }
  
  
  fmtTable = FormatTable(table)
  name = tail(strsplit(files[n],"[/.]")[[1]],n=2)[1]
  
  data = list(name = name,
              table = fmtTable,
              dir = dataDir,
              nrow = nrow(table),
              ncol = ncol(table),
              initialPrice = fmtTable$price[1])
}