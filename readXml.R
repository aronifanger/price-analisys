library(gdata)
library(XML)

doc.html = readHTMLTable("acoes/Historico_EMBR3.html", useInternal = TRUE)
doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))


tb = read.xls(, sheet=1)
tb = read.csv("acoes/BolsaPT_^BVSP_20001228.csv", sep = ",")
table = data.frame(as.Date(as.character(table$X.DTYYYYMMDD.),format = "%Y%m%d"))
names(table) = "date"
table$price = tb$X.OPEN.
fmtTable = table

name = "Acoes"

data = list(name = name,
            table = fmtTable,
            nrow = nrow(table),
            ncol = ncol(table),
            initialPrice = fmtTable$price[1])