
library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(corrplot)
library(forecast)
datos <- fread("data/Venta al trade 2015 a 2019 - Data.csv", encoding = 'UTF-8',
               dec = '.', header = T, data.table = F)
datos$TIPO <- NULL
datos[datos == '-'] <- 0
for(i in 5:65){
  
  datos[, i] <- as.numeric(datos[, i])
}

aa <- datos %>% group_by(SKU) %>% nest()
cc <- map(aa[[2]], function(x){
  
  colSums(x[, 5:64], na.rm = T)
  
})

cc <- as.data.frame(do.call(cbind, cc))


mesesConVar <- cc[12:59, ]

mesesConVar$diasLab <- diasLab
mesesConVar$temp <- temp

correlation <- cor(mesesConVar)

corrplot(correlation, type = "upper")

masVend <- mesesConVar[, which.max(colSums(mesesConVar))]

train <- ts(masVend[1:40])
test <- ts(masVend[41:48])

modelo <- auto.arima(train, xreg = cbind(temp[1:40], diasLab[1:40]))
pred <- forecast(modelo, xreg = cbind(temp[41:48], diasLab[41:48]))

plot(ts(c(train, pred$mean)))
lines(ts(masVend), col = 'red')
lines(temp, col = 'green', )


modeloSimp <- auto.arima(train)
predSimp <- forecast(modeloSimp, 8)

plot(ts(c(train, predSimp$mean)))
lines(ts(masVend), col = 'red')
