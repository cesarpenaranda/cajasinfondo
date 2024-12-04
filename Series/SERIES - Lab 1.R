library(ggfortify)
library(forecast)
library(fpp2)
library(data.table)
library(TTR)
library(xts)
library(tidyverse)
library(lubridate)
library(quantmod)

#2 Ejemplo: Pasajeros de avión

data("AirPassengers")
AirPassengers

str(AirPassengers) 
frequency(AirPassengers) 
cycle(AirPassengers)
class(AirPassengers)

AP <- as.numeric(AirPassengers)
AP

plot(AP,type="l")

#Serie con vector indexado
AP.data = data.frame(tiempo=seq_along(AP),pasajero=AP)
AP.data

plot(AP.data,type="l")

ggplot(AP.data,aes(tiempo,pasajero))+geom_line()

#Crear objeto ts
AP.ts <- ts(AP,start = c(1949,1),frequency=12)
  
#Graficar con ts
plot.ts(AP.ts)
plot(AP.ts)
autoplot(AP.ts)+labs(x="tiempo",y="pasajero",title = "Pasajeros (1949-1961)")

#Descomposicion de series 
decompose.AP <- decompose(AP.ts,type = "multiplicative")
autoplot(decompose.AP)

#Efecto estacional
boxplot(AP.ts~cycle(AP.ts))
ggseasonplot(AP.ts, year.labels=FALSE, continuous=TRUE)
ggseasonplot(AP.ts,year.labels = T,continuous = F)
#Polar
ggseasonplot(AP.ts,year.labels = F,continuous = F,polar = T)

#Ejemplo cemento
cemento<-fpp2::qcement
str(cemento)

cycle(cemento)

autoplot(cemento)
ggseasonplot(cemento,year.labels = F,continuous = T)
ggseasonplot(cemento,year.labels = F,continuous = T,polar =T)

decompose.c <- decompose(cemento,"multiplicative")
autoplot(decompose.c)

ggsubseriesplot(cemento)

boxplot(cemento~cycle(cemento))

#Lagplot
gglagplot(cemento,lags = 12)
gglagplot(cemento,lags = 12,do.lines = F)

acf(cemento)
ggAcf(cemento)
acf(cemento,plot = F)

#Ejemplo: gasto de medicamento anti-diabético (mensual)

medicamento<-fpp2::a10
autoplot(medicamento)
ggseasonplot(medicamento,year.labels = F,continuous = T)
ggseasonplot(medicamento,year.labels = F,continuous = T,polar =T)
ggsubseriesplot(medicamento)

decompose.m <- decompose(medicamento,type="multiplicative")
autoplot(decompose.m)


#Ejemplo: Producción de cerveza en Australia
cerveza<-fpp2::ausbeer
autoplot(cerveza)
ggseasonplot(cerveza,year.labels = F,continuous = T)
ggseasonplot(cerveza,year.labels = F,continuous = T,polar =T)
ggsubseriesplot(cerveza)

#Lagplot
gglagplot(cerveza,lags = 16)
gglagplot(cerveza,lags = 16,do.lines = F)

h=1
gglagplot(cerveza,lags=h,do.lines=FALSE)


cerveza.shift<-shift(cerveza,n=4,type="lag")
cbind(cerveza,cerveza.shift)

plot(cerveza~cerveza.shift,xlim=c(200,600),ylim=c(200,600),
     xy.labels=FALSE,col=cycle(cerveza),pch=20)
cor(cerveza[-(1:4)],cerveza.shift[-c(1:4)])

#Lagplot medicamento
gglagplot(medicamento,lags = 12)
gglagplot(medicamento,lags = 12,do.lines = F)

medicamento.shift<-shift(medicamento,n=12,type="lag")
cbind(medicamento,medicamento.shift)

plot(medicamento~medicamento.shift,xlim=c(1,30),ylim=c(1,30),
     xy.labels=FALSE,col=cycle(medicamento),pch=20)
cor(medicamento[-(1:12)],medicamento.shift[-c(1:12)])

#Funcion de autocorrelacion
acf(cerveza)
ggAcf(cerveza)

acf(medicamento)
acf(medicamento, plot = FALSE)
ggAcf(medicamento)

ggAcf(AP.ts)


acf(AP.ts)
acf(cerveza, plot = FALSE)


#Ejemplo: Muertes por accidente en EU 1973-1978
autoplot(USAccDeaths)
acf(USAccDeaths)
ggAcf(USAccDeaths)

ggseasonplot(USAccDeaths, year.labels=FALSE, continuous=TRUE)
gglagplot(USAccDeaths,lags=16)

#Ejemplo: Series multivariadas
arrivals<-fpp2::arrivals
str(arrivals)

arrivals

autoplot(arrivals)
autoplot(arrivals, facets = TRUE)

#Promedio diario industrial Dow Jone
getSymbols("^DJI",from = "2016/12/31",
           to = "2018/12/31",
           periodicity = "daily")
y <- DJI$DJI.Close
library(xts)
plot(y)

#note el comportamiento en diferentes segmentos de tiempo.
plot(y[1:200])
plot(y[1:100])
