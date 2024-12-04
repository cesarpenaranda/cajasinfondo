#1 librerías
library(ggplot2)
library(forecast)
library(fpp2)

#2 Ejemplo de ruido blanco
set.seed(1000)
w = rnorm(500,0,1) 
ts.plot(w)

acf(w,lag.max = 50,main="estimación de acf de w")

#3 Ejemplo de medias móviles
w = rnorm(500,0,1) 
v = stats::filter(w, sides=2, filter=rep(1/3,3)) # moving average
v = na.omit(v)
ts.plot(v)

acf(v,lag.max = 50,main="estimación de acf de v")

w = rnorm(500,0,1) 
v = filter(w, sides=2, filter=rep(1/7,7)) # moving average
v = na.omit(v)
ts.plot(v)
acf(v,lag.max = 50,main="estimación de acf de v")

#4 Ejemplo de pasajeros de aerolínea
data(AirPassengers)
plot.ts(AirPassengers)
AP.v1 = stats::filter(AirPassengers, sides=2, filter=rep(1/3,3)) # moving average
AP.v2 = stats::filter(AirPassengers, sides=2, filter=rep(1/6,6)) # moving average
AP.v3 = stats::filter(AirPassengers, sides=2, filter=rep(1/12,12)) # moving average
points(AP.v1,type="l",col=2)
points(AP.v2,type="l",col=3)
points(AP.v3,type="l",col=4)
legend("topleft",legend=c("MA-3","MA-6","MA-12"),
       col=c(2,3,4),lty=1)

#5 Señal+ruido
cs = 2*cos(2*pi*1:500/50 + .6*pi); w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))

#6 Ejemplo de sorteo navideño
sorteo<-read.csv("SERIES/Datos/sorteo.csv",sep=",")
y<-ts(sorteo$numero)
autoplot(y)
ggAcf(y,lag.max = 50,main="estimación de acf")

#7 Ejemplo de graduados de ITCR
itcrgrad<-read.csv("SERIES/Datos/ITCR.csv",sep=",")
y<-ts(itcrgrad$graduados,start=1975)
autoplot(y) 

ggAcf(y)

w=diff(y)
autoplot(w) 

ggAcf(w)

#8 Ejemplo de turistas
turistas<-read.csv("SERIES/Datos/turistas.csv",sep=";")
y<-ts(turistas$turistas,start=c(1991,1),frequency=12)
autoplot(y) 

ggAcf(y)
ggAcf(y, lag.max = 50)


w=diff(y)
autoplot(w) 

ggAcf(w, lag.max = 50)


autoplot(log(y)) 

ggAcf(log(y), lag.max = 50)

logw=diff(log(y))
autoplot(logw) 

ggAcf(logw,lag.max=50)
