library(ggplot2)
library(forecast)
library(fpp2)

#Descomposición aditiva
contrayentes<-read.csv("SERIES - Tema_2/contrayentes.csv",sep=";")
y <- contrayentes$serie

contrayentes.ts <- ts(y,start =c(1978,1),frequency = 12)
autoplot(contrayentes.ts)

#Decomposicion
aditivo <- decompose(contrayentes.ts,type = "additive")
autoplot(aditivo)

ggsubseriesplot(contrayentes.ts)
ggseasonplot(contrayentes.ts,year.labels = F,continuous = T)
ggseasonplot(contrayentes.ts,labels = F,continuous = T,polar =T)

names(aditivo)

aditivo$x #serie original
aditivo$seasonal #índices estacionales normalizados 
aditivo$trend #tendencia
plot(aditivo$trend)
plot(aditivo$seasonal)

autoplot(aditivo)

tend <- aditivo$trend
tend.estac <- aditivo$trend+aditivo$seasonal
serie.aj.estac<-aditivo$x-aditivo$seasonal

autoplot(cbind(aditivo$x,tend,tend.estac,serie.aj.estac), size = 1)+
  xlab("tiempo")+ylab("contrayentes")+
  ggtitle("Serie contrayentes")+
  scale_color_manual(labels = c("serie original","tend", "tendencia+estac","serie.aj.estac"), 

                                          values = 1:4)

#Descomposición multiplicativa
turistas<-read.csv("SERIES - Tema_2/turistas.csv",sep=";")
y <- turistas$turistas

turistas.ts <- ts(y,start = c(1991,1),frequency=12)
autoplot(turistas.ts)

multiplicativo <- decompose(turistas.ts,type = "multiplicative")
autoplot(multi,range.bars = TRUE)

#Grafico
tend <- multiplicativo$trend
tend.estac <- multiplicativo$trend * multiplicativo$seasonal
serie.aj.estac<-multiplicativo$x/multiplicativo$seasonal

autoplot(cbind(multiplicativo$x,tend,tend.estac,serie.aj.estac), size = 1)+
  xlab("tiempo")+ylab("contrayentes")+
  ggtitle("Serie contrayentes")+
  scale_color_manual(labels = c("serie original","tend", "tendencia+estac","serie.aj.estac"), 
                     values = 1:4)

#Descomposición STL
y.stl1<-stl(turistas.ts,t.window=20, s.window="periodic", robust=TRUE)
plot(y.stl1)

y.stl2<-stl(turistas.ts,t.window=5, s.window="periodic", robust=TRUE)
plot(y.stl2)

y.stl3<-stl(turistas.ts,t.window=5, s.window=3, robust=TRUE)
plot(y.stl3)


#Pronóstico con STL
y.train<-window(turistas.ts,start=c(1991,1),end=c(1999,12))
y.test<-window(turistas.ts,start=c(2000,1),end=c(2000,12))
y.stl3<-stl(y.train,t.window=20, s.window="periodic", robust=TRUE)

#forecast.stl
class(y.stl3)

#Pronostico
pronostico<-forecast(y.stl3,h=12)
#default: suavizamiento exponencial

names(pronostico)
plot(pronostico)
points(y.test,type="l",col=2)

accuracy(pronostico)
summary(pronostico)
accuracy(pronostico,y.test)

fitted(pronostico) #Valores predichos

#entrenamiento
e.train<-y.train-fitted(pronostico)
(MAE.train<-sum(abs(e.train))/length(e.train))
(RMSE<-sqrt(sum((e.train)^2)/length(e.train)))

#prueba
n<-12
e<- y.test - pronostico$mean #Prediccion/pronostico menos el test
(MAE<-sum(abs(e))/n)
sqrt(MSE<-sum(e^2)/n)
(MAPE<-sum(abs(100*e/y.test))/n)

#Pronóstico con tendencia
contrayentes<-read.csv("SERIES - Tema_2/contrayentes.csv",sep=";")
y<-ts(contrayentes$serie,start=c(1978,1),frequency=12)
aditivo<-decompose(y,type="additive")

plot(aditivo)
autoplot(aditivo)

tend <- aditivo$trend
tend.estac <- aditivo$trend+aditivo$seasonal
serie.aj.estac<-aditivo$x-aditivo$seasonal

autoplot(cbind(aditivo$x,tend,tend.estac,serie.aj.estac), size = 1.2)+
  xlab("tiempo")+ylab("contrayentes")+
  ggtitle("Serie contrayentes")+
  scale_color_manual(labels = c("serie original","tend", "tendencia+estac","serie.aj.estac"), 
                     values = 1:4)

#Regresion simple
data<-data.frame(y,tend,tend.estac,serie.aj.estac,t=seq_along(y))
head(data)
mod<-lm(serie.aj.estac~t,data=data)
summary(mod)
fitted(mod)
plot(contrayentes$serie,type = "l")
points(fitted(mod),type = "l",col="red")

plot(as.numeric(data$y)~data$t,type="l")
abline(mod,col=2)

pred<-predict(mod,newdata = data.frame(t=73:83))
accuracy(mod)        

plot(contrayentes$serie,type = "l")
points(c(rep(NA,73),pred),type = "l",col="red")





