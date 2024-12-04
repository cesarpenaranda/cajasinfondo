# 1 librerías
library(ggplot2)
library(forecast)
library(fpp2)
library(astsa)
library(car)
library(TSA)
library(tseries)
library(urca)
library(car)

#2 Modelo AR(1)
#2.1 Funciones para simular un AR(1)
set.seed(1000)
gen_ar1a <- function(N = 150, phi1 = 0.8, sigma2 = 1) {
  a <- rnorm(N,0,sigma2) 
  y <- as.numeric(0)
  y[1] <- a[1]
  for(i in 2:N){
    y[i] <- phi1*y[i-1]+a[i]
  }
  return(y)
}                     

gen_ar1b <- function(N = 150, C=0, phi1 = 0.8, sigma2 = 1) {
  NN <- 1000
  a <- rnorm(NN+N,0,sigma2) 
  y <- as.numeric(0)
  
  y[1] <- a[1]
  for(i in 2:(NN+N)){
    y[i] <- C + phi1*y[i-1] + a[i]
  }
  return(y[NN:(NN+N)])
}

#2.2 AR(1)

phi1=0.6
y <- gen_ar1b(N=150,C=5,phi1=phi1,sigma2=1)

#2.3 Simulación y el análisis descriptivo
# descriptiva
ts.plot(y)

mean(y)  
#promedio teórico: 
5/(1-phi1)

acf(y,lag.max=30)
pacf(y)

acf2(y)   #library(astsa)

#2.4 Estimación del modelo AR(1)

#p = 1: Número de términos autorregresivos (AR).
#d = 0: Número de diferencias necesarias para hacer la serie estacionaria.
#q = 0: Número de términos de media móvil (MA).

mod0a <- Arima(y, order=c(1,0,0),method="CSS-ML")
mod0b <- Arima(y, order=c(1,0,0),method="ML")
mod0c <- Arima(y, order=c(1,0,0),method="CSS")
#"CSS-ML": Combina estimación de suma de cuadrados condicional (Conditional Sum of Squares)
#y máxima verosimilitud (Maximum Likelihood).
#"ML": Utiliza solo máxima verosimilitud.
#"CSS": Utiliza solo suma de cuadrados condicional.
summary(mod0a)
summary(mod0b)
summary(mod0c)

#No está estimando el intercepto C, sino la media del proceso.\
mean(y)

5/(1-phi1) #media teórica

#2.5 El paquete tseries
#Este paquete permite la estimación del intercepto.
mod0e<-tseries::arma(y,order=c(1,0),include.intercept=TRUE)
summary(mod0e)

#order = c(1,0): Define el orden del modelo ARMA:
#p = 1: Número de términos autorregresivos (AR).
#q = 0: Número de términos de media móvil (MA).
#Esto corresponde a un modelo AR(1).

#2.6 El diagnóstico del modelo AR(1)

#devolvemos al mod0c
res<-mod0c$residuals
ts.plot(res)

acf(res)
pacf(res)

acf2(res)

tsdiag(mod0c)    #library(stats)

checkresiduals(mod0c,lag=10)

#Hipótesis nula (H₀): Los residuos no presentan autocorrelación significativa, es decir,
#los residuos son independientes (no correlacionados) en los lags evaluados.
#Hipótesis alternativa (H₁): Los residuos presentan autocorrelación significativa en al menos un lag.
#Con un nivel de confianza del 95%, no se rechaza la hipótesis nula, por lo que para los primeros 10 lags
#no encontramos correlacion.

checkresiduals(mod0c,lag=30)
#No se rechaza igual


#2.7 Normalidad
shapiro.test(res)
jarque.bera.test(res)
qqPlot(res)

#2.8 Pronóstico
forecast(mod0c)
autoplot(forecast(mod0c))

#2.9 Simulación con arima.sim
#AR(1)
m<-5 #la media del proceso
y1 <- arima.sim(n = 150, model = list(order = c(1,0,0),ar = c(0.8)),sd=3,rand.gen= rnorm) + m
ts.plot(y1)

acf2(y1)

mod1<- forecast::Arima(y1, order = c(1, 0, 0))
summary(mod1)


#3 Modelo AR(2)
y2 <- arima.sim(n = 150, model = list(order = c(2,0,0),ar = c(0.6,-0.28)),sd=1,rand.gen= rnorm) 
ts.plot(y2)

#order = c(2, 0, 0):
#Indica que se trata de un modelo AR(2) (autoregresivo de orden 2), ya que:
#El primer valor (2) especifica el orden AR.
#El segundo valor (0) indica que no hay diferenciación (I = 0).
#El tercer valor (0) indica que no hay componente de media móvil (MA = 0).

ar2.st <- arima(y2, c(2, 0, 0), include.mean=FALSE,
                transform.pars=FALSE, method="ML")
ar2.st$coef

polyroot(c(1, -ar2.st$coef))

Mod(polyroot(c(1, -ar2.st$coef)))

root.comp <- Im(polyroot(c(1, -ar2.st$coef)))
root.real <- Re(polyroot(c(1, -ar2.st$coef)))
# Plotting the roots in a unit circle
x <- seq(-1, 1, length = 1000)
y1 <- sqrt(1- x^2)
y2 <- -sqrt(1- x^2)
plot(c(x, x), c(y1, y2), xlab='Parte Real',
     ylab='Parte Compleja', type='l',
     main='Circulo unitario', ylim=c(-2, 2), xlim=c(-2, 2))
abline(h=0)
abline(v=0)
points(Re(polyroot(c(1, -ar2.st$coef))),
       Im(polyroot(c(1, -ar2.st$coef))), pch=19)
legend(-1.5, -1.5, legend="Raíces del AR(2)", pch=19)

#Otra posibilidad es usar el inverso de las raíces.
autoplot(ar2.st)


#4 Modelo ARMA(1,1)
y3<-arima.sim(n = 150, list(order = c(1,0,1),ar = c(0.88), ma = c(-0.23)),
              sd = sqrt(2))

ts.plot(y3)

acf2(y3)

#ARMA(1,1)
mod3<- forecast::Arima(y3, order = c(1, 0, 1))
summary(mod3)

checkresiduals(mod3,lag=10)

#AR(1)
mod3ar1<- forecast::Arima(y3, order = c(1, 0, 0))
summary(mod3ar1)

checkresiduals(mod3ar1,lag=10)

mod3$aic
mod3ar1$aic

#4.1 Identificación con auto.arima
#procedimiento automático (pero tener mucho cuidado!!!)
auto.arima(y3,ic="aicc") #por defecto

auto.arima(y3,ic="aic")

auto.arima(y3,ic="bic")

#5 Contraste de raíz unitaria

#5.1 Probamos con dos tamaño de series
TT=150
# prueben con TT=500

#5.2 AR(1)
y1 <- arima.sim(n = TT, model = list(order = c(1,0,0),ar = c(0.8)),sd=3,rand.gen= rnorm)
ts.plot(y1)
adf.test(y1) 


#Conclusión: La serie presenta evidencia suficiente de ser estacionaria. 
#Es probable que tenga una raíz unitaria y sea no estacionaria.

#5.3 ARIMA(0,1,0)
y2 <- arima.sim(n = TT, model = list(order = c(0,1,0),sd=1,rand.gen= rnorm))
ts.plot(y2)

acf2(y2)
adf.test(y2)

#6 Ejemplos reales
#6.1 Ejemplo con graduados de ITCR de 1975 a 2002

itcrgrad<-read.csv("SERIES/Datos/ITCR.csv",sep=",") 
y<-ts(itcrgrad$graduados,start=1975)

ts.plot(y) 

acf2(y)
#Indicación de no estacionariedad. Como ejemplo vamos a ajustar un 
#AR(1) ignorando la no estacionariedad. (1 rezago de f.a.c.p. significativo)

try(mod0 <- Arima(y, order=c(1,0,0)))
try(mod0a <- Arima(y, order=c(1,0,0),method="CSS-ML"))

mod0b <- Arima(y, order=c(1,0,0),method="ML")
mod0c <- Arima(y, order=c(1,0,0),method="CSS")
summary(mod0b)
summary(mod0c)

adf.test(y) #no estacionario

dif.y<-diff(y)

ts.plot(dif.y)

acf(dif.y)
pacf(dif.y)
adf.test(dif.y)

mod1 <- Arima(y, order=c(0,1,0))
summary(mod1)

#6.1.1 Diagnóstico
res<-mod1$res
ts.plot(res)
acf(res)
pacf(res)

tsdiag(mod1)

checkresiduals(mod1,lag=10)

#6.1.1.1 Normalidad
shapiro.test(res)
jarque.bera.test(res)
qqPlot(res)

#6.1.1.2 Pronóstico
forecast(mod1)
autoplot(forecast(mod1))

#6.1.1.3 Identificación automática
auto.arima(y,ic="aicc", allowdrift = FALSE) #por defecto

auto.arima(y,ic="aic", allowdrift = FALSE)
auto.arima(y,ic="bic", allowdrift = FALSE)

auto.arima(y,ic="aicc") #por defecto
auto.arima(y,ic="aic")

auto.arima(y,ic="bic")

#6.3 Producto nacional bruto
#Producto nacional bruto, U.S. (en mil millones y son datos trimestrales de 1947 a 2002)
#los datos fueron ajustada estacionalmente. (Ejemplo 3.40, Shumway&Stoffer)

y<-astsa::gnp
ts.plot(y)

acf2(y, 50)

#6.3.1 Contraste de Dickey-Fuller
adf.test(y)

ts.plot(diff(y))

ts.plot(log(y))

dif.log.y = diff(log(y))      # taza de crecimiento
plot(dif.log.y)

acf2(dif.log.y, 24)

adf.test(dif.log.y)

#6.3.2 Identificación de modelos
#AR(1)
moda<-Arima(dif.log.y, order=c(1,0,0))
summary(moda)

autoplot(moda)

#MA(2)
modb<-Arima(dif.log.y, order=c(0,0,2))
summary(modb)

autoplot(modb)

checkresiduals(moda)
checkresiduals(modb)

c(moda$aic,moda$aicc,moda$bic)
c(modb$aic,modb$aicc,modb$bic)

modc<-Arima(log(y),order=c(1,1,0),include.drift=TRUE)
summary(modc)

modd<-Arima(y,order=c(1,1,0),include.drift=TRUE,lambda=0)
summary(modd)

mode<-Arima(y,order=c(0,1,2),include.drift=TRUE,lambda=0)
summary(mode)

checkresiduals(modd,lag=10)
checkresiduals(mode,lag=10)

res<-modd$res

ts.plot(res)
acf(res)
pacf(res)
#Normalidad
shapiro.test(res)
jarque.bera.test(res)
qqPlot(res)
