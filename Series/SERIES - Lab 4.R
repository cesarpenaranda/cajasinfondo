library(ggplot2)
library(forecast)
library(fpp2)
library(astsa)
library(car)
library(gamair)
library(mgcv)
library(dplyr)
library(GGally)

#Modelo de Regresión

#Cambios porcentuales trimestrales (tasas de crecimiento) del gasto de consumo 
#personal real (Y) e ingresos disponibles(X), para EE.UU. desde 1970 a 2016.

data(uschange)

autoplot(uschange[,1:5], facets = TRUE, colour=TRUE) +
  ylab("") + xlab("Year") +
  guides(colour="none")

autoplot(uschange)

uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()

uschange %>% as.data.frame() %>% cor()

mod=lm(Consumption~Income+Production+Savings+Unemployment,data = uschange)
summary(mod)

plot(mod$fitted.values,type = "l");points(mod$model$Consumption,type = "l",col="red")

mod0 <- tslm(
  Consumption ~ Income + Production + Unemployment + Savings,
  data=uschange)
summary(mod0)

plot(mod0$fitted.values,type = "l")

autoplot(uschange[,'Consumption'], series="Data") +
  forecast::autolayer(fitted(mod0), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Cambio porcentual de gastos de consumo en EE.UUU ") +
  guides(colour=guide_legend(title=" "))

checkresiduals(mod0)

#Modelos de tendencia
#Ejemplo con graduados de ITCR de 1975 a 2002

itcrgrad<-read.csv("SERIES - Tema_3/ITCR.csv",sep=",")
y<-ts(itcrgrad$graduados,start=1975)
autoplot(y) 

#Crear variables independientes
tiempo=seq_along(y)
tiempo2=tiempo**2

datos.itcrgrad<-data.frame(y,tiempo,tiempo2)

mod1 <- lm(y~tiempo+tiempo2,data = datos.itcrgrad)
summary(mod1)

mod.ts1<-tslm(y~trend+trend^2) #note la salida no incluye a la tendencia al cuadrado.
summary(mod.ts1)

mod.ts2<-tslm(y~trend+I(trend^2)) #este da solo trend y trend al cuadrado 
summary(mod.ts2)

par(mfrow = c(1,1))
e<-mod1$residuals

#Normalidad
hist(e)
qqPlot(e)
shapiro.test(e)

#Homoscedasticidad
ts.plot(e)
lmtest::bptest(mod1)

#Autocorrelación
acf(mod1$residuals)
checkresiduals(mod1)

#Regresión con series estacionales
turistas<-read.csv("SERIES - Tema_3/turistas.csv",sep=";")
y<-ts(turistas$turistas,start=c(1991,1),frequency=12)
autoplot(y)

#transformacion logarítmica
w<-log(y)
autoplot(y) 

autoplot(w)

tiempo<-seq(1,length(y))
tiempo2<-tiempo^2
mes<-rep(seq(1,12),10)
mes<-as.factor(mes)

datos1<-data.frame(w,tiempo,tiempo2,mes)
mod1<-lm(w~tiempo+tiempo2+mes,datos1)
summary(mod1)

levels(datos1$mes)

datos2 <- within(datos1, mes <- relevel(mes, ref = 12))
levels(datos2$mes)

mod2 <- lm(w~tiempo+tiempo2+mes,datos2)
summary(mod2)

#Pronóstico
mod1<-lm(w~tiempo+tiempo2+mes,datos1)
summary(mod1)

try(
  pronostico<-forecast(mod1) #error
)

mod3<-tslm(w~trend+I(trend^2)+season)
summary(mod3)

pronostico<-forecast(mod3,h=12)
pronostico


autoplot(w) +
  ylab("ln Y") +
  autolayer(mod3$fitted.values, series = "ajustado") +
  autolayer(pronostico, series = "pronostico")

checkresiduals(mod3)


y<-ts(turistas$turistas,start=c(1991,1),frequency=12)
y.train<-window(y,start=c(1991,1),end=c(1999,12))
y.test<-window(y,start=c(2000,1),end=c(2000,12))
mod4<-tslm(y.train~trend+I(trend^2)+season)
summary(mod4)

pronostico<-forecast(mod4,h=12)
pronostico

accuracy(pronostico)

accuracy(pronostico,y.test)

#Algunos temas adicionales

data("cairo")
ctamm <- gamm(temp~s(day.of.year,bs="cc",k=20)+s(time,bs="cr"),
              data=cairo,correlation=corAR1(form=~1|year))

pred<-fitted(ctamm$gam)
plot(cairo$temp~cairo$time,type="l", ylab="temp",xlab="time")
points(cairo$time,pred,type="l",col=2,lwd=2)

#Análisis espectral
x<-cairo$temp
x.spec <-mvspec(x,log="no")

plot(x.spec)
frecuencia<-x.spec$freq[x.spec$spec==max(x.spec$spec)]
abline(v=frecuencia,col=2)


1/frecuencia

cos1<-cos(2*pi*frecuencia*cairo$time)
sin1<-sin(2*pi*frecuencia*cairo$time)
data=data.frame(x=x,time=cairo$time,cos=cos1,sen=sin1)
mod<-lm(x~cos1+sin1,data=data)
plot(cairo$temp~cairo$time,type="l", ylab="temp",xlab="time")
points(cairo$time,fitted(mod),type="l",col=2,lwd=2)


dim(x.spec$details)

head(x.spec$details)

tail(x.spec$details)


spectro_orden <- x.spec$details %>% as.data.frame() %>% arrange(desc(spectrum))
spectro_orden[1:8,]

frecuencia1 <- spectro_orden$frequency[1] 
frecuencia2 <- spectro_orden$frequency[2]
frecuencia3 <- spectro_orden$frequency[3]
frecuencia4 <- spectro_orden$frequency[4]
frecuencia5 <- spectro_orden$frequency[5]

cos1<-cos(2*pi*frecuencia1*cairo$time)
sin1<-sin(2*pi*frecuencia1*cairo$time)
cos2<-cos(2*pi*frecuencia2*cairo$time)
sin2<-sin(2*pi*frecuencia2*cairo$time)
cos3<-cos(2*pi*frecuencia3*cairo$time)
sin3<-sin(2*pi*frecuencia3*cairo$time)
cos4<-cos(2*pi*frecuencia4*cairo$time)
sin4<-sin(2*pi*frecuencia4*cairo$time)
cos5<-cos(2*pi*frecuencia5*cairo$time)
sin5<-sin(2*pi*frecuencia5*cairo$time)

data=data.frame(x=x,
                cos1=cos1,sen1=sin1,
                cos2=cos2,sen2=sin2,
                cos3=cos3,sen3=sin3,
                cos4=cos4,sen4=sin4,
                cos5=cos5,sen5=sin5)

mod2<-lm(x~.,data=data)
plot(cairo$temp~cairo$time,type="l", ylab="temp",xlab="time")
points(cairo$time,fitted(mod2),type="l",col=2,lwd=2)            
