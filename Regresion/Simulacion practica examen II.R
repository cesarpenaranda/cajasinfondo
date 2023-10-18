#1.Cree una variable x con 50 números aleatorios una distribución uniforme con parámetros
#mínimo=1 y máximo=10.  Además, cree  dos variables que les llamará error1 y error2 de la
#siguiente forma:

#a.error1=rt (50, df=3)  (O sea, números aleatorios de una distribución t de Student con v=3)
#b.error2=rnorm(50)  (O sea, números aleatorios de una distribución normal estándar)	 (2 ptos.).
  



x1=runif(n=50,min = 1,max=10)
error1=rt(n=50,df=3)
error2=rnorm(50)

library(lmtest)
library(car)
library(tseries)

#3.	Las variables anteriores equivalen a los residuos de los modelos de regresión.
#Pruebe la hipótesis nula de que error1 y error2 se distribuyen normalmente con media=0
#y desviación estándar=1, con:
#a.	Prueba de Shapiro
#b.	Prueba de Kolmogorov-Smirnov
#c.	Prueba de Jarque Bera
#d.	No escriba si se rechaza H0 o no en cada una, sino que en 5 líneas describa las diferencias generales entre las tres pruebas para las dos variables (4 ptos).  

shapiro.test(error1)
shapiro.test(error2)

jarque.bera.test(error1)
jarque.bera.test(error2)

ks.test(error1,"pnorm",mean=0,sd=1)
ks.test(error2,"pnorm",mean=0,sd=1)

par(mfrow=c(2,1))

hist(error1)
hist(error2)

########
#4.	Ahora estime dos variables y1 y y2 que sean iguales a:
#a.	y1=10+3*x+error1
#b.	y2=10+3*x+error2



y1=10+3*x1+error1 
y2=10+3*x1+error2

#5.	Estime dos modelos de regresión, y1 en función de x, y y2 en función de x.  
#Para cada uno de ellos, calcule el intervalo de confianza para la pendiente 
#(el coeficiente beta1) (5 ptos.)

tt=qt(0.975,50-2)

modt=lm(y1~x1)
modz=lm(y2~x1)

c(summary(modt)$coef[2,1]-tt*summary(modt)$coef[2,2],summary(modt)$coef[2,1]+tt*summary(modt)$coef[2,2])
c(summary(modz)$coef[2,1]-tt*summary(modz)$coef[2,2],summary(modz)$coef[2,1]+tt*summary(modz)$coef[2,2])

confint(modt)[2,]
confint(modz)[2,]


######

#6.	(Repasar de Métodos). Con bootstrap y 1000 réplicas, calcule intervalos de
#confianza no paramétricos (los percentiles) para beta1 del modelo 1 y para beta1
#del modelo 2 (5 ptos.)
#library(boot)

coefic=function(y,x,d){
  lm(y[d]~x[d])$coef[2]
}

coef.t=boot(y1,coefic,R=1000,x=x1)
coef.z=boot(y2,coefic,R=1000,x=x1)


boot.ci(coef.t,type="perc")
boot.ci(coef.z,type="perc")



#######
arreglo=rep(0,1000)
alm.pvalue=rep(NA,1000)

for (i in 1:1000){
  YY1=rnorm(5,0,1)
  YY2=rnorm(5,0,100)
  YY=c(YY1,YY2)
  XX=c(rep(0,5),rep(1,5))
  mod=lm(YY~XX)
  sum=summary(mod)
  sum$fstatistic
  pv=1-pf(q=sum$fstatistic[1],df1=sum$fstatistic[2],df2=sum$fstatistic[3])
  arreglo[i]=1*(pv<0.10)
  alm.pvalue[i]=pv
  
}

mean(arreglo)
hist(alm.pvalue)


