
#-------------------------------------------------------------------------------Ejercico 36
psi=c(1.1,
      1.3,
      1.1,
      1.3,
      1.2,
      1.2,
      1.2,
      1.2,
      1.3)

pureza= c(
  83.0,
  85.7,
  84.0,
  86.0,
  84.0,
  83.5,
  83.0,
  84.0,
  86.3)

modelo.36=lm(pureza~psi)
summary(modelo.36)
#-------------------------------------------------------------------------------Ejercicio 38



library(car)
vida= c(8,7,7,8,9,10,14,12,12,13,14,14,11,16,16)
humedad= c(95,90,85,80,75,70,65,60,55,50,45,40,35,30,25)

lm.viva.humedad=lm(formula=vida~humedad)
summary(lm.viva.humedad)

qqPlot(lm.viva.humedad$residuals, main= "analisis de normalidad de los residuos",
       xlab="Cuantilos normalizados",
       ylab="Residuos",
       pch=17)
shapiro.test(lm.viva.humedad$residuals)
#no se rechaza 

#-------------------------------------------------------------------------------Ejercicio 39



#a)	Haga gráficos de dispersión entre cada variable independiente y el consumo de electricidad.
attach(electricidad)
names(electricidad)

par(mfrow=c(1,3))

plot(gastokw~alquilado)
plot(gastokw~miembroshogar)
plot(gastokw~metroscuad)

#El mejor predictor es el grafico tres
#Para crear un grafico de cajas solo cambiamos a as factor la segunda variable

plot(gastokw~as.factor(alquilado))
#No podemos hacer un boxplot para las otras dos variables porque no son variables cualitativas

#b)	Estime un modelo de regresión con la variable dependiente y las independientes antes señaladas.
#Escriba la ecuación.

mod.39 <- lm(gastokw~alquilado+miembroshogar+metroscuad)
summary(mod.39)
#ecuacion: 90.11x+5051.80

#c)	Sin mirar los p-values de los coeficientes,
#interprete los 4 coeficientes de regresión que tiene en la ecuación.

#1- El promedio de gasto de los alquilados es 4694.88 menos que los no alquilados,
#manteniendo constantes las variables

#2- Por cada miembro del hogar adicional, el gasto va a tener un aumento de 303.21 kilowatts, 
#manteniendo constantes las variables

#3- Por cada metro cuadrado, aumenta el consumo en 90.11 kilovatios, manteniendo costantes las variables

#d)	Plantee las hipótesis nulas y alternativas tanto de los coeficientes como 
#la hipótesis nula global (la prueba F) implícitas en la salida de regresión.

##H0: Beta0=0; H1:Beta0<>0
##H0: Beta1=0; H1:Beta1<>0
##H0: Beta2=0; H1:Beta2<>0
##H0: Beta3=0; H1:Beta3<>0

##H0: Beta1=Beta2=Beta3=0; H1: Al menos un Betai<>0

#e)	Interprete los resultados de cada una de las pruebas de hipótesis usando como indicador los p-values.

#Rechazo H0: beta0=0
#Rechazo H0: beta1=0
#No Rechazo H0: beta2=0
#Rechazo H0: beta3=0


#Rechazo H0: beta0=beta1=beta2=beta3=0

#f)	Interprete el coeficiente de determinación.
#El modelo explica el 64.54% de la variabilidad del gasto en electricidad

#g)	Interprete el gráfico de residuos contra los predichos (puede suponer homoscedasticidad).

par(mfrow=c(1,1))
plot(mod.39$fitted.values,mod.39$residuals)
plot(mod.39$residuals~mod.39$fitted.values)

#h)	Realice la prueba de Shapiro para los residuos.  Se rechaza o no se rechaza H0, con un alfa=0.05

#H0: errores se distribuyen normalmente
#H1: errores no se distribuyen normalmente 

shapiro.test(mod.39$residuals)

library(car)
qqPlot(mod.39$residuals)

#R/ No hay suficiente evidencia estadistica para rechazar la hipotesis nula de que los residuos se distribuyen
#normalmente

#-------------------------------------------------------------------------------Ejercicio 42

library(car)

sexo <- c(rep(1,12),rep(0,12))
felicidad <- c(9,4.5,6.3,5.8,8.3,3.1,
               2.5,9.1,7.4,3.3,4.1,2.1,
               6.6,2.7,5.2,8,3.6,5.9,
               2.4,6,2,3.6,3,5.20)

length(sexo)
length(felicidad)

#modelo de regresion 
mod.ej42=lm(felicidad~as.factor(sexo))
summary(mod.ej42)

#Evaluar el supuesto de homoscedasticidad
par(mfrow=c(1,2))
plot(mod.ej42$residuals~mod.ej42$fitted.values)
boxplot(felicidad~as.factor(sexo),names=c("Mujer","Hombre"))

#levene test para probar homocedasticidad 
leveneTest(felicidad~as.factor(sexo))
#solo funciona cuando la variable independiente es cualitativa

#H0;var1=Var0
#H1;var1<>var2


#Evaluar normalidad condicional 

par(mflow=c(1,1))
qqPlot(mod.ej42$residuals)


#shapiro
shapiro.test(mod.ej42$residuals)

#si suponemos normalidad preferimos la prueba parametrica
#Prueba parametrica 

t.test(felicidad[sexo==0],felicidad[sexo==1],paired=F,var.equal = T)
#no parametrica 
wilcox.test(felicidad[sexo==0],felicidad[sexo==1],paired=F)

#para analizar homocedasticidad 
par(mfrow=c(1,2))
plot(mod.ej42$residuals ~mod.ej42$fitted.values)
boxplot(felicidad~sexo)


(data.ej42=data.frame(felicidad,sexo))

cuadrado.medio= function(datos,d){
  objeto.lm=lm(datos[d,]$felicidad~datos[d,]$sexo)
  objeto.anova= anova(objeto.lm)
  cme=objeto.anova[2,3]
  print(cme)
  
}

cuadrado.medio(data.ej42)

library(boot)

boot(data.ej42,cuadrado.medio,R=999)

#-------------------------------------------------------------------------------Ejercicio 46
#H0: Mu1=Mu2

#creamos los vectores
maq1=c(8.26,8.13,8.385,8.07,8.34)
maq2=c(7.95,7.89,7.9,8.14,7.92,7.84)

#Creamos el arreglo y los graficos de normalidad
maq.total=c(maq1,maq2)
tipo=as.factor(c(rep(1,5),rep(2,6)))
par(mfrow=c(1,2))
qqPlot(maq1)
qqPlot(maq2)

#Probamos la normalidad
shapiro.test(maq1)
shapiro.test(maq2)

#H0: Mu1=Mu2
#H1: Mu1<>Mu2
#Hacemos la prueba de wilcoxon
#a)
wilcox.test(
  maq1,
  maq2,
  paired=F
)


#H0: Var1=Var2
#H1: Var1<> Var2
#Hacemos la prueba leveneTest
#b)
leveneTest(maq.total,tipo)

#Hacemos la prueba t 
#c)
t.test(maq1,maq2,paired=F,var.equal=T)

#-------------------------------------------------------------------------------Ejercicio 40
aditivo.factor=as.factor(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4)))
rendimiento=c(17,14,13,12,14,14,13,10,12,13,12,9,13,11,11,12,11,12,10,8)
tapply(rendimiento,aditivo.factor,mean)
mean(rendimiento)
sd(rendimiento)

#Análisis exploratorio de variabilidad mediante grafico de cajas

etiquetas=c("A1","A2","A3","A4","A5") #Nombre de las etiquetas

boxplot(
  rendimiento~aditivo.factor, 
  names=etiquetas
)

#Pruebas de homogeneidad de varianzas

#H0: Sigma1=sigma2=sigma3=sigma4=sigma5 
#H1: Al menos un sigma_i<>sigma_j

library(car)

leveneTest(rendimiento,aditivo.factor)



#anova 
#H0: Mu1=Mu2=Mu3=Mu4=Mu5
#H1: Al menos un MU_i <> Mu_j

anova.1=lm(rendimiento~aditivo.factor)
anova.1
#Análisis de Variancia
summary(anova.1)
anova(anova.1)
 #supuesto de normalidad de los residuos 
qqPlot(anova.1$residuals)
shapiro.test(anova.1$residuals)


#grafico para analizar homoestasticidad con los residuos
plot(anova.1$fitted.values,anova.1$residuals)

#aov para hacer el tukey
anova.2=
  aov(rendimiento~aditivo.factor)
anova(anova.2)

#H0: Mu1=Mu2, Mu1=Mu3, Mu1=Mu4, Mu1=Mu5, Mu2=Mu3, Mu2=Mu4, Mu2=Mu5, Mu3=Mu4, Mu3=Mu5,Mu4=Mu5

TukeyHSD(anova.2)
#Oneway.test cuando se viola el supuesto de homoscedasticidad


#-------------------------------------------------------------------------------Ejercicio 41
planta.factor=as.factor(c(rep(1,9),rep(2,9),rep(3,9)))
produc=c(10,12,15,18,9,17,15,12,18,15,17,18,12,13,11,12,11,12,12,17,15,15,18,12,13,14,14)
tapply(produc,planta.factor,mean)
df=data.frame(produc,planta.factor)
df

etiquetas=c("Alajuela","Heredia","San jose") #Nombre de las etiquetas

boxplot(
  produc~planta.factor, 
  names=etiquetas
)

#anova 
#H0: Mu1=Mu2=Mu3
#H1: Al menos un MU_i <> Mu_j

anova.prod=lm(produc~planta.factor)
anova.prod
anova(anova.prod)

#supuesto de normalidad de los residuos 
qqPlot(anova.prod$residuals)
shapiro.test(anova.prod$residuals)

#H0: Sigma1=sigma2=sigma3
#H1: Al menos un sigma_i<>sigma_j

leveneTest(produc,planta.factor)


#-------------------------------------------------------------------------------Ejercicio 43 
#H0: Mu1=Mu2

#creamos los vectores
maq1=c(8.26,8.13,8.385,8.07,8.34)
maq2=c(7.95,7.89,7.9,8.14,7.92,7.84)

maq.total=c(maq1,maq2)
tipo=as.factor(c(rep(0,5),rep(1,6)))
etiquetas=c("maquina 1","maquina 2") #Nombre de las etiquetas

boxplot(
  maq.total~tipo, 
  names=etiquetas
)

leveneTest(maq.total,tipo)

anova.maq=lm(maq.total~tipo)
summary(anova.maq)

anova(anova.maq)


qqPlot(anova.maq$residuals)
shapiro.test(anova.maq$residuals)

#grafico para analizar homoestasticidad con los residuos
plot(anova.maq$fitted.values,anova.maq$residuals)
