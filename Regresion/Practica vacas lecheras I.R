#================================================================================

library(haven)
vacaslecherasA <- read_dta("vacaslecherasA.dta")
View(vacaslecherasA)
vacas=vacaslecherasA
attach(vacas)
#1.	Con gráficos explore la linealidad entre la variable dependiente y cada una de las independientes.
#Escriba en menos de 5 líneas con cuáles variables se ve más clara la asociación lineal.
#Justifique (4 ptos.)'

library(PerformanceAnalytics)
'Para observar mas claramente la linialidad'
chart.Correlation(vacas[,-1])

library(car)
par(mfrow=c(1,3))
plot(litrosxdia~nut1)
plot(litrosxdia~nut2)
plot(litrosxdia~nut3)

#2.	Estime un modelo de regresión en el que los litros de leche por día estén en función de los 
#nutrientes, y a partir de este modelo, conteste las siguientes preguntas (Si estima el modelo 
#incorrectamente, pero las preguntas las contesta correctamente con base en el modelo malo, se le 
#asignará parte de los puntos, pero no todos):'

#a)	Independientemente de la probabilidad asociada, interprete el coeficiente de la variable 
#nut1 y el intercepto (4 ptos.).

mod1=lm(litrosxdia~nut1+nut2+nut3)
summary(mod1)

'La produccion promedio de vacas lechera es de 9.12 litros
por dia si la cantidad de nut1,nut2,nut3 es igual a 0'

'Por cada aumento de un gramo por centimetro cubico en el nut1 la 
produccion lechera va a aumentar 0.14 litros por dia en promedio, 
manteniendo constante las demas variables'

#b)	Calcule los coeficientes de regresión estandarizados usando el procedimiento de las 
#matrices de correlación y diga cuál variable está más fuertemente asociada con los litros 
#diarios de leche (4 ptos.)
cor(vacas)
(rxx=cor(cbind(nut1,nut2,nut3)))
#(rxx=cor(vacas)[2:4,2:4]) #Otra manera de hacerlo

(ryx=cor(vacas)[2:4,5])

(coef.estand=solve(rxx)%*%ryx)

#c)	Contraste la hipótesis nula de que los coeficientes estandarizados para nut2 y nut3 son 
#iguales, con un alfa=0.05.  Escriba las hipótesis nula y alternativa y conteste 
#apropiadamente al resultado de la prueba de hipótesis (5 ptos.)

'H0:beta2.st=beta3.st'
'H1:beta2.st<>beta3.st'

mod2=lm(scale(litrosxdia)~scale(nut1)+scale(nut2)+scale(nut3))
summary(mod2)

mod2.igual=lm(scale(litrosxdia)~scale(nut1)+I(scale(nut2)+scale(nut3)))
anova(mod2.igual,mod2) 


'Por lo tanto rechazo la hipotesis nula'

#d)	En términos prácticos relacionados con el aumento de la producción lechera, qué 
#significa el resultado de la pregunta anterior? (2 ptos.)

'Es mejor daler nut3 que nut2 porque el coeficiente es mayor'

#e)	Interprete el coeficiente de determinación múltiple de la “salida” del lm (2 ptos.)

'El modelo explica el 81.29% de la variabilidad de la produccion lechera'

#f)	Calcule matriz de variancia-covariancia de los betas y explique, con base en los 
#resultados del procedimiento, por qué las covariancias entre Bi y Bj son 0 (exceptuando
#B0) (3 ptos.) 

cme= anova(mod1)[4,3]
(mat.var.cor=cme*summary(mod1)$cov.unscaled)
(vcov(mod1))
round((vcov(mod1)),4)

'El hacer que las relaciones entre las predictoras sea cero, hace que las covarianzas sea cero, debido a 
que estamos en un experimento'


#3.	Se le da abajo el programa que escribió el profesor Ricardo Alvarado que contiene la función para 
#simular datos de un modelo de regresión.  Dicha función se usó posteriormente para representar 
#gráficamente el error estándar.  Simule 1000 veces el generar el par de variables (X y Y) con los 
#valores de los parámetros dados en el programa (a=2; b=3; sig=5; n=10).  Usando esa función, 
#estime el valor ajustado o predicho cuando X=5.5 (la media teórica de la variable X) para cada 
#una de las 1000 simulaciones y asígnelo a un vector.  Haga lo mismo para el valor ajustado 
#cuando X=10 (el valor teórico máximo de la variable X).  Haga un histograma de cada uno de los 
#dos vectores y compárelos.  Diga qué diferencias hay en la variabilidad de los histogramas y a 
#qué se debe dicha variabilidad (4 ptos.).

'El siguiente es el codigo'

sim.beta=function(a,b,sig,n,min=1,max=10){
  x=runif(n,min,max)
  y=a+b*x+rnorm(n,0,sig)  
  return(list("x"=x,"y"=y))
}

a=2; b=3; sig=5; n=10

s1=sim.beta(a,b,sig,n)
s1

summary(lm(s1$y~s1$x))
anova(lm(s1$y~s1$x))


'Desarrollo'

sim.beta=function(a,b,sig,n,min=1,max=10){
  x=runif(n,min,max)
  y=a+b*x+rnorm(n,0,sig)  
  return(list("x"=x,"y"=y))
}

a=2; b=3; sig=5; n=10

almacen.5.5=rep(NA,1000)
almacen.10=rep(NA,1000)

for(i in 1:1000){
  
  s1=sim.beta(a,b,sig,n)
  #summary
  mod1=(lm(y~x,data=s1))
  almacen.5.5[i]= predict(mod1,data.frame(x=5.5))
  almacen.10[i]= predict(mod1,data.frame(x=10))
  
}
#anova(lm(s1$y~s1$x))
par(mfrow=c(1,2))
hist(almacen.5.5,xlim=c(0,60))
hist(almacen.10,xlim=c(0,60))
