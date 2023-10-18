rm(list=ls(all=T))

load("pantorrilla1.Rdata")

###En la investigación nutricional de adultos mayores se está utilizando la circunferencia 
###de la pantorrilla como indicador de problemas de peso.  Se le da el archivo 
###pantorrilla1.Rdata que contiene las variables que se describen abajo.  
###Se desea analizar si la altura de la rodilla, la estatura, el peso en libras, 
##los días para hacer ejercicios, sexo, condición de malaria y edad predicen 
###la circunferencia de la pantorrilla para validar su uso como indicador nutricional.

###Sin embargo, como la ecuación se quiere usar para predecir la circunferencia de pantorrilla,
###se busca un modelo más pequeño que sea más fácil de programar.

#Modelo parcimonioso es el que tiene menos variables pero aun asi explica un gran porcentaje de variabilidad

attach(pantorrilla1)
names(pantorrilla1)

library(car)
library(olsrr)
library(qpcR)

###Explorando las asociaciones lineales con un grafico###

par(mfrow=c(2,4))

plot(rodilla, pantorrilla)
plot(estatura, pantorrilla)
plot(pesolibras, pantorrilla)
plot(diasej1, pantorrilla)
plot(edad, pantorrilla)
plot(malaria, pantorrilla)
plot(hombre, pantorrilla)


par(mfrow=c(1,2))
plot(pantorrilla~as.factor(malaria))
plot(pantorrilla~as.factor(hombre))

###Primero se estima un modelo grande y un modelo nulo

mod.nulo=lm(pantorrilla~1) #Modelo sin variables predictoras 

modgrande=lm(pantorrilla~rodilla+estatura+pesolibras+diasej1+edad+malaria+hombre)

summary(modgrande)


####Se experimenta primero con un modelo stepwise basado en p-values.

###Procedimiento forward
###Se incorporan las variables cuyo coeficiente tenga un pvalue menor a 0.05

ols_step_forward_p(modgrande, penter=0.05, progress=TRUE)

###Procedimiento backward
###Se excluyen las variables cuyo coeficiente tenga un pvalue mayor a 0.05
#Son dos algoritmos distintos pero llegan a la misma conclucion 

ols_step_backward_p(modgrande, prem=0.05, progress=TRUE)

###Procedimiento stepwise forward backward
###Combinación de ambos.

ols_step_both_p(modgrande, penter=0.05, prem=0.05, progress=TRUE)

mod1.both=ols_step_both_p(modgrande, penter=0.05, prem=0.05, progress=TRUE)
summary(mod1.both$model)




####Ahora procedimiento usando AIC
###Se prefiere el modelo con el AIC o el BIC más bajo


###Procedimiento forward
#Solo con forward utilizamos el modelo nulo 
step(mod.nulo, scope=~rodilla+estatura+pesolibras+diasej1+edad+malaria+hombre, direction="forward")

#Call:
#lm(formula = pantorrilla ~ pesolibras + estatura + rodilla + hombre)
#Esto es lo ultimo en salir de la funcion y es lo que la misma aconseja 



###Procedimiento backward

step(modgrande, direction="backward")

###Procedimiento stepwise forward/backward
step(modgrande, direction="both")
modelos.step= step(modgrande)
summary(modelos.step)

summary(mod1.both$model)


#buscar que con step no sea aic sino bic, el creterio utilizado 
library(leaps)
library(faraway)


###Un procedimiento en que se escoge el modelo con el mejor BIC
###Para cada subconjunto de variables desde 1 hasta p-1
###Se puede observar en un gráfico

solovariables=as.data.frame(pantorrilla1[,c(2,3,4,5,6,7,8,9)])
names(pantorrilla1)
conjuntos=regsubsets(pantorrilla~rodilla+estatura+pesolibras+diasej1+edad+malaria+hombre, data=solovariables,nbest=1)

subsets(conjuntos)


###Cálculo a pie del BIC y el AIC

mod.pequenyo=lm(pantorrilla~rodilla+estatura+pesolibras)

logLik(mod.pequenyo)
logLik(modgrande)

AIC(mod.pequenyo)
BIC(mod.pequenyo)
mod.pequenyo
-2*logLik(mod.pequenyo)+2*(length(mod.pequenyo$coef)+1)
-2*logLik(mod.pequenyo)+(length(mod.pequenyo$coef)+1)*log(length(mod.pequenyo$fitted))

AIC_f = function (x){
  result=AIC(x)
  print(paste('Valor de la funcion del AIC: ', round(result,4)))
  -2*logLik(mod.pequenyo)+2*(length(mod.pequenyo$coef)+1)
}
AIC_f(mod.pequenyo)


BIC_f = function (x){
  result=BIC(x)
  print(paste('Valor de la funcion del BIC: ', round(result,4)))
  -2*logLik(x)+(length(x$coef)+1)*log(length(x$fitted))
}
BIC_f(mod.pequenyo)

###PRESS y Pcuadrado


###Primero con la funci?n de R

(press.grande=ols_press(modgrande))
#vemos que el press del modelo grande es mas grande que el del pequeno
(press.pequenyo=ols_press(mod.pequenyo))

(ag=anova(modgrande))  ##Suma de Cuadrados Total
(ap=anova(mod.pequenyo))  ##Suma de Cuadrados Total

(psq.grande=1-press.grande/sum(ag[,2]))  ##F?rmula de c?lculo a mano
(psq.pequenyo=1-press.pequenyo/sum(ap[,2]))
#Esto me indica que deberia elegir el modelo pequeno 


#funcion
PREES_f= function(x){
  press=ols_press(x)
  print(paste('El press del modelo es',round(press,4)))
  SCT=anova(x)
  result=(1-press/sum(SCT[,2]))
  print(paste('EL p-cuadrado es: ', round(result,4)))
}


PREES_f(modgrande)
PREES_f(mod.pequenyo)

PRESS(modgrande, verbose=FALSE)$stat  #Son subobjetos de un objeto PRESS
PRESS(modgrande, verbose=FALSE)$P.square

PRESS(mod.pequenyo, verbose=FALSE)$stat
PRESS(mod.pequenyo, verbose=FALSE)$P.square



###Calculo a mano del PRESS
###En lugar de hacer un ciclo, se usa la equivalencia a partir de los leverage

(press.grande.apie=sum((modgrande$residuals/(1-hatvalues(modgrande)))^2))

(press.peqenyo.apie=sum((mod.pequenyo$residuals/(1-hatvalues(mod.pequenyo)))^2))

PRESS2_f = function(x){
  result=sum((x$residuals/(1-hatvalues(x)))^2)
  print(paste('EL resultado del Press: ',round(result,4)))
}

PRESS2_f(mod.pequenyo)
PRESS2_f(modgrande)


##Juntando los indicadores del modelo pequeño y del modelo grande

tabla=matrix(rep(NA,10),ncol=2,nrow=5)

tabla[1,1]=summary(modgrande)$adj.r.squared
tabla[2,1]=psq.grande
tabla[3,1]=press.grande
tabla[4,1]=AIC(modgrande)
tabla[5,1]=BIC(modgrande)

tabla[1,2]=summary(mod.pequenyo)$adj.r.squared
tabla[2,2]=psq.pequenyo
tabla[3,2]=press.pequenyo
tabla[4,2]=AIC(mod.pequenyo)
tabla[5,2]=BIC(mod.pequenyo)

rownames(tabla)=c("R2 ajust","P2","PRESS","AIC","BIC")
colnames(tabla)=c("Grande","Pequeño")

round(tabla,4)


#funcion
TABLA_f= function(x,y){
  
tabla=matrix(rep(NA,10),ncol=2,nrow=5)
  
tabla[1,1]=summary(x)$adj.r.squared
tabla[2,1]=PRESS(x, verbose=FALSE)$P.square
tabla[3,1]=PRESS(x, verbose=FALSE)$stat
tabla[4,1]=AIC(x)
tabla[5,1]=BIC(x)
  
tabla[1,2]=summary(y)$adj.r.squared
tabla[2,2]=PRESS(y, verbose=FALSE)$P.square
tabla[3,2]=PRESS(y, verbose=FALSE)$stat
tabla[4,2]=AIC(y)
tabla[5,2]=BIC(y)
  
rownames(tabla)=c("R2 ajust","P2","PRESS","AIC","BIC")
colnames(tabla)=c("Grande","Pequeño")
  
round(tabla,4)  
}
TABLA_f(modgrande,mod.pequenyo)



modmashombre=lm(pantorrilla~rodilla+estatura+pesolibras+hombre)



####Práctica para los estudiantes:
###Tome las variables del modelo pequeño: rodilla, estatura y pesolibras
###Estime todos los modelos de regresión lineal simple con esas variables
###Y todos los modelos con 2 variables (rodilla y estatura, rodilla y pesolibras, 
###estatura y pesolibras).  Para todos estos 6 modelos, calcule:
###el R cuadrado ajustado, el P-cuadrado, el PRESS, el AIC y el BIC.
###Haga un cuadro comparando dichos indicadores y diga si coinciden en el 
###modelo escogido.  
###Además, haga un gráfico de dispersión en el que compare el R2 ajustado con
###el P-cuadrado, y el PRESS con el AIC.

#Modelos simples 
mod_simple_1=lm(pantorrilla~rodilla)
mod_simple_2=lm(pantorrilla~rodilla)
mod_simple_3=lm(pantorrilla~rodilla)


#Modelos con dos variables
mod_bi_1=lm(pantorrilla~rodilla+estatura)
mod_bi_2=lm(pantorrilla~rodilla+pesolibras)
mod_bi_3=lm(pantorrilla~estatura+pesolibras)

estats= function(x){
tabla=matrix(rep(NA,5),ncol=1,nrow = 5)
tabla[1,1]=summary(x)$adj.r.squared
tabla[2,1]=PRESS(x, verbose=FALSE)$P.square
tabla[3,1]=PRESS(x, verbose=FALSE)$stat
tabla[4,1]=AIC(x)
tabla[5,1]=BIC(x)
rownames(tabla)=c("R2 ajust","P2","PRESS","AIC","BIC")
round(tabla,4) 
}
estats(mod.pequenyo)

###Qué concluye?





####Modelo con interacciones


mod.int=lm(pantorrilla~rodilla*hombre+estatura*hombre+pesolibras*hombre)

mod.int.red=step(mod.int)

summary(mod.int)
summary(mod.int.red)


