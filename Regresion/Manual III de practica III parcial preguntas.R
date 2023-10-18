library(car)
library(lmtest)
library(tseries)
library(e1071)
library(dglm)
library(MASS)
###
scar=function(x){
  scatterplot(x$fitted,x$residuals)
}
###
load("bridge.Rdata")
attach(bridge)
View(bridge)
names(bridge)
str(bridge)

#PREGUNTA 1---------------------------------------------------------------------

#a) Estime un modelo de regresión gaussiano (el tradicional de mínimos cuadrados ordinarios) 
#en el que se prediga el tiempo para realizar las obras en función del resto de variables 
#(excepto case por ser el identificador). Llámelo mod1. Presente el summary del modelo

mod1=lm(time~darea+ccost+dwgs+length+spans)
summary(mod1)
plot(mod1)

#b) Analice la normalidad de los residuos con una prueba de Jarque Bera.
#Plantee la hipótesis nula y alternativa, y responda adecuadamente. Conteste:
#¿Se puede suponer normalidad de los errores? ¿Por qué sí o por qué no?

#H0= los errores se distribuye normal
jarque.bera.test(mod1$residuals)
qqPlot(mod1$residuals)
#p-value = 0.0001239

"Por el pvalue < alfa, se rechaza H0, por lo tanto no podemos suponer normalidad
ademas de que los analicis graficos nos indican tambien que no podemos suponerla"

#c) Utilice el método de BoxCox para encontrar una transformación para la variable 
#dependiente. Pegue el gráfico en el examen. Escoja un exponente redondeado que sea
#razonable de acuerdo al gráfico de BoxCox

boxcox(time~darea+ccost+dwgs+length+spans)

bcobjeto=boxcox(time~darea+ccost+dwgs+length+spans)

cbind(bcobjeto$x,bcobjeto$y)

bcobjeto$x[bcobjeto$y==max(bcobjeto$y)]

time.lambda1=I(time^(0.05))

#time.lambda=I(log(time))
"es mejor transformar con log, porque esta cerca del cero en el grafico"

mod1.box=lm(time.lambda1~darea+ccost+dwgs+length+spans)
summary(mod1.box)

#d) Estime un nuevo modelo gaussiano con la variable dependiente transformada,
#llame al modelo mod2, y verifique con la prueba de Jarque Bera si ya se puede 
#suponer normalidad. No es necesario que plantee las hipótesis nula y alternativa. 
#Solo señale la conclusión.
mod2=mod1.box

jarque.bera.test(mod2$residuals)
qqPlot(mod2$residuals)

"Si podemos suponer normalidad"

#e) Utilice el método de Bootstrap para estimar intervalos no paramétricos para 
#ambos modelos (mod1 y mod2), y compárelos con los intervalos paramétricos teóricos 
#de ambos modelos (mod1 y mod2). A partir de la comparación con los intervalos de bootstrap, 
#argumente si hay evidencia de que la transformación mejora las estimaciones del modelo.


library(boot)

coefic=function(y,x,d) {
  lm(y[d]~x[d,])$coef
}

coef=boot(time,coefic,R=1000,x=cbind(darea,ccost,dwgs,length,spans))

boot.ci(coef,index=1, type="perc")#B0
boot.ci(coef,index=2, type="perc")#B1
boot.ci(coef,index=3, type="perc")#B2
boot.ci(coef,index=4, type="perc")#B4
boot.ci(coef,index=5, type="perc")#B5
boot.ci(coef,index=6, type="perc")#B6



round(rbind(boot.ci(coef,index=1, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=2, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=3, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=4, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=5, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=6, type="perc")$percent[,c(4,5)]),4)

round(confint(mod1),4)
"la falta de normalidad me amplia los intervalos de la formula"
#mod2 
coef=boot(time.lambda1,coefic,R=1000,x=cbind(darea,ccost,dwgs,length,spans))

boot.ci(coef,index=1, type="perc")#B0
boot.ci(coef,index=2, type="perc")#B1
boot.ci(coef,index=3, type="perc")#B2
boot.ci(coef,index=4, type="perc")#B4
boot.ci(coef,index=5, type="perc")#B5
boot.ci(coef,index=6, type="perc")#B6



round(rbind(boot.ci(coef,index=1, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=2, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=3, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=4, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=5, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=6, type="perc")$percent[,c(4,5)]),4)

round(confint(mod2),4)

"al observar los intervalos, hay evidencia de que la transformacion mejora el supuesto de normalidad
y debido a esto mejora el modelo"

#f) Estime un modelo heteroscedástico (modelo lineal generalizado doble dglm) con 
#la especificación del modelo1 y llámelo mod1h. A partir de las estimaciones de
#este modelo, justifique si hay presencia de heteroscedasticidad o no, y cómo lo sabe.

mod1h=dglm(time~darea+ccost+dwgs+length+spans, 
           dformula~darea+ccost+dwgs+length+spans,
           family=gaussian)
summary(mod1h)
"si almenos uno es menor a 0.05 entonces se puede argumentar heterocedasticidad"
#dwgs e spans me predicen la heterocedasticidad
plot(mod1h)
scar(mod1h)

"hay precencia de heterocedasticidad, al observar el grafico de predichos contra residuos
por su forma de megafono"


#g) Estime la prueba de variancia no constante (ncvTest) para el modelo 1 y el 
#modelo 2. Plantee una sola vez la hipótesis nula y alternativa. Diga si hay 
#alguna diferencia en el supuesto de homoscedasticidad entre el modelo 1 y el
#modelo 2, y cómo una transformación de Box Cox (que es para corregir violaciones 
#a la normalidad) puede generar similitudes o diferencias en los resultados de estas pruebas

ncvTest(mod1)
ncvTest(mod2)

#H0=varianzas iguales

"Si hay diferencias a simple vista, en el modelo1 no podemos suponer homocedasticidad,
mientras que en el modelo dos si podemos suponerla"

#PREGUNTA 10--------------------------------------------------------------------

#a) Estime un modelo de regresión gaussiano en el que se prediga el gasto en 
#alimentos en función del ingreso total del hogar, la cantidad de miembros del
#hogar y la edad del jefe. Muestre el summary del modelo.


load("enignorte.Rdata")
names(enignorte)
View(enignorte)
attach(enignorte)


mod1=lm(gastoalim~ingresototal+miembroshogar+edadjefe)
summary(mod1)
crPlots(mod1)

#b) Haga un gráfico de residuos contra predichos y diga cómo evalúa el supuesto
#de homoscedasticidad.

scatterplot(mod1$fitted,mod1$residuals)

"No podemos suponer homocedasticidad"


#c) Estime un modelo de mínimos cuadrados ponderados con dos iteraciones y en 
#que los residuos absolutos sean predichos por los predictores para calcular los 
#ponderadores. Muestre el summary del modelo final.


abs.res1=abs(residuals(mod1))
mod.ponde1=lm(abs.res1~ingresototal+miembroshogar+edadjefe)

ponde1=1/abs(fitted(mod.ponde1))

#Modelo podenrado

mod2=lm(gastoalim~ingresototal+miembroshogar+edadjefe, weights=I(ponde1))

round(cbind(mod1$coef,mod2$coef),4)

ncvTest(mod1)
ncvTest(mod2)

#Interaccion 2
abs.res2=abs(residuals(mod2))
mod.ponde2=lm(abs.res2~ingresototal+miembroshogar+edadjefe)

ponde2=1/abs(fitted(mod.ponde2))

#Modelo ponderado 2

mod3=lm(gastoalim~ingresototal+miembroshogar+edadjefe, weights=I(ponde2))
ncvTest(mod3)
scatterplot(mod3$fitted,mod3$residuals)

summary(mod3)

#d) Estime un modelo hesteroscedástico (un modelo lineal generalizado gaussiano 
#doble) con las variables predictoras en las ecuaciones de la media y la variancia,
#y a un 5% de significancia diga si hay variables asociadas con heteroscedasticidad.


modh=dglm(gastoalim~ingresototal+miembroshogar+edadjefe,
          dformula~ingresototal+miembroshogar+edadjefe, family=gaussian)
summary(modh)


sum0=summary(modh)
round(sum0$coefficients,2)

"Al 5% de significancia las variables asociadas a la heterocedasticidad que encontramos son
miembros del hogar ingreso total"

#e) Haga un cuadro en el que compare los coeficientes y los p-values de los 3 
#modelos (guassiano en inciso (a), mínimos cuadrados ponderados en inciso 
#(c) y heteroscedástico en inciso (d)), y diga en qué se parecen y se diferencian
#las conclusiones de los tres modelos (use un alfa de 5%).




#PREGUNTA 10--------------------------------------------------------------------
load("sarscovtri.Rdata")
attach(sarscovtri)
names(sarscovtri)
View(sarscovtri)


#a) Estime un modelo lineal gaussiano (el modelo de regresión tradicional) y
#analice el supuesto de normalidad con un qqPlot de los residuos y una prueba de
#Shapiro (use un alfa de 5%). Diga qué concluye sobre el supuesto de normalidad

mod1=lm(carga.viral10~edad+mujer)
summary(mod1)

hist(mod1$residuals)
qqPlot(mod1$residuals)

shapiro.test(mod1$residuals)

#b) Haga gráficos crPlots con la ecuación y diga si hay alguna transformación
#que usted sugeriría para las variables predictoras. Explique su elección 

crPlots(mod1)

#c) Estime un nuevo modelo lineal gaussiano con la transformación sugerida. 
#Compare la bondad de ajuste de este modelo con el modelo del inciso (a), y diga 
#si la transformación mejoró en algo la bondad de ajuste del modelo. 
#[Nota: Para obtener todos los puntos de la pregunta, tiene que enseñarme un nuevo 
#modelo con un crecimiento sustancial en el indicador de bondad de ajuste]

#modelo boxcox

box=boxcox(carga.viral10~edad+mujer)

cbind(box$x,box$y)

box$x[box$y==max(box$y)]

carga_lambda=log(carga.viral10)

mod_box=lm(carga_lambda~edad+mujer)
summary(mod_box)

#modelo heterocidastico

mod_h=dglm(carga.viral10~edad+mujer, 
               dformula~edad+mujer, family=gaussian)
summary(mod_h)
plot(mod_h)

#modelo huber
mod_huber=rlm(carga.viral10~edad+mujer)

summary(mod_huber, cor=FALSE)

plot(mod_huber)
crPlots(mod_huber)

#bicuadratico 
mod_bisq=rlm(carga.viral10~edad+mujer, method='MM')

summary(mod_bisq, cor=FALSE)

crPlots(mod_bisq)

#transformacion de la dependiente 
edad_2=(edad)^0.5

mod2=lm(carga.viral10~edad_2+mujer)
summary(mod2)
crPlots(mod2)

AIC(mod1)
AIC(mod2)
AIC(mod_bisq)
AIC(mod_box)
AIC(mod_h)
AIC(mod_huber)

"la tranformacion escogida es la del  modelo box cox ya que su AIC es el mas pequeño 
de los modelos estimados"

#d) Haga un gráfico qqPlot de los residuos y una prueba de Shapiro
#(al 5% de significancia) con el nuevo modelo, y diga qué concluye sobre la 
#violación al supuesto de normalidad.


shapiro.test(mod_box$residuals)
qqPlot(mod_box)

"Al 5% de significancia, podemos concluir que hay normalidad en el modelo
ademas podemos observarla por medio de analisis grafico"

#e) A partir del análisis anterior, explique cómo se relacionan los supuestos 
#de linealidad y normalidad.
