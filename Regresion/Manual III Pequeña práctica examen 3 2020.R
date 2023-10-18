
#PREGUNTA2----------------------------------------------------------------------
load("~/aves.Rdata")
attach(aves)
names(aves)
#a)	Estime un modelo de regresión gaussiano (el tradicional de mínimos cuadrados ordinarios)
#en el que se prediga la cantidad de pájaros en función del tamaño (altura) y ancho del árbol,
#y la estación en la que se hizo la observación (3 ptos.)

mod1=lm(pajaros~tamanyo+ancho+estacion)

#b)	Analice la normalidad de los residuos con un gráfico.  Conteste: 
#¿Se puede suponer normalidad? (3 ptos.)
qqPlot(mod1$residuals)
hist(mod1$residuals)
shapiro.test(mod1$residuals)

"Apartir del grafico y otros analisis no podemos suponer normaldiad"
#c)	Utilice el procedimiento de BoxCox para encontrar alguna transformación 
#de la variable dependiente, y vuelva a estimar los residuos del nuevo modelo.
#¿Se puede suponer normalidad en esta nueva ecuación?  ¿Por qué sí o por qué no? (4 ptos).

box=boxcox(pajaros~tamanyo+ancho+estacion)

cbind(box$x,box$y)

box$x[box$y==max(box$y)]

pajaros_lambda=log(pajaros)

mod_box=lm(pajaros_lambda~tamanyo+ancho+estacion)
summary(mod_box)

qqPlot(mod_box$residuals)
hist(mod_box$residuals)
shapiro.test(mod_box$residuals)

"Aunque tenemos una mejora, aun no podemos suponer normalidad, ya que asi nos lo 
muestra el analisis grafico"

#d)	Haga un gráfico de residuos contra predichos y diga si hay valores extremos
#en esta ecuación (diciendo por qué sí o por qué no hay valores extremos) (3 ptos.)

plot(mod_box$fitted.values,mod_box$residuals)
plot(mod_box)

"Podemos observar algunos valores extremos en el grafico, lo son porque estan muy dispersos"
#e)	Con los resultados del modelo gaussiano del punto a) calcule los intervalos
#de confianza al 95% para cada una de las pendientes.  Escriba los intervalos de
#confianza en la tabla que aparece más abajo (3 ptos)  

confint(mod1)

"               2.5 %    97.5 %
(Intercept) -0.9592340 4.8468382
tamanyo      0.1254523 0.6264827
ancho       -2.1416260 0.2832046
estacion    -0.4756413 2.3962542
"

#f)	Utilice un procedimiento de bootstrap para calcular intervalos de confianza
#no paramétricos para el modelo equivalente al del punto a), y escríbalos en la
#tabla que aparece más abajo. (3 ptos.)

library(boot)

coefic=function(y,x,d) {
  lm(y[d]~x[d,])$coef
}

coef=boot(pajaros,coefic,R=1000,x=cbind(tamanyo,ancho,estacion))

boot.ci(coef,index=1, type="perc")#B0
boot.ci(coef,index=2, type="perc")#B1
boot.ci(coef,index=3, type="perc")#B2
boot.ci(coef,index=4, type="perc")#B3


round(rbind(boot.ci(coef,index=1, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=2, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=3, type="perc")$percent[,c(4,5)],
            boot.ci(coef,index=4, type="perc")$percent[,c(4,5)]),4)

round(confint(mod1),4)

"
[1,] -0.2762  4.0384
[2,]  0.1882  0.5904
[3,] -2.1941 -0.0714
[4,] -0.3862  2.5300

"
#g)	Cambian las conclusiones que se pueden hacer entre el modelo gaussiano y 
#el modelo con bootstrap en términos de las variables que predicen el número de 
#pájaros observados.  ¿Por qué sí o por qué no? (3 ptos). 

"Si cambian porque los intervalos de confianza con boostrap y por medio de las formulas
son distintos, ademas de algunas variaciones de signo, esto nos indica que no podemos confiar
en esas concluciones"


