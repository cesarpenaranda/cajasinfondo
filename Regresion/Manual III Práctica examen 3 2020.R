
#PREGUNTA 1---------------------------------------------------------------------
load("~/cigarros.Rdata")
names(cigarros)
attach(cigarros)
View(cigarros)
#1.	Haga una matriz de gráficos con el gráfico de dispersión de la variable 
#dependiente (cigarros) vs. hatcalor, hatalco y deprescale (3 ptos.)

par(mfrow=c(2,2))
plot(cigarros$cigarros~hatcalor)
abline(lm(cigarros$cigarros~hatcalor))

plot(cigarros$cigarros~hatalco)
abline(lm(cigarros$cigarros~hatalco))

plot(cigarros$cigarros~deprescale)
abline(lm(cigarros$cigarros~deprescale))

#2.	Según los gráficos de dispersión, ¿en cuáles asociaciones puede argumentar 
#linealidad? (2 ptos.)


#3.	Estime una ecuación de regresión en la que prediga el número de cigarros 
#en función de la escala de depresión, las calorías consumidas y la cantidad de 
#alcohol consumido (3 ptos.)

mod1=lm(cigarros$cigarros~deprescale+hatcalor+hatalco)
summary(mod1)

#4.	Interprete los coeficientes de estas tres variables explicativas (3 ptos.)


#5.	Haga un gráfico de residuos estandarizados vs. leverage y,
#si hay valores extremos y/o influyentes, identifique los casos 
#con la variable cigarros (4 ptos.).  
lev=hatvalues(mod1)

lim.lev=2*length(mod1$coef)/length(lev)
estandariz=rstandard(mod1)
plot(estandariz,lev)
abline(h=lim.lev, col=5)
title("Estandarizados vs lev")
identify(cigarros$cigarros)

plot(mod1)
#6.	¿Hay valores extremos en el consumo de cigarros que tengan influencia en 
#los resultados de la regresión?  ¿Diga por qué?  (2 ptos.)



#7.	Analice el supuesto de homoscedasticidad con una prueba de Breusch-Pagan, 
#con un =0.05.  ¿Se puede mantener el supuesto de homoscedasticidad? (3 ptos.)

bptest(mod1)
scatterplot(mod1$fitted,mod1$residuals)

#H0 hay homocedasticidad

"Se rechaza nuestra H0 por lo tanto, no podemos suponer homocedasticidad"

#8.	Analice la normalidad de los residuos con una prueba de Shapiro-Wilks,
#con un =0.05.  ¿Qué concluye? (3 ptos.)

shapiro.test(mod1$residuals)
qqPlot(mod1$residuals)

#H0 hay normalidad 

"Se rechaza nuestra H0 por lo tanto no podemos suponer normaldiad"

#9.	Haga una transformación de BoxCox a la variable dependiente escogiendo la 
#transformación que más se ajuste a los datos (3 ptos.)

boxcox(cigarros$cigarros~deprescale+hatcalor+hatalco)

bcobjeto=boxcox(cigarros$cigarros~deprescale+hatcalor+hatalco)

cbind(bcobjeto$x,bcobjeto$y)

bcobjeto$x[bcobjeto$y==max(bcobjeto$y)]

cigarros_lambda=I((cigarros$cigarros)^0.2)


#10.	Vuelva a estimar el modelo en el que la variable transformada de cigarros
#es predicha por las calorías consumidas, el alcohol consumido y la escala de 
#depresión. Usando como base las pruebas de hipótesis para los coeficientes de 
#regresión, cómo cambia la intepretación de los factores que determinan la cantidad 
#de cigarros al usar la variable transformada en comparación con la variable sin 
#transformar? (3 ptos.)

mod_box=lm(cigarros_lambda~deprescale+hatcalor+hatalco)
summary(mod_box)
"No hubo mayor cambio"

#11.	Haga un gráfico de residuos vs. predichos y argumente si la forma de dicho
#gráfico sugiere o no homoscedasticidad (3 ptos.)

scatterplot(mod_box$fitted.values,mod_box$residuals)
ncvTest(mod_box)

"Se puede observar homocedasticidad"

#12.	Estime una prueba de Breusch-Pagan para el modelo con la variable 
#transformada, con un =0.05 y concluya apropiadamente (3 ptos.).  
bptest(mod_box)

"segun la prueba de breushpagan no se puede suponer homocedasticidad,
pero en este caso en el grafico se puede ver que si la hay, por esto preferimos las 
concluciones por medio del grafico"

#13.	Conteste: Si la transformación de Box-Cox fue creada para corregir el
#supuesto de normalidad de los residuos, cómo una transformación de BoxCox 
#puede afectar los resultados de una prueba de homoscedasticidad 
#(ya sea White y Breusch-Pagan)?  Nota: No estoy buscando que digan 
#que la prueba de Breusch-Pagan supone normalidad y la de White es más
#robusta ante tal violación. (3 ptos)






