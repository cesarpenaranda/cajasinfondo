#Con el fin de estudiar la participación en la fuerza laboral (variable dependiente) 
#de las familias urbanas pobres, se obtuvo una muestra de segmentos censales de un 
#censo de población.  Los datos se dan a continuación

Area= c(137,139,141,142,143,145,147,149,151,153,155,157,159,161,163)
fuerza.laboral= c(64.3,45.4,26.6,87.5,71.3,82.4,26.3,61.6,52.9,64.7,64.9,70.5,87.2,81.2,67.9)
ingreso.medio= c(1998,1114,1942,1998,2026,1853,1666,1434,1513,2008,1704,1525,1842,1735,1639)
tamano.medio= c(2.95,3.4,3.72,4.43,3.82,3.9,3.32,3.8,3.49,3.85,4.69,3.89,3.53,4.96,3.68)
tasa.desempleo= c(4.4,3.4,1.1,3.1,7.7,5,6.2,5.4,12.2,4.8,2.9,4.8,3.9,7.2,3.6)

#Notas: 
#1. Área es el identificador del segmento censal
#2. El ingreso familiar medio está medido en dólares corrientes.
#3. Tasa de desempleo está medida en puntos porcentuales.

#Si el objetivo es plantear un modelo en el que se trate de predecir el porcentaje
#de participación en el mercado laboral, conteste las siguientes preguntas:

#1.	Usando las operaciones matriciales, encuentre los estimadores mínimo-cuadráticos
#para los coeficientes de regresión.

x=cbind(1,ingreso.medio,tamano.medio,tasa.desempleo)
x
# Construir XTX.
xtx = t(x) %*% x 
xtx
# Invertir la matriz.
(xtxi = solve(xtx))

#verificar relacion
xtx %*% xtxi
round(xtx %*% xtxi,5)

# Estimar los coeficientes.
coef = xtxi %*% t(x) %*% fuerza.laboral
coef

#R/
#[,1]
#               -33.45985205
#ingreso         0.01937303
#tamafamilia    15.52203573
#tasadesempleo   0.81276960

#2.	Utilizando el comando de lm, estime el modelo de regresión.

mod1=lm(fuerza.laboral~ingreso.medio+tamano.medio+tasa.desempleo)
summary(mod1)

#R/
#Residual standard error: 18.38 on 11 degrees of freedom
#Multiple R-squared:  0.2755,	Adjusted R-squared:  0.07786 
#F-statistic: 1.394 on 3 and 11 DF,  p-value: 0.2963


#3.	Interprete todos los coeficientes (beta0, beta1, beta2 y beta3)
"B0 En promedio si una familia tiene un ingreso medio de cero colones,  tamaño medio de cero y una tasa
de desempleo de cero, la fuerza laboral porcentual promedio de dicha familia seria de -33.45985 "

'B1 Por cada aumento en un dolar corriente en el ingreso familiar, la fuerza laboral porcentual promedio   
va a aumentar 0,019 puntos porcentualesmanteniendo costante las demas
variables(tamaño medio, tasa desempleo)' 

'B2 por cada aumento en de un miembro en el tamaño de la familia, la fuerza laboral porcentual promedio 
va a aunmentar 15.52 puntos porcentuales manteniendo costante las demas 
variables(ingreso medio, tasa desempleo)'

'B3 por cada aumento de un punto en la tasa de desempleo la fuerza laboral procentual promedio
va a aumentar 0.8127 puntos porcentuales manteniendo costante las demas 
variables(ingreso medio,tamaño medio)'

#4.	Interprete el coeficiente de determinación múltiple de la “salida” del lm
'el modelo explica el 27% de la variabilidad del porcentaje en fuerza laboral'

#5.	Estime una tabla de Análisis de Variancia con el modelo, y calcule el coeficiente
#de determinación múltiple a partir de los resultados de dicha tabla.

anova(mod1)
SCE=anova(mod1)[4,2]
SCETOTAL=sum(anova(mod1)[,2])
1-SCE/SCETOTAL
summary(mod1)$r.sq

#R/
#[1] 0.2754624

#6.	Calcule los coeficientes de determinación parcial de cada variable condicional 
#a que las otras dos variables ya fueron incorporadas.

#Para ingreso
mod.multi.ingreso=lm(fuerza.laboral~tamano.medio+ingreso.medio+tasa.desempleo)
mod.parci.ingreso=lm(fuerza.laboral~tamano.medio+tasa.desempleo)
anova(mod.multi.ingreso)
anova(mod.parci.ingreso)
'Forma 1' 
resta.residuos=anova(mod.parci.ingreso)[3,2]-anova(mod.multi.ingreso)[4,2]
resta.residuos

det.parcial=resta.residuos/anova(mod.parci.ingreso)[3,2]
det.parcial
'Forma 2' 
det.parcial2=1-anova(mod.multi.ingreso)[4,2]/anova(mod.parci.ingreso)[3,2]
det.parcial2
#[1] 0.08466914

#Para tamaño medio 
mod.multi.tam.medio=lm(fuerza.laboral~tamano.medio+ingreso.medio+tasa.desempleo)
mod.parci.tam.medio=lm(fuerza.laboral~ingreso.medio+tasa.desempleo)
anova(mod.multi.tam.medio)
anova(mod.parci.tam.medio)
'Forma 1'
resta.residuos=anova(mod.parci.tam.medio)[3,2]-anova(mod.multi.tam.medio)[4,2]
resta.residuos

det.parcial=resta.residuos/anova(mod.parci.tam.medio)[3,2]
det.parcial
'Forma 2'
det.parcial2=1-anova(mod.multi.tam.medio)[4,2]/anova(mod.parci.tam.medio)[3,2]
det.parcial2
#[1] 0.1964617

#Para tasa desempleo

mod.multi.tasa=lm(fuerza.laboral~tamano.medio+ingreso.medio+tasa.desempleo)
mod.parci.tasa=lm(fuerza.laboral~tamano.medio+ingreso.medio)
anova(mod.multi.tasa)
anova(mod.parci.tasa)
'Forma 1'
resta.residuos3=anova(mod.parci.tasa)[3,2]-anova(mod.multi.tasa)[4,2]
resta.residuos3

det.parcial3=resta.residuos3/anova(mod.parci.tasa)[3,2]
det.parcial3
'Forma 2'
det.parcial3=1-anova(mod.multi.tasa)[4,2]/anova(mod.parci.tasa)[3,2]
det.parcial3
#[1] 0.01618177


#7.	Con operaciones matriciales, calcule la matriz de variancia-covariancia de los
#coeficientes de regresión.
anova(mod1)
CME=anova(mod1)[4,3]
var=CME *solve(t(x)%*%x)
var
#sqrt(diag(var))
#summary(mod1)$coef[,2]

#R/
#(Intercept)  ingreso.medio   tamano.medio tasa.desempleo 
#48.78314471     0.01920559     9.46491316     1.91080120 

#8.	Calcule el valor ajustado para el área No. 163.
XD=data.frame(x)
XD
predict(mod1,data.frame(ingreso.medio=1639,tamano.medio=3.68,tasa.desempleo=3.6))
'Comparacion'
fuerza.laboral[1]

#R/58.3396

#9.	Usando las funciones ya programadas de R, calcule un intervalo de confianza al 95% 
#para el porcentaje de participación laboral promedio de todas aquellas áreas que compartan
#las características del área 163.
predict(mod1,data.frame(ingreso.medio=1639,tamano.medio=3.68,tasa.desempleo=3.6),interval="confidence")

#R/
#fit      lwr     upr
#1 58.3396 45.01451 71.6647

#10.	Usando las funciones ya programadas de R, calcule el intervalo de confianza al 95% 
#para el porcentaje de participación predicho para el área 163.

predict(mod1,data.frame(ingreso.medio=1639,tamano.medio=3.68,tasa.desempleo=3.6),interval="prediction")

#R/
#fit      lwr      upr
#1 58.3396 15.74273 100.9365

#11.	Usando operaciones matriciales, calcule el intervalo de confianza al 95% para
#el porcentaje solicitado en el punto anterior.

'Vectores de trabajo'
x0=c(1,1639,3.68,3.6)
y0=predict(mod1,data.frame(ingreso.medio=1639,tamano.medio=3.68,tasa.desempleo=3.6))
CMEE = anova(mod1)[4,3]
vcov= vcov(mod1)
vcov
var0= t(x0) %*% vcov %*% x0
var0
ee=sqrt(var0)
n=nrow(x)
p=ncol(x)
t=qt(.975,n-p)  
ic0=c(y0-t*ee,y0+t*ee)
ic0
'Comparacion'
predict(mod1,data.frame(ingreso.medio=1639,tamano.medio=3.68,tasa.desempleo=3.6),interval="confidence")

#R/
#45.01451 71.66470

#12.	Reproduzca con funciones de R el valor t y el p-value para el contraste de hipótesis
#de que el coeficiente del tamaño familiar medio (Beta2) sea igual a 0.
#H0: Beta2=0
#H1: Beta2<>0
summary(mod1)
anova(mod1,mod.parci.tam.medio)

'Con el p-value correspondiente de 0.129, no hay suficiente evidencia estadistica para
rechazar la hipotesis nula de que beta2=0, con una significancia de 5%.

H0: Omega.minuscula: Modelo reducido
H1: Omega.mayuscula: Modelo mas amplio'

#R/
#2     11 3717.0  1    908.79 2.6895 0.1293
'El resultado es el mismo comparando el modelo simple con el modelo complejo'

#13.	Haga la prueba de hipótesis de que los coeficientes de regresión del tamaño familiar
#medio y de la tasa de desempleo (beta2 y beta3) son iguales a cero simultáneamente.
'H0: Beta2 = Beta3 = 0
 H1: Beta2<>0 o Beta3<>0'

summary(mod1)
mod.simple1=lm(fuerza.laboral~ingreso.medio)
anova(mod.simple1,mod1)

'Con el p-value correspondiente de 0.287, no hay suficiente evidencia estadistica para
rechazar la hipotesis nula de que beta2 = beta 3 = 0, con una significancia de 5%.'

#R/
#2     11 3717.0  2    946.97 1.4012  0.287

#14.	Calcule los terciles a la variable ingreso familiar medio.  Con base en dichos 
#terciles, cree una variable categórica que sea igual a 1 si el ingreso es menor al 
#primer tercil, que sea igual a 2 si el ingreso está entre el primer y segundo tercil,
#y que sea igual a 3 si el ingreso es mayor al segundo tercil.  Convierta esta variable
#a tipo factor y llámele tercilesingreso.  Para efectos de interpretación, llame a las categorías

#1: Ingreso muy bajo, 
#2: Ingreso bajo, 
#3: Ingreso moderado.

tercil=quantile(ingreso.medio,probs=c(0.33,0.66))
tercil

#R/
#33%     66% 
#1655.74 1874.36

x=x %>% as_tibble()
x=x %>% mutate("tercilesingreso" = case_when(ingreso.medio < 1657.000  ~ 1,
                                                       ingreso.medio >  1882.667 ~ 3,
                                                       ingreso.medio < 1882.667  ~ 2))
x$tercilesingreso = x$tercilesingreso %>% as.factor()
x



#15.	Estime con el comando lm un modelo de regresión en el que las variables independientes 
#sean el tamaño familiar medio, la tasa de desempleo y la variable tercilesingreso
#(pista: automáticamente R generará variables dummy).

mod.dummy=lm(fuerza.laboral~tamano.medio+tasa.desempleo+tercilesingreso)
summary(mod.dummy)

#R/
#Residual standard error: 20.1 on 10 degrees of freedom
#Multiple R-squared:  0.2121,	Adjusted R-squared:  -0.103 
#F-statistic: 0.6732 on 4 and 10 DF,  p-value: 0.6255


#16.	Interprete los coeficientes de regresión para las variables dummy generadas automáticamente
#al estimar un modelo con tercilesingreso.

'Para valores fijos de tamano familiar medio y tasa de desempleo, el Porcentaje de Fuerza 
laboral promedio de la familia es 2.3612% mayor para familias en el segundo tercil de ingreso 
en comparacion a las familias en el primer tercil de ingreso, sin importar cuales son los 
valores de Tamano Familiar Medio y Tasa de Desempleo'

'Para valores fijos de tamano familiar medio y tasa de desempleo, el porcentaje de fuerza
laboral promedio de la familia es 2.65% mayor para familias en el tercer tercil de ingreso
n comparacion a las familias en el primer tercil de ingreso, sin importar cuales son los
valores de tamano familiar medio y tasa de desempleo.'

#17.	Trate de averiguar cómo puede en R cambiar la categoría de referencia y estime el mismo 
#modelo del punto anterior, pero poniendo de categoría de referencia a la categoría 2 (ingreso bajo). 
#Interprete los coeficientes para las dumies generadas con tercilesingreso.

terciles.nuevos = tercilesingreso
contrasts(terciles.nuevos)=contr.treatment(levels(terciles.nuevos),base=2)

modelo.nueva.categoria = lm(fuerza.laboral~tamano.medio+tasa.desempleo+terciles.nuevos)
summary(modelo.nueva.categoria)

#R/
#Residual standard error: 20.1 on 10 degrees of freedom
#Multiple R-squared:  0.2121,	Adjusted R-squared:  -0.103 
#F-statistic: 0.6732 on 4 and 10 DF,  p-value: 0.6255

'terciles.nuevos1: 
Para valores fijos de tamano familiar medio y tasa de desempleo, el porcentaje de fuerza
laboral promedio de la familia es 2.36% menor para familias en el primer tercil de ingreso
en comparacion a las familias en el segundo tercil de ingreso, sin importar cuales son los
valores de tamano familiar medio y tasa de desempleo.'

'terciles.nuevos3:
Para valores fijos de tamano familiar medio y tasa de desempleo, el porcentaje de fuerza
laboral promedio de la familia es 0.2866% mayor para familias en el tercer tercil de ingreso
en comparacion a las familias en el segundo tercil de ingreso, sin importar cuales son los
valores de tamano familiar medio y tasa de desempleo.'

#18.	Estime con lm un modelo en el que prediga la participación en la fuerza laboral 
#en función del tamaño familiar medio, la tasa de desempleo, los terciles de ingreso,
#y las interacciones de los terciles de ingreso con el tamaño familiar medio.

modelo.interaccion = lm(fuerza.laboral~tamano.medio+tasa.desempleo+tercilesingreso+
                           tercilesingreso:tamano.medio)
summary(modelo.interaccion)

#R/
#Residual standard error: 21.9 on 8 degrees of freedom
#Multiple R-squared:  0.2521,	Adjusted R-squared:  -0.3087 
#F-statistic: 0.4495 on 6 and 8 DF,  p-value: 0.8272


#19.	Independientemente de si las interacciones son significativas, escriba las 3 
#ecuaciones implícitas (o sea, las ecuaciones para cada tercil de ingreso), implícitas 
#en el modelo del punto anterior.

#Primera ecuacion implicita del primer tercil
#Fuerza laboral = -130.747 + 50.579tamanno + 0.968tasa.desempleo 

#Segunda ecuacion implicita del segundo tercil
#Fuerza laboral = -130.747 + 50.579tamanno + 0.968tasa.desempleo + 138.928tercilesingreso2 +
#-37.015tamanno*tercilesingreso2

#Tercera ecuacion implicita del tercer tercil
#Fuerza laboral = -130.747 + 50.579tamanno + 0.968tasa.desempleo + 129.244tercilesingreso3 -
#34.517tamanno*tercilesingreso3

#no son paralelas, por las interacciones.