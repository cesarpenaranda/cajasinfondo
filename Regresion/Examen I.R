#Examen 
#id	=  Número de identificación del punto de recolección
#ph	= Grado de acidez del agua, medido en “unidades logarítmicas”
#oxig.d =	Oxígeno disuelto, en mg por litro
#cloruro =	Cloruro, en mg por litro
#sulfato =	Sulfato, en mg por litro
#aceites =	Cantidad de aceites, en mg por litro
#alcalinidad =	Alcalinidad (cantidad de CaCO3), en mg por litro
#caudal	Caudal = del río, en metros cúbicos por segundo
#rioarriba:	1= Recolección en río arriba, 0=Recolección en río abajo
#desembocadora:	1=Recolección cerca de desembocadura, 0=Recolección lejos de desembocadura
#urbano:	1=Recolección cerca de zona urbana, 0=Recolección en zona rural
#distrit =	Numeración de distritos en los que está ubicado el punto de recolección: Distrito 1, distrito 2, distrito 3 y distrito 4.



"1.A. Estime con R un modelo lineal en que se prediga el valor del pH en función del 
oxígeno disuelto, el cloruro, el sulfato, la cantidad de aceites, la alcalinidad, el
caudal, si el punto de recolección está río arriba o río abajo, si el punto de recolección 
está cerca o no de la desembocadura y si el punto de recolección está cerca de zona urbana o
cerca de zona rural.  En otras palabras, usar todas las variables independientes excepto distrit y, 
evidentemente, excepto id.   Con base en las salidas de R, conteste las siguientes preguntas: "

load("rio.Rdata")
names(rio)
attach(rio)
mod.ph=lm(ph~oxig.d+cloruro+sulfato+aceites+alcalinidad+caudal+rioarriba+urbano+desembocadura)
summary(mod.ph)

  
"a.	Independientemente del valor del pvalue (probabilidad asociada a la prueba), interprete 
el coeficiente de regresión para la variable sulfato. (3 ptos)"

#R/
#"B3 Por cada aumento de un mg por litro en el sulfato el ph va aumentar en 0.068489 
#manteniendo costante las demas variables"

"b.	Independientemente del valor del pvalue (probabilidad asociada a la prueba), 
interprete el coeficiente de regresión para la variable desembocadura. (3 ptos)"

#"B9 Por cada recolección cerca de desembocadura el ph va aumentar en 0.143478 
#manteniendo costante las demas variables"

"c.	Debido a que no hay interacciones, la diferencia en el PH entre puntos de recolección 
urbanos cerca de la desembocadura por un lado y en puntos de recolección rurales lejos de 
la desembocadura por el otro lado sería la suma de los coeficientes de las variables urbano y 
desembocadura.  Con un procedimiento paso a paso (o sea, “a pie” calculando cada componente),
calcule un intervalo de confianza al 95% para la suma de las estimaciones de esos dos coeficientes 
(Nota: Necesita usar la matriz de variancia-covariancia de los betas). (6 ptos.)"

x1=cbind(1,ph,oxig.d,cloruro,sulfato,aceites,alcalinidad,caudal,rioarriba,urbano,desembocadura)
x1
anova(mod.ph)
CME1=anova(mod.ph)[10,3]
CME1
var1=CME1 *solve(t(x1)%*%x1)
var1
diagonal=sqrt(diag(var1))
diagonal  

#urbano
#0.054573959

#desembocadura
#0.061515426

x.dataframe=data.frame(x1)
y0=predict(mod.ph,data.frame())
y0

n=nrow(x1)
p=ncol(x1)

'Este es el t tabular con n-p gl'

t=qt(.975,n-p)  
ic0=c(y0-t*diagonal,y0+t*diagonal)
'Mi intervalo de confianza'
ic0

"d.	Al 5% de significancia, pruebe la hipótesis nula de que los coeficientes poblacionales 
de las variables oxig.d, cloruro y sulfato son iguales entre sí (6 ptos)."
'H0: Beta1 = Beta2 = Beta3=0
 H1: Beta1<>0 ,Beta2<>0 , Beta3<>0'

summary(mod.ph)
mod.ph.simple.=lm(ph~aceites+alcalinidad+caudal+rioarriba+urbano+desembocadura)
anova(mod.ph.simple.,mod.ph)

#R/
#p-value menor a alfa, por tanto hay suficiente evidencia estadistica para rechazar H0


"1.B. Estime con R un modelo lineal en que se prediga el valor del pH en función 
únicamente del sulfato, aceites, alcalinidad y caudal.  Con base en este modelo, 
conteste las siguientes preguntas:"

mod.1b=lm(ph~sulfato+aceites+alcalinidad+caudal)
summary(mod.1b)
"a.	Cree una matriz de 4 gráficos de dispersión, con la variable ph en el eje de 
las ordenadas, y cada una de las 4 variables cuantitativas (sulfato, aceites, alcalinidad y caudal), 
y señale cuáles son las dos variables predictoras con las que está más clara y fuete la asociación lineal con el ph (6 ptos)."

attach(rio)
par(mfrow=c(1,4))

plot(ph~sulfato,main="Grafico sulfato")

plot(ph~aceites, main="Grafico aceites")


plot(ph~alcalinidad, main="Grafico alcalinidad")


plot(ph~caudal, main="Grafico caudal")

#R/
#Las dos variables predictoras en las que se ve mas clara y fuerte la asociacion lineal con el ph
#son las de sulfato y caudal.



"b.	Calcule el coeficiente de determinación parcial de cada predictor condicional
a la incorporación de los otros 3 predictores, y concluya cuál es el predictor que hace reducir menos la Suma de Cuadrados del Error del modelo más simple (6 ptos.)"

#Para sulfato 
mod.multi.sulfato=lm(ph~sulfato+aceites+alcalinidad+caudal)
mod.parci.sulfato=lm(ph~aceites+alcalinidad+caudal)
anova(mod.multi)
anova(mod.parci.sulfato)
'Forma 1' 
resta.residuos=anova(mod.parci.sulfato)[4,2]-anova(mod.multi.sulfato)[5,2]
resta.residuos

det.parcial=resta.residuos/anova(mod.parci.sulfato)[4,2]
det.parcial
'Forma 2' 
det.parcial2=1-anova(mod.multi.sulfato)[5,2]/anova(mod.parci.sulfato)[4,2]
det.parcial2
#R/
#[1] 0.04247394

#Para aceite 
mod.multi.aceites=lm(ph~sulfato+aceites+alcalinidad+caudal)
mod.parci.aceites=lm(ph~sulfato+alcalinidad+caudal)
anova(mod.multi.aceites)
anova(mod.parci.aceites)

'Forma 1'
resta.residuos2=anova(mod.parci.aceites)[4,2]-anova(mod.multi.aceites)[5,2]
resta.residuos2

det.parcial2=resta.residuos2/anova(mod.parci.aceites)[4,2]
det.parcial2
'Forma 2'
det.parcial22=1-anova(mod.multi.aceites)[5,2]/anova(mod.parci.aceites)[4,2]
det.parcial22
#R/
#[1] 0.002510805

#Para alcalinidad
mod.multi.alcalinidad=lm(ph~sulfato+aceites+alcalinidad+caudal)
mod.parci.alcalinidad=lm(ph~sulfato+aceites+caudal)
anova(mod.multi.alcalinidad)
anova(mod.parci.alcalinidad)

'Forma 1'
resta.residuos3=anova(mod.parci.alcalinidad)[4,2]-anova(mod.multi.alcalinidad)[5,2]
resta.residuos3

det.parcial3=resta.residuos3/anova(mod.parci.alcalinidad)[4,2]
det.parcial3
'Forma 2'
det.parcial33=1-anova(mod.multi.alcalinidad)[5,2]/anova(mod.parci.alcalinidad)[4,2]
det.parcial33
#R
#[1] 0.1746659

#Para caudal
mod.multi.caudal=lm(ph~sulfato+aceites+alcalinidad+caudal)
mod.parci.caudal=lm(ph~sulfato+aceites+alcalinidad)
anova(mod.multi.caudal)
anova(mod.parci.caudal)

'Forma 1'
resta.residuos4=anova(mod.parci.caudal)[4,2]-anova(mod.multi.caudal)[5,2]
resta.residuos4

det.parcial4=resta.residuos4/anova(mod.parci.caudal)[4,2]
det.parcial4
'Forma 2'
det.parcial44=1-anova(mod.multi.caudal)[5,2]/anova(mod.parci.caudal)[4,2]
det.parcial44

#R/
#[1] 0.02263106


#Por lo tanto la que mas me reduce la suma de cuadrados medio del error es caudal,ya que el 
#incorporar el caudal al modelo me reduce la suma de cuadrados del error en 17,4%'

"1.C. Estime con R un modelo lineal en que se prediga el valor del pH en función 
únicamente del caudal, el sulfato, el distrito y la interacción entre sulfato y el
distrito.  Con base en este modelo, conteste las siguientes preguntas:"

mod.inter=lm (ph~caudal+sulfato+distrit+sulfato*distrit)
summary(mod.inter)

"a.	Haga un summary del modelo con interacciones que se le pide en el encabezado 
de la pregunta 1.C, y con base en él, escriba las ecuaciones implícitas para cada 
distrito (independientemente del pvalue). (6 ptos.)"
summary(mod.inter)


"b.	Fijando el valor del caudal en 1 metro cúbico por segundo, diseñe un gráfico
de dispersión entre sulfato y ph, poniéndole distintos colores para los pares ordenados 
correspondientes a cada distrito, y superponga una recta de regresión condicional a cada
distrito con el color correspondiente sobre cada nube de puntos.  Cada una de las rectas
de regresión se estima dentro de cada submuestra definida por cada distrito.  Por esta razón, 
puede usar las ecuaciones estimadas estimadas en el punto anterior (Nota: Sería un gráfico parecido 
al hecho con el gasto en seguros vs el ingreso, para cada nivel de educación).  Interprete si Ud cree
que puede suponer paralelismo, justificando su interpretación. (4 ptos.)"

kol=1*(distrit=="1")+2*(distrit=="2")+ 3*(distrit=="3")+4*(distrit=="4")#Asignacion de colores
plot(ph,sulfato,col=kol,pch=18)
mod1a=lm(ph~sulfato,rio[distrit=="1",])
mod1b=lm(ph~sulfato,rio[distrit=="2",])
mod1c=lm(ph~sulfato,rio[distrit=="3",])
mod1d=lm(ph~sulfato,rio[distrit=="4",])
abline(mod1a,col=1); abline(mod1b,col=2);abline(mod1C,col=3); abline(mod1D,col=4)
legend(10000,150000,c("1","2","3","4","5"),bty="n",lty=1,col=c(2,4),cex = 0.5)
summary(mod1a)$coef
'observamos que las pendientes azul y roja son diferentes'
summary(mod1b)$coef
'cuando tengo dos pendientes distintas esto me indica que tengo una interaccion'
"c.	Al 5% de significancia, pruebe si existe interacción entre sulfato y distrito 
en la predicción del ph, incluyendo al caudal en el modelo (4 ptos.)"
#H0: Omega.minuscula
#H1: Omega.mayusucula

mod.inter10=lm (ph~sulfato*distrit)
summary(mod.inter10)

mod.inter20= lm (ph~caudal+sulfato*distrit) 
summary(mod.inter20)
anova(mod.inter10,mod.inter20)

#Res.Df    RSS Df  Sum of Sq      F Pr(>F)
#1     96 14.564                            
#2     95 14.563  1 0.00079449 0.0052 0.9428

#R/ P-value mayor a alfa, por lo tanto no hay suficiente evidencia estadistica para rechazar H0 

"II.	Preguntas teóricas. "

"1.	Explique brevemente cuál es la diferencia entre normalidad de la variable dependiente
y el supuesto del modelo lineal denominado normalidad condicional de la variable dependiente. (3 ptos)."

#R/
#la diferencia es que la normalidad de la variable dependiente esta condicionada a ella misma,
#mientras que en el supuesto esta normalidad se condiciona al modelo lineal en el que 
#que esta incluido dicha variable dependiente

"2.	Defina qué es una verosimilitud y qué significa que la log-verosimilitud de 
un modelo estimado A sea más cercana a cero que una log-verosimilitud de otro modelo estimado B (3 ptos.)"

#R/
# verosimilitud es una dencidad probabilidad, la cual nos indica que ocurra algo que ya ha ocurrido con anterioridad.
#log-verosimilitud es lo que nos maximisa la verosimilitud

