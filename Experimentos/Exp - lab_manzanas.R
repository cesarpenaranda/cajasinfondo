"Las manzanas tienen un compuesto llamado polifenol oxidasa, el cual hace que al
cortarse y entrar en contacto con el aire se oscurezcan rápidamente. Para evitar el
pardeamiento se probaron tres tratamientos: tapar (código 2), poner en bolsa plástica
cerrada (código 3), y aplicar jugo de limón (código 4). Además, se incluyó un control
sin aplicar nada (código 1). Se seleccionan 40 manzanas y a cada una se le aplica
aleatoriamente uno de los 4 tratamientos, lo cual resulta en 10 manzanas para cada
tratamiento. Una vez aplicado el tratamiento a cada manzana, se pide a 3 jueces que
califiquen el color en una escala de 1 a 6, donde 1 es el color normal de la fruta y 6 es
el más oscuro. Cada manzana recibe como calificación el promedio de los 3 jueces. El
objetivo final es seleccionar el tratamiento que mantenga mejor el color original para
una empresa que se encarga de banquetes.
En un primer análisis solo se va a investigar si existe alguna diferencia en el color
promedio resultante con los cuatro tratamientos."

#1. Preparación:
#(a) Lea el archivo manzanas.csv en R.
base= read.csv("manzanas.csv",header=TRUE, sep=";")
View(base)
#(b) Defina correctamente el factor y ponga las etiquetas correspondientes para
#cada uno de los tratamientos.
names(base)
base$trat=as.factor(base$trat)
levels(base$trat)=c("control","tapar","plastica","limon")
#(c) Guarde la base en un archivo llamado manzanas.Rdata para ser utilizado
#en futuros ejercicios.
save(base,file = "manzanas.Rdata")
#2. Análisis gráfico:
#(a) Obtenga una tabla con los promedios de cada tratamiento, y llámela m.
(m=tapply(base$color, base$trat,mean))
(promedio_grupo <- mean(base$color[base$trat == "tapar"]))
#(b) Obtenga una tabla con las variancias por tratamiento, y llámela v.
v=tapply(base$color, base$trat, var)
(var_grupo= var(base$color[base$trat=="tapar"]))
#(c) Obtenga la media general de la respuesta y llámela media.
(media=mean(base$color))
#(d) Haga un boxplot para analizar el efecto de los tratamientos
#sobre la respuesta promedio. Agregue la media general usando
#abline(h=media,col=2) y las medias de los tratamientos usando
#points(1:4,m,col=4,pch="-",cex=2).
boxplot(base$color~base$trat,main="boxplot de los tratamientos")
abline(h=media,col=2)
points(1:4,m,col=4,pch="-",cex=2)
#(e) Obtenga los efectos muestrales de cada tratamiento a partir de la tabla de
#medias y compare estos resultados con lo que ve en el gráfico. Cada efecto
#se puede estimar como: τˆj = y¯j −y¯.
(efectos=m-media)
#(f) Explique el significado de cada uno de los valores obtenidos para los
#efectos muestrales.
"Lo que observamos en el objeto efectos, es la diferencia entre las medias condicionales
de cada uno de los tratamientos con respecto a la media general de los cuatro tratamientos, 
en otras palabras lo que vemos es el mu_j-mu=tau_j"
#(g) Obtenga la suma de los efectos anteriores.
(sum(efectos))
#(h) Obtenga una estimación de la variancia del error a partir de la tabla de
#variancias. La estimación debe ser la media ponderada de las variancias en
#los tratamientos, las cuales se ponderan con los grados de libertad de cada
#variancia, sin embargo, en este caso se tiene el mismo número de réplicas
#en todos los tratamientos, por lo que basta hacer un promedio simple de
#las variancias.
(mean(v))
#3. Análisis de variancia:
#(a) Ajuste un modelo lineal. Use tanto la función aov como la función lm; la
#diferencia principal es que con lm se pueden obtener los coeficientes del
#modelo, mientras que con aov se puede obtener la tabla de efectos. En
#todo caso, cuando usa lm, por ejemplo mod=lm(y~x), luego puede obtener
#mod1=aov(mod) de la misma forma que haciendo mod1=aov(y~x).
mod=lm(base$color~base$trat)
mod1=aov(mod)

#(b) Obtenga los resultados del análisis de variancia mediante anova(mod) o
#anova(mod1). Si usa la función aov da lo mismo usar summary(mod1) o
#anova(mod1).
anova(mod)
summary(mod1)
#(c) Observe la línea de residuales para obtener el cuadrado medio residual y
#compárelo con la estimación de la variancia del error obtenida en el punto
#anterior.
vector=c(mean(v),anova(mod)[2,3])
nombres=c("Estimacion","Error del punto")
tabla=matrix(vector,nrow = 1,byrow = T)
colnames(tabla)=nombres
tabla
#(d) Observe los grados de libertad residuales y justifique por qué se obtiene
#ese número.
grados=c(anova(mod)[2,1],length(base$color))
nombres2=c("grados","n")
(t2=matrix(grados,nrow = 1))
colnames(t2)=nombres2
t2

"Este numero de grados se obtiene del n-k el cual es la resta del numero total de muestra 
menos el numero de grupo, tratamientos del experimento el cual en este caso son 4"
#(e) Observe la línea del tratamiento y obtenga la suma de cuadrados de
#tratamiento.
anova(mod)
(Sumsq_trat=anova(mod)[1,2])
#(f) Haga la suma de los cuadrados de los efectos obtenidos anteriormente.
#Observe que estos cuadrados deben multiplicarse por el número de
#réplicas para obtener exactamente la suma de cuadrados de tratamiento.
#Justifique por qué esto debe ser así.
(sumsq_efectos2=sum(efectos^2))
#(g) Compare la variabilidad de los promedios con la variabilidad residual
#para determinar si hay alguna evidencia de diferencias entre las medias
#de la respuesta.
anova(mod)
cmtrat=anova(mod)[1,3]
cmres=anova(mod)[2,3]
(f=cmtrat/cmres)
#(h) Establezca adecuadamente la hipótesis que está poniendo a prueba y dé
#una conclusión.
H0=mu1=mu2=mu3=mu4
H0=tau1=tau2=tau3=tau4=0
#4. Estimación de parámetros del modelo de tratamiento referencia:
#(a) Obtenga las estimaciones de los parámetros del modelo. Por default R usa
#el modelo de tratamiento referencia. Esto se logra con el ajuste hecho con
#lm mediante summary(mod) o mod$coef.
summary(mod)$coef
mod$coefficients
#(b) ¿Qué significa el intercepto en este modelo?

"Puesto que se usa el modelo con el control como referencia, el intercepto
coincide con la media del control que es justamente 5,4"

#(c) ¿Qué representa cada uno de los coeficientes del modelo?
media_control=mean(base$color[base$trat=="control"])
(m-media_control)
#(d) Obtenga la matriz de estructura y observe la codificación de las variables
#auxiliares.
model.matrix(mod)
#(e) A partir de los coeficientes obtenidos, obtenga los efectos muestrales y
#compárelos con los obtenidos en el punto 2e).
mod$coef
efectos
#(f) Obtenga los efectos directamente con model.tables(mod) (solo funciona si
#el modelo fue hecho con la función aov).
model.tables(mod1)
#5. Modelo de suma nula:
#(a) Cambie al modelo de suma nula usando la siguiente instrucción:
#options(contrasts=c("contr.sum","contr.poly")).
options(contrasts=c("contr.sum","contr.poly"))
contrasts(base$trat)
options(contrasts=c("contr.treatment","contr.poly"))
contrasts(base$trat)
#para volver al modelo de tratamiento referencia se usa:
#options(contrasts=c("contr.treatment","contr.poly")).
#(b) Verifique la codificación con contrasts(base$trat).
contrasts(base$trat)
#(c) Repita los pasos del punto 4. Compare los resultados.

summary(mod)$coef
mod$coefficients
model.matrix(mod)
