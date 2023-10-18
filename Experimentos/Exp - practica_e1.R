
setwd("C:/Users/cesar/OneDrive/Documentos")
load("lectura.Rdata") 
#1.Obtenga un gráfico de los valores de los puntajes del test1 por cada tratamiento.
boxplot(base$test1~base$grupo,main="boxplot de test1 en funcion de grupos",
        xlab="grupos", ylab = "puntaje")
abline(h=mean(base$test1))

#2.Usando la terminología de diseño de experimentos, especifique cada uno de los siguientes conceptos:
#•	Variable respuesta; puntaje del nivel de comprensión de lectura
#•	Factor de diseño; método de enseñanza de lectura 
#•	Niveles del factor; (Basal)(DRTA)(Strat) 
#•	Tratamientos; (Basal)(DRTA)(Strat)
#•	Número de repeticiones; 22

#¿Qué implicaría cometer el error tipo I?
#seria el de rechazar la H0 siendo esta verdadera, implicaría el no conocer en 
#realidad la prueba mas optima para la enseñanza a niños de esta manera perdiendo 
#efectividad en la enseñanza general.

#¿Qué implicaría cometer el error tipo II?; 
#seria el de aceptar la H0 siendo esta falsa, implicaría que se empiece a utilizar
#un método de enseñanza en especifico por ser mas adecuado para la enseñanza sin
#tener cambios notables uno con el otro 

#En qué sentido se podrían hacer generalizaciones en este este estudio
#Comente sobre las limitantes.; 
#edad de los niños, nivel de enseñanza o educación de los niños, nivel de comprensión 
#de lectura en particular de cada uno de ellos, nivel de inteligencia de cada uno
#de los niños

#¿Cuál factor podría controlarse y cuál sería el objetivo de hacer esto?;
#podría controlarse la edad de los niños o el nivel de educación, esto para disminuir
#el ruido en la prueba, haciéndola mas efectiva en general

#¿Cuál puede ser un factor no controlable pero medible?; 
#nivel de compresión de lectura de cada niño

#¿Cuál puede ser un factor de ruido?;
#edad

#3.Obtenga la media, desviación estándar y varianza de los puntajes del test3 
#para cada método.  Obtenga la media general.  Haga un gráfico donde se representen
#las medias de cada especie y la media general.
#Coloque luego los puntos.
mu_bas_t3=mean(base$test3[base$grupo=="Basal"])
media=tapply(base$test3,base$grupo,mean); var=tapply(base$test3,base$grupo,var); ds=sqrt(var)
res=cbind(media,var,ds)
(t(res))

plot(as.numeric(base$grupo),base$test3,xaxt="n",pch=18,cex=0.5,xlab="método")
points(1:3,media,pch="*",cex=2,col=2)
axis(1,1:3,levels(base$grupo))
abline(h=mean(base$test3),col=2,lty=2)
#4.Siga usando los puntajes del test3:
#¿Cuál es el efecto de cada método?  Calcule los efectos manualmente y también
#obténgalos de forma automática en R.

(efectos=media-mean(base$test3))

mod=lm(base$test3~base$grupo)
mod_aov=aov(base$test3~base$grupo)
model.tables(mod_aov)

#Cuando se habla del efecto, ¿a qué se refiere exactamente?;
#Es un tipo de error, es la distancia de la media condicional a la media general 
#de todos los tratamientos

#Calcule el residual para el primer individuo que usó el método estándar.
base$test3[1]-media[1]

#Obtenga todos los residuales manualmente y también de forma automática en R.
base$test3-media[1]
mod_aov$residuals

#¿Cuánto deberían sumar los residuales dentro de cada tratamiento? Verifíquelo
(resi=sum((mod_aov$residuals)))
(round(resi,10))
"deben de sumar cero"
#Dé una estimación de la variabilidad del error para el método estándar.
var(base$test3[base$grupo=="Basal"])

#5.Se quiere probar la hipótesis de que los promedios de los puntajes del test3 
#son iguales para los tres métodos.  Esta pregunta es sólo para el test3.
#Escriba la hipótesis a probar en forma simbólica.

#H0:mu_1=mu_2=mu_3

#Plantee la hipótesis desde el enfoque de los efectos con palabras y con símbolos.

#tau_1=tau_2=tau_3=0

#Si se concluye que algún promedio es más alto que los otros, ¿se puede atribuir
#esa diferencia al método como causa? ¿Entonces es correcto hablar de efecto del
#método en este caso?
anova(mod)
#¿Cuántos grados de libertad deben usarse para el cálculo de la variabilidad 
#entre los 3 promedios?

"k-1; Son 2 grados de libertad, uno menos que el número de grupos."

#Calcule manualmente la estimación para la varianza entre los 3 promedios. 
#No tiene que hacer absolutamente todos los cálculospuede usar resultados intermedios.
#¿Cómo se llama usualmente este resultado?
anova(mod)[1,3]
(cmtrat=(22*sum(efectos^2))/2)
"Se  le llama cuadrado medio entre tratamientos"
#¿Cuántos grados de libertad deben usarse para el cálculo de la varianza dentro 
#de tratamientos?
table(base$grupo)
"n-k;por lo tanto gl63"
#Se pueden obtener 4: la variancia muestral de cada grupo, y el CMRes. La más adecuada es el CMRes
#porque usa la información de todos los datos. Cualquiera puede estar más cerca de la verdadera, pero en
#probabilidad se espera que sea el CMRes el más aproximado al verdadero valor de la variancia común.

#Calcule la variancia ponderada dentro de tratamientos. ¿Cómo se llama usualmente este resultado?
mean(var)
anova(mod)

(cmres=((21*5)*sum(var))/(63*5))

"Esta es la media ponderada de las variancias pero como todos los grupos tienen el mismo número de
réplicas basta hacer un promedio simple. Esto corresponde al Cuadrado Medio Residual."

#Si se aumentara el número de personas en cada método, ¿se podría esperar que la 
#variancia dentro de tratamientos disminuya?
"no, esta seguiria siendo la misma"

summary(mod_aov)$residuals

r=table(base$grupo)
v=tapply(base$test3,base$grupo,var)
v1=sum((r-1)*v)/(sum(r)-3)
v2=mean(v)
anova(mod)

mod_aov$residuals
