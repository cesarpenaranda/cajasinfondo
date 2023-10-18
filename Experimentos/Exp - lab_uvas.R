"En Costa Rica se cultivan dos especies de uva: roja y blanca. Como medida de dulzor
se utiliza la escala Brix. Se tomaron medidas de los grados brix de una muestra de
uvas de ambas especies provenientes de 3 sitios: La Garita, La Guácima y San Vito.
Se quería determinar si el dulzor promedio varía según la localidad de producción.
Los investigadores consideran que una diferencia en el dulzor promedio de dos
poblaciones debe ser al menos de medio grado brix para considerarse relevante."

#1. Preparación:
#(a) Cargue el archivo uvas.csv. Verifique que la variable localidad esté
#definida correctamente como un factor.
base=read.csv("uvas.csv")
#Garita
#Gu<e1>cima
#San Vito
class(base$localidad)
base$localidad=as.factor(base$localidad)
levels(base$localidad)=c("guacima","garita","san vito")

#(b) Comente las características de este experimento. Si solo se están
#comparando los promedios entre las tres localidades, ¿es necesario tomar
#en cuenta que las uvas son de dos tipos?

"Dependiendo del interes del experimento esto es necesario, pero para la disminucion del ruido en el
experimento, es necesario tener en cuenta todas las caracteristicas posibles medibles para de esta manera
tener resultados mas acertados"
#2. Hipótesis básica:
#(a) Establezca la hipótesis básica para verificar que en efecto las tres
#localidades no producen el mismo promedio de dulzor.
H0: mu_1=mu_2=mu_3 
H0: tau_1=tau_2=tau_3=0

#(b) Haga un boxplot que permita de una forma descriptiva apoyar o
#contradecir esta hipótesis.
media=mean(base$brix)
boxplot(base$brix~base$localidad,ylab = "Escala brix",xlab = "Localidad")
abline(h=media,col="red")
m=tapply(base$brix,base$localidad,mean)
points(1:3,m,col="green",pch="-",cex=2)
#(c) Obtenga las medias de cada tratamiento.
(tapply(base$brix,base$localidad,mean))
#(d) Ponga a prueba la hipótesis.
mod=lm(base$brix~base$localidad)
aov=aov(mod)

summary(aov)
anova(mod)
#3. Comparaciones de promedios:
#(a) Dado que el objetivo es comparar todas las localidades entre sí, se trata
#de un problema de comparación de todos los pares de promedios. Escriba
#todas las hipótesis que se deben probar.

#H0: mu_1=mu_2
#H0: mu_1=mu_3
#H0: mu_2=mu_3

#(b) Verifique que estas hipótesis no son ortogonales.
v1=c(1,-1,0); v2=c(1,0,-1); v3=c(0,1,-1)
v1%*%v2; v1%*%v3; v2%*%v3
#(c) Obtenga el cuadrado medio residual.
anova(mod)[2,1]
#(d) Obtenga los estadísticos de interés para realizar cada prueba, es decir, debe
#calcular y¯i−y¯j Puesto que solo interesa ver si existen diferencias entre cada 
#par de promedios, use el valor absoluto de las diferencias |y¯i −y¯j|.
m=tapply(base$brix,base$localidad,mean)

dif_12=abs(m[1]-m[2])
dif_13=abs(m[1]-m[3])
dif_23=abs(m[2]-m[3])
b=c(dif_12,dif_13,dif_23)
n=c("gua-gari","guaci-sv","garit-sv")
names(b)=n
b
#(e) Obtenga el error estándar del estadístico |y¯i −y¯j| en cada caso.
sum(base$localidad == "guacima")
sum(base$localidad == "san vito")
sum(base$localidad=="garita")
anova(mod)
ee1=sqrt(1.8095*(1/25+1/38))
ee2=sqrt(1.8095*(1/25+1/37))
ee3=sqrt(1.8095*(1/37+1/38))
ee=c(ee1,ee2,ee3);names(ee)=n
ee
#(f) Obtenga el valor estandarizado del estadístico dividiéndolo por su error
#estándar.
q=b/ee;names(q)=n
q
#(g) Encuentre la probabilidad de obtener un valor igual o mayor al
#estadístico usando la distribución del rango estudentizado de Tukey.
#Use ptukey(q*sqrt(2),k,df,lower.tail = F), donde k es el número
#de grupos y df son los grados de libertad de los residuales. Se
#debe multiplicar el valor q por √2 porque la función de R asume
#que los dos grupos tienen igual número de réplicas e incluye en el
#denominador √2, lo cual debe corregirse. Con el argumento lower.tail=F
#se obtienen directamente las dos colas. Esta probabilidad debe compararse
#directamente contra α puesto que se trata de pruebas de dos colas.
anova(mod)
(prueba_tukey=ptukey(q*sqrt(2),3,97,lower.tail = F))
round(prueba_tukey,4)
#(h) Obtenga estas probabilidades automáticamente usando la función
#TukeyHSD(mod). Las probabilidades que se obtienen con esta función son
#relativas a hipótesis de dos colas, mientras que los intervalos de confianza
#son válidos solo si se rechazan todas las comparaciones de pares de
#promedios. En caso de que haya diferencia entre solo algunos pares, debe
#hacerse la corrección de Bonferroni para obtener los límites de confianza
#simultáneos solo para aquellos pares en que se encontraron diferencias
#significativas.
#(i) ¿Qué se concluye en términos de las hipótesis que se probaron?
TukeyHSD(aov)
#4. Límites para las diferencias:
#(a) Obtenga intervalos de confianza para la diferencia de las medias solo en
#los casos en que se encontró una diferencia significativa. Se debe obtener
#el valor de la distribución t con los grados de libertad residuales (gl). Este
#valor se obtiene haciendo el ajuste de Bonferroni para tener un nivel de
#95% de confianza para todos los intervalos en conjunto, lo cual se hace
#con t=qt(1-0.05/(2*d),gl), donde d es el número de intervalos. Luego
#se calculan los límites de confianza con:
#IC = |y¯i −y¯j| ±t · ee_ij

(t=qt(1-0.05/(2*2),97))
b
(ic=b+t*ee)
(ic2=b-t*ee)
tabla=c(ic2[1],ic[1],ic2[2],ic[2],ic2[3],ic[3])

matris=round(matrix(tabla,nrow = 3,byrow = T),2)
rownames(matris)=n
colnames(matris)=c("low","upper")
matris
#(b) ¿Qué se concluye en términos generales?
