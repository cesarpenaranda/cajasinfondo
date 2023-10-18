
bicarbonato=c(6.7,6,6.5,4.9,6)
limon=c(6.1,6.8,6.3,6.4,6.1)
sal=c(6.4,6.4,6,5.3,6.1)
vinagre=c(7.3,7.1,7.2,6.8,7.3)
replicas=c("I","II","III","IV","V")

(x=t(cbind(bicarbonato,limon,sal,vinagre)))
colnames(x)=replicas
#hay diferencia en los siguientes pares de tratamientos “vinagre” vs “bicarbonato”
#y “vinagre” vs “sal”. Los investigadores plantearon como diferencia relevante en
#la diferencia de la frecuencia un valor de 1 día. 
#Con los siguientes datos, y el programa R, calcule los intervalos de confianza para 
#los dos pares de promedios en los que se encontraron diferencias (10 puntos). 
#Interprete los resultados (5 puntos). 
bicar=1 ; limon=2 ; sal=3 ; vinagre=4

H0: mu4=mu1
mu4-mu1
L1=c(-1,0,0,1)
H0: mu4=mu3
mu4-mu3
L2=c(0,0,-1,1)
crossprod(L1,L2)

(media_bi=mean(bicarbonato))
(media_vi=mean(vinagre))
(media_sal=mean(sal))

d1=abs(media_bi-media_vi)
d2=abs(media_vi-media_sal)
d=c(d1,d2)
names(d)=c("v-b","v-s")
var=c(var(bicarbonato),var(sal),var(limon),var(vinagre))
(cr2=mean(var))
sumvar=sum(var)
(cmres= (4*sumvar)/(16))
ee=sqrt(2*cmres/5)
(q=d/ee)
(names(q)=names(d))
q


t=qt(1-0.05/4,16)
(lim=cbind(d-t*ee,d+t*ee))
names(datos)[5] <- "numero_replica"
datos
