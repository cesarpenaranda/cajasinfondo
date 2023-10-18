c1=c(0.5,-0.5,0.5,-0.5)
c2=c(0,1,0,-1)
c3=c(1,0,-1,0)
c1%*%c2;c1%*%c3;c2%*%c3

crossprod(c1,c2)
crossprod(c1,c3)
crossprod(c2,c3)

jacks=c(38,32,33,35,33)
cuerda=c(39,36,34,45,37)
dillas=c(28,39,36,33,32)
correr=c(40,40,48,45,40)

(x=cbind(jacks,cuerda,dillas,correr))
(t(x))

#hay diferencia en los siguientes pares de tratamientos “correr” vs “jacks”
#y “correr” vs “sentadillas”. Los investigadores plantearon como diferencia 
#relevante en la diferencia de la frecuencia un valor de 10 unidades. 
v1=c(-1,0,0,1)
v2=c(0,0,-1,1)
crossprod(v1,v2)
media_jacks=mean(jacks)
media_correr=mean(correr)
media_sentadillas=mean(dillas)

dif_corr_jack=abs(media_jacks-media_correr)
dif_corr_dillas=abs(media_sentadillas-media_correr)
d=c(dif_corr_jack,dif_corr_dillas)
names(d)=c("C-J","C-D")
d


var=c(var(jacks),var(cuerda),var(dillas),var(correr))
sumvar=sum(var)
cmres=(4/16)*sumvar

ee=sqrt(2*cmres/5)

t=qt(1-0.05/4,16)
(lim=cbind(d-t*ee,d+t*ee))
