pchisq(42.1,1,lower.tail = F)

load("bases/sueño.Rdata")
str(base)
table(base$sujeto)
length(base$sujeto)
#2 Relacion entre pendientes e interceptos, regresion simple para cada sujeto, dias predictor y reac respuesta

names(base)
table(base$sujeto)
names(table(base$sujeto))
length(names(table(base$sujeto))
       
       
beta0=beta1=c()
id=as.numeric(names(table(base$sujeto)))
for(i in 1:18) {
  mod=lm(reac~dias,base[base$sujeto==id[i],])
         beta0[i]=mod$coef[1]
         beta1[i]=mod$coef[2]
}


library(lme4)
library(lattice)

#@ Relacion entre los valores de las pendiente e intercetos
xyplot(reac~dias|sujeto,type=c("r","p"),data = base)
xyplot(reac~dias,groups=sujeto,type=c("r","p"),data = base)

"No parece haber una relacion clara entre pendientes e interceptos"

#@ Grafico de relacion entre pendientes e interceptos, correlacion de los mismos
plot(beta1,beta0,pch=18)
abline(lm(beta0~beta1))
cor(beta0,beta1)

"La correlacion entre pendientes e intercetos es de -0.1375534 la cual es una correlacion baja, ademas en el grafico no se observa una 
correlacion clara"

#Modelo lineal odinario, ecuacion sin considerar la tendencia de cada sujeto
mod0=lm(reac~dias,data = base)
summary(mod0)$coef

"\hat{y}=251.4+10.5X"
  
#@ Ajustes de dos modelos 1 con correlacion entre interceptos y pendientes, 2 sin correlacion entre interceptos y pendientes

#modelo 1
mod1=lmer(reac~dias+(1+dias|sujeto),data = base)
summary(mod1)$varcor
mod2=lmer(reac~dias+(1|sujeto)+(0+dias|sujeto),data = base)#
summary(mod2)
anova(mod1,mod2,test="LRT")

"En este caso no se rechaza H0 con un p>0.05 por lo que se asume que no hay correlacion entre pendientes e interceptos"

#@ Verificar si todos los sujetos tienen la misma pendiente H0: var(b_1,j)=0

mod3=lmer(reac~dias+(1|sujeto),data = base)
anova(mod1,mod3,test="LRT")

"Se rechaza H0 con un p<0.05, por lo que sigma_1=/0"

#Interpretacion en terminos dele problema
"En este caso con el resultado anterior no podemos decir que todos los sujetos tengan una tendencia igual, por lo que a los individuos les afecta
la ausencia de sueño en la capacidad de reaccion a ritmos distintos, por lo que un individuo puede tener un ritmo pronunciado de perdida en la capacidad
de reaccion a lo largo del tiempo, mientras que a otro puede ser que apenas y sienta diferencia en su capacidad de reaccion a lo largo del tiempo "

summary(mod2)

"\hat{y}=251.4+10.46X"

#Errores estandar de los errores
lm=summary(mod0)$coef[,2]
lmer=summary(mod2)$coef[,2]
cbind(lm,lmer)
"Podemos ver como los errores en el caso del modelo mixto son mas altos, ya que estos toman en cuenta que los individuos provienen de una poblacion,
mientras que la regresion hace la estimacion para esos individuos en especifico, por lo que las generalizaciones pueden ser aplicadas unicamente para ese grupo"

confint(profile(mod2))
"Se espera con un 95% de confianza global se espera que el ritmo de crecimiento en el tiempo de reaccion por ausensia de sueño
se encuntre entre 7.33 y 13.6 mm por cada dia que pase"