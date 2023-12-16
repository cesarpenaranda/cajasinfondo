load("bases/riqueza.Rdata")
str(base)

base$riq=apply(base[,2:76]>0,1,sum)

# Grafico 
library(lattice)
xyplot(riq~NAP|playa,type=c("r","p"),data=base)
xyplot(riq~NAP,groups = playa,type=c("r","p"),data=base,auto.key = list(columns=4))
"Si se observa cierta linealidad en la mayoria de las playas"
"Ademas se observa cierta relacion entre los interceptos y las pendientes"

table(base$playa)

beta0=beta1=c()
for (i in 1:9){
  mod=lm(riq~NAP,base[base$playa==i,])
  beta0[i]=mod$coef[1]
  beta1[i]=mod$coef[2]
}

plot(beta1,beta0)
abline(lm(beta0~beta1))
cor(beta0,beta1)

"Se observa cierta correlacion negativa entre interceptos y pendientes"

#H0 rho=0

mod1=lmer(riq~NAP+(1+NAP|playa),data = base)#
summary(mod1)$varcor
mod2=lmer(riq~NAP+(0+NAP|playa),data = base)

anova(mod1,mod2,test="LRT")

"Se rechaza H0 con un p<0.05, por lo que no hay independencia entre pendientes e interceptos"


#H0: var(u_1j)=0
mod3=lmer(riq~NAP+(1|playa),data = base)
anova(mod1,mod3,test="test")

"Se rechaza H0 con un p<0.05, por lo que las pendientes no son iguales para todas las playas"

confint(profile(mod1))

"Se espera con un 95% de confianza que la desviacion estandar de los interceptos para las playas varie de 2.27 a 7.37 ademas vemos una correlacion negativa
no despreciable entre interceptos y pendientes la cual varia de 1 a 0.36, por otra parte se espera que la riqueza promedio de las especies se encuentre
entre 2.16 y 3.44 cuando se ubica en el nivel medio de la marea al ademas se espera un decrecimiento de las especies que va de 1.36 a 4.40
por cada unidad del NAP"


#Cosiderar interaccion entre playa y NAP
options(contrasts=c("contr.sum","contr.poly"))

#modelo lineal simple
mod4=lm(riq~NAP*factor(playa),data = base)
drop1(mod.4,test = "F")
"La conclucion es la misma pero para el modelo lineal se tripica"
confint(mod4)[2,]
confint(mod1)[6,]

"El intervalo para el modelo lineal es mas reducido debido a los errores"
summary(mod4)$coef[2,2]
summary(mod1)$coef[2,2]
0.7230539-0.4351066
"al comparar los dos encontramos una diferencia de 0.29 en el error siendo el mayor el del modelo mixto ya que toma encuenta el efecto 
aleatorio de la playa"

#problema que se encuentra 
"El principal problema es que el modelo ordinario solo es valido para esas 9 playas, ademas que no toma la correlacion existente en las 
observaciones de las mismas, asumiendo que estas son independientes"