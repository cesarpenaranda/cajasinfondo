load("bases/ortodoncia.Rdata")
str(base)
base$edad1=base$edad-8

#2 Grafico para apreciar la tendencia de los ninos 
xyplot(distancia~edad1,groups = sujeto,type=c("r","p"),data=base)

"Se ve un crecimiento algo uniforme entre los ninos pero no se puede observar que entre mas alta las distancia a los 8 anos, los ninos experimenten
un crecimiento mas pronociado a lo largo del tiempo en la distancia"

#3 Grafico crecimiento de la distancia en funcion del tiempo para hombres, mujeres a lo largo del tiempo, por sujeto y general por sexo

#grafico 1
xyplot(distancia~edad1|sexo,groups = sujeto, type=c("r","p"), data=base)
#grafico 2
xyplot(distancia~edad1, groups = sexo, type=c("r"),data=base, auto.key = list(columns=2))

"apartir del grafico de las tendencias generales por sexo se observa que para los masculinos hay un ritmo de crecimiento de la distancia mas
pronunciado"

beta0=beta1=c()
id=as.numeric(base$sujeto)
length(id)

for(i in 1:27){
  mod=lm(distancia~edad1,base[id==i,])
  beta0[i]=mod$coef[1]
  beta1[i]=mod$coef[2]
}

cor(beta0,beta1)

"Hay una correlacion negativa baja"

plot(beta0,beta1)
"Para el grafico no se observa una correlacion entre interceptos y pendientes"
names(base)
#Modelos
mod1=lmer(distancia~edad1*sexo+(1+edad1|sujeto),data=base) # con correlacion
summary(mod1)$varcor

mod2=lmer(distancia~edad1*sexo+(1|sujeto)+(0+edad1|sujeto),data = base)

anova(mod1,mod2,test="LRT")

# H0: roh=0
"No se rechaza H0 con un p>0.05, por lo que los interceptos y pendientes son independientes"

# Ahora probamos H0: var(b_1,j)=0

mod3=lmer(distancia~edad1*sexo+(1|sujeto),data = base)#
anova(mod1,mod3,test="LRT")

"No se rechaza H0 con un p>0.05, por lo que no se asumen pendientes aleatorias entre los sujetos, todos los individuos dentro de cada sexo tienen
la misma pendiente"

#H0: delta_j=0
mod4=lmer(distancia~edad1+sexo+(1|sujeto),data = base)
anova(mod3,mod4,test="LRT")

drop1(mod3,test="Chisq")

"No se rechaza H0 con un p>0.05 por lo que la interaccion de sexo y edad es significativa, se encuentran distintos ritmos de crecimientos 
para hombres y para mujeres"

# Modelo para los individuos
"yij,T=beta_0+ tau_i+beta_1T+delta_iT+u_0,j+e_ij"
#i-esimo sexo, j-esimo individuo

# Modelo para los hombres
contrasts(base$sexo)
"\muH,T=beta_0+ tau_1+beta_1T+delta_1T"

# Modelo para los mujeres
"\muF,T=beta_0+beta_1T"

#Estimar cuanto crece cada ano en prom, entre hombres y mujeres
summary(mod3)
#hombres
(21.20909+1.40653)+(0.47955+0.30483)*T
22.61562+0.78438*T
#mujeres
21.20909+0.47955*T

"Podemos decir que por cada ano que pasa la distancia promedio para los ninos aumenta 0.78mm mientras que para las ninas por cada ano que pasa
la distancia promedio aumenta 0.48, podemos ver que los ninos tienen una tendencia de crecimiento mayor que las ninas"
22.61562-21.20909
1.40653

"Esto difiere a los 8 anos de edad"
22.61562+0.78438*6
27.3219
21.20909+0.47955*6
24.08639
27.3219-24.08639

3.23551

"A los 14 anos esta diferencia aumenta considerablementa, ya ahora la distancia promedio para los hombres es 4.44mm mayor que para las mujeres"