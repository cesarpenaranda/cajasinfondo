load("bases/arbustos.Rdata")
str(base)
table(base$parcela)
base$tiempo=base$año-2001
table(base$tiempo)
base$parcela=as.factor(base$parcela)
base$parcela=as.numeric(base$parcela)
table(base$parcela)
str(base)
xyplot(tcov~tiempo,groups = parcela,type=c("r"),data = base)
"No se ve una relacion clara entre los interceptos y las pendientes"

# Prueba de correlacion H0: rho=0
mod1=lmer(tcov~tiempo*trt+lluvia+(1+tiempo|parcela),data = base)
summary(mod1)$cov

mod2=lmer(tcov~tiempo*trt+lluvia+(1|parcela)+(0+tiempo|parcela),data = base)
anova(mod1,mod2,test="LRT")

"No se rechaza H0 con un p>0.05, por lo que se asume independencia entre interceptos y pendientes"

# Grafico segun tratamiento a lo largo del tiempo

xyplot(tcov~tiempo, groups = trt,type=c("r","p"),data = base,auto.key = list(columns=3))

"Se nota un leve mayor decrecimiento para el tratamiento nD"

# Modelo 

"y_ij,TL=beta_0+tau_i+beta_1T+delta_iT+u_0j+u_1jT+alpha*L+e_ij"

"y_ij,TL=(beta_0+u_0j)+(beta_1+u_1j+delta_i)T+alpha*L+e_ij"

"\mui,T,L=beta_0+(beta_1+delta_i)T+alpha*L"

#H0: var(u_ij)=0
mod3=lmer(tcov~tiempo*trt+lluvia+(1|parcela),data = base)
anova(mod1,mod3,test="LRT")
"Se rechaza H0 con un p<0.05, por lo que se concluye que no todas las pendientes son iguales segun parcela"
summary(mod2)
contrasts(base$trt)

#para tratamiento C
55.169672-0.686138*T+alphaL
#para tratamiento nD
55.169672+(-0.686138+-0.174492)*T+alphaL
55.169672-0.86063*T+alphaL
#para tratamiendo nH
55.169672+(-0.686138+0.324945)*T+alphaL
55.169672-0.361193*T+alphaL

"Se puede concluir que el tratamiento con mayor decrecimiento es el nD y C ya que por cada ano decrece 0.86 y 0.69 respectivamente"

#Probar si el decrecimiento o crecimiento es mas rapido en alguno de los trat
mod4=lmer(tcov~tiempo+trt+lluvia+(1|parcela)+(0+tiempo|parcela),data = base)
anova(mod2,mod4,test="LRT")
"No se rechaza H0, por lo que que el decrecimiento es igual para todos los tratamientos"

#Estimacion de la tasa de decrecimiento general
summary(mod4)$coef[2,1]
confint(profile(mod4))

"La tasa de decrecimiento anual se estima en un 64%, con un 95% de confianza se espera que la tasa de decrecimiento anual se encuentre
entre 0.21 y 1.07 para cualquier tratamiento"