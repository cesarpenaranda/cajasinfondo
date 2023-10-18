load("~/migraciontica.Rdata")
###

library(foreign)
attach(migraciontica)
names(migraciontica)
View(migraciontica)
str(migraciontica)
###Regresion logistica de condición de migrante en función de
###años de educación, edad, y haber estado casado en país de origen


logistica1=glm(inus~educ+edad+casado,family=binomial(link=logit))
summary(logistica1)

###Interpretación de coeficientes se hace exponenciando


exp(logistica1$coefficients)

#Interpretaciones
#Para educ
(1.2531060-1)*100
"Por cada año adicional de educacion de un costarricense, los ODDS de migrar 
aumentan en un 25,31%, manteniendo constante las demas variables"
#Para edad
(0.9554737-1)*100
"Por cada año adicional de edad de un costarricense, los ODDs de migrar disminuyen en
un 4,45%, manteniendo constante las demas variables"
#Para casado
(0.2408244-1)*100
"Entre los casados, los ODDS de migrar son 76% menores a los ODDS de migrar de los no casados,
manteniendo constantes las demas variables"

###Intervalos de confianza para los coeficientes y para los OR:

confint(logistica1)
exp(confint(logistica1)) #intervalos de confianza para los odds

###Predichos en términos lineales y predichos en términos de probabilidad

pred.lineal <- predict(logistica1)
pred.prob <- predict(logistica1, type="response")

summary(cbind(pred.lineal,pred.prob))

predict(logistica1,data.frame(educ=10,edad=25,casado=1), type="response")


###Con esta función se confirma cuánto es el valor de la función de 
###log-verosimilitud, así como el valor del AIC


logLik(logistica1) #cuanto mas cerca de cero, mejor es el ajuste, mientras mas negativo sea
#peor es el ajuste

logistica1$aic

AIC(logistica1)
BIC(logistica1)

###Cómo se calcularía "a pie" el BIC y el AIC?
(-2*-63.17+2*4)

AIC_f = function (x){
  result=AIC(x)
  print(paste('Valor de la funcion del AIC: ', round(result,4)))
  -2*logLik(x)+2*(length(x$coef)+1)
}
AIC_f(logistica1)

BIC_f = function (x){
  result=BIC(x)
  print(paste('Valor de la funcion del BIC: ', round(result,4)))
  -2*logLik(x)+(length(x$coef)+1)*log(length(x$fitted))
}
BIC_f(logistica1)

###El seudo-R2 se calcula con el modelo nulo


nulo=glm(inus~1,family=binomial(link=logit))

(seudoR2 <- (logLik(nulo)-logLik(logistica1))/logLik(nulo))
#interpretacion
"El modelo observado cambia Logverosimilitud del modelo nulo en un 25%"
#cuanto mas cercano a 100% es mejor porque significa que lo cambia mas


###La siguiente es la librer?a para estimar la prueba de Hosmer y Lemeshow
###En forma directa

library(ResourceSelection)

anova(logistica1,test="LRT") 
deviance(logistica1) #Queremos un resid.dev lo mas pequeño posible


####H0: El modelo tiene un buen ajuste a los datos
hlt=hoslem.test(inus,logistica1$fitted,g=10)
hlt

###Aquí se puede observar los valores esperados y los observados
cbind(hlt$observed,hlt$expected)

###La siguiente es la tabla de clasificación, usando el 0.5 como punto de corte
library(car)
clasif=recode(logistica1$fitted,"0:0.5=0;0.5:1=1")
table(clasif,inus)/length(inus)

0.84507042+0.03755869

###Ahora la tabla de clasificación usando la proporción global como punto de corte

summary(inus)

clasif2=recode(logistica1$fitted,"0:0.1362=0;0.1362:1=1")
table(clasif2,inus)/length(inus)


0.70422535+0.08920188


###Nótese la particularidad de la forma del gráfico de los residuos de deviancia
###vs. los predichos

plot(logistica1$fitted,logistica1$residuals)
"Por definicion la regrecion logistica es una regrecion heterosedastica por definicion"
title("Predichos vs. residuos de deviancia")







###Note ahora que se puede analizar los mismos estadísticos de influencia y 
###extremos que se analizaron en el modelo gaussiano





###Valores influyentes###

###Leverage###
lev.logistica1=hatvalues(logistica1)

###DFFIT
dffit.logistica1=dffits(logistica1)

###D de Cook
cook.logistica1=cooks.distance(logistica1)


###DFBETAs
dfbeta.logistica1=dfbetas(logistica1)


summary(cbind(lev.logistica1,dffit.logistica1,cook.logistica1))

summary(dfbeta.logistica1)

###Definir los l?mites para considerar como valor extremo

##Para leverage

lim.lev=2*length(logistica1$coef)/length(lev.logistica1)

plot(logistica1$fitted,lev.logistica1)
abline(h=lim.lev, col=5)
title("Leverage por valores predichos")
identify(logistica1$fitted,lev.logistica1,id)

plot(logistica1$fitted,logistica1$residuals, cex=lev.logistica1/mean(lev.logistica1))
abline(h=0, col=6)
title("Residuos por valores predichos, ponderados por leverage")
identify(logistica1$fitted,logistica1$residuals,id)

###Para DFFITs

plot(logistica1$fitted,logistica1$residuals, cex=0.5*abs(dffit.logistica1)/mean(abs(dffit.logistica1)))
abline(h=0, col=6)
title("Residuos por valores predichos, ponderados por DFFIT")
identify(logistica1$fitted,logistica1$residuals,id)


lim.dffit=2*((length(logistica1$coef)/length(lev.logistica1))^.5)



###Para D de Cook

###Gráfico ponderado por D de Cook

plot(logistica1$fitted,logistica1$residuals, cex=0.5*cook.logistica1/mean(cook.logistica1))
abline(h=0, col=6)
title("Residuos por valores predichos, ponderados por D de Cook")
identify(logistica1$fitted,logistica1$residuals,id)

lim.dcook2=qf(0.5,length(logistica1$coef),(length(cook.logistica1)-length(logistica1$coef)))
lim.dcook1=qf(0.2,length(logistica1$coef),(length(cook.logistica1)-length(logistica1$coef)))

lim.dcook2
lim.dcook1

###Gráfico para identificar límite de D de Cook

plot(logistica1$residuals, cook.logistica1,ylim=c(0,1))
abline(h=lim.dcook1, col=6)
abline(h=lim.dcook2, col=6)
title("D de Cook vs. Residuos")
identify(logistica1$residuals, cook.logistica1,id)


