library(car)
library(lmtest)
library(tseries)
library(e1071)
library(dglm)
library(MASS)
###
scar=function(x){
  scatterplot(x$fitted,x$residuals)
}
###

load("~/crime1.Rdata")
attach(crime1)
View(crime1)
names(crime1)
narr866=recode(narr86,"0=0;1:12=1")
crime1$narr866 = narr866
detach(crime1)
attach(crime1)

#1.	 Cree una variable que se llame lognarr que sea igual al logaritmo natural 
#de (narr86+1).

lognarr=log(narr86+1)

#2.	Estime un modelo gaussiano en el que lognarr esté en función de pcnv, ptime86,
#inc86, black y hispan
  
mod1=lm(lognarr~pcnv+ptime86+inc86+black+hispan)

#3.	Con la función aic(), cálculele el AIC

AIC(mod1)

#4.	Estime un modelo logístico en el que la variable arrestado sea predicha por 
#pcnv, ptime86, inc86, black y hispan.

mod_log=glm(narr866~pcnv+ptime86+inc86+black+hispan,family=binomial(link=logit))
summary(mod_log)

#7.	Interprete los Odds Ratio para inc86 e hispan.
exp(coef(mod_log))

OR_inc86=0.992
(0.992-1)*100

"-0.8%"
#Interpretacion
"por cada cien dolares adicionales de ingreso los odds de que una persona sea 
arrestada disminuyen en 0.8% manteniendo constante las demas varibles"

OR_hispa=1.649
(1.649-1)*100
"64.9%"
#Interpretacion
"Los odds de ser arrestado entre los hispanos son 64.9% mayores que entre los no 
hispanos,manteniendo constante las demas varibles"

exp(B)=1.101
(1.101-1)*100
"10.1%"
#Interpretacion
"El numero de arrestos entre los hispanos son 10.1% mayores que entre los no 
hispanos,manteniendo constante las demas varibles"

# 8.	Pruebe la bondad de ajuste del modelo con una prueba de Hosmer y Lemeshow, al 5%.

library(ResourceSelection)

#H0: El modelo tiene un buen ajuste a los datos

hlt=hoslem.test(narr86,mod_log$fitted,g=10)
hlt

#10.	Haga un gráfico de residuos de deviancia vs. leverage, identificados por 
#número de arrestos.  
lev.mod_log=hatvalues(mod_log)

lim.lev=2*length(mod_log$coef)/length(lev.mod_log)

plot(mod_log$residuals,lev.mod_log)
abline(h=lim.lev, col=5)
title("Leverage por valores predichos")
identify(mod_log$residuals,lev.mod_log,narr86)


