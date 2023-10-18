load("baseparaboot.Rdata")
library(boot)

attach(baseparaboot)

ingreso = ingreso.3

#Funcion varianza del logaritmo 

varln = function(z,d)
{
  operacion = var(log(z[d]))
}
varln(ingreso)

#boostrapiando la funcion
x = boot(
  ingreso,
  varln,
  R=999
  )
x

ci =boot.ci(x,type = "perc")
ci

boxplot(x$t[,1])

#aktquinson 
atk = function(z,d)
{
  meanlog= mean(log(z[d]))
  expmenlog=exp(meanlog)
  atkk=1-expmenlog/mean(z[d])
  atkk
}

x2 = boot(ingreso,atk,R=10000)
x2

ci2 = boot.ci(x2,type = "perc")
ci2
x2











