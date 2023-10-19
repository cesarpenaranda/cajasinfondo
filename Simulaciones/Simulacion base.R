####Dos formas de generar datos simulados para una regresion

###Primero, genero mis x

###Se escogió un tamaño de muestra de 20

x1=rnorm(20, mean=10, sd=2)
x2=rnorm(20, mean=13, sd=3)

cbind(x1,x2)

###Primera forma: Con los residuos

##beta0=2
##beta1=3
##beta2=4

###errores son normales con sigma cuadrado=4
###error estandar residual=2

y=2+3*x1+4*x2+rnorm(20,mean=0,sd=2) # error se distribuye normal con mu = 0 y sd =2.

plot(y~x1)
plot(y~x2)

modelo1=lm(y~x1+x2)
summary(modelo1)
confint(modelo1)

###Segunda forma, con el valor esperado

# Planteamiento de esta forma: Y|X ~ N(B'X, SIGMA = 2)

y1=rnorm(20, mean=2+3*x1+4*x2, sd=2)

plot(y1~x1)
plot(y1~x2)

modelo2=lm(y1~x1+x2)
summary(modelo2)



####Simulacion para ver si beta1 es insesgado

#E(theta) = theta
#Sesgo = E(theta) - theta


startTime <- Sys.time()

#Simulo el modelo lineal y almaceno beta_1

almacenbeta1=rep(NA,40000)

for (i in 1:40000) {
  x1=rnorm(20, mean=10, sd=2)
  x2=rnorm(20, mean=13, sd=3)
  
  y=2+3*x1+4*x2+rnorm(20,mean=0,sd=2)
  
  modelo1=lm(y~x1+x2)
  almacenbeta1[i]=modelo1$coefficients[2]
  
}

mean(almacenbeta1)

endTime <- Sys.time()
(endTime - startTime)

almacenpotencia=rep(NA,10000)

for (i in 1:10000) {
  x1=rnorm(20, mean=10, sd=2)
  x2=rnorm(20, mean=13, sd=3)
  
  y=2+3*x1+4*x2+rnorm(20,mean=0,sd=2)
  
  modelo1=lm(y~x1+x2)
  almacenpotencia[i]=1*(summary(modelo1)$coef[2,4]<0.05)
  
}

mean(almacenpotencia)

###Otra vez pero con 100 corridas




#############################################################################

#Programación funcional 2

indep1 = rnorm(20, mean = 10, sd =2)
indep2 = rnorm(20, mean = 13, sd =3)
error = rnorm(20, mean = 0, sd = 2)
dep= 2+3*indep1+4*indep2+error

vars= array(cbind(dep, indep1, indep2), dim = c(20,3))
vars[,1]
vars[,2]
vars[,3]

pendiente1 = function(mat){
  objeto1 = lm(mat[,1]~mat[,2]+ mat[,3])
  objeto1$coefficients[2]
}


