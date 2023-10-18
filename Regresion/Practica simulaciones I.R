#1.	Suponga que tiene dos variables independientes con distribución normal X1 y X2
#con medias MU1=1 y sigma1=1, y Mu2=1 y sigma2=2, y tamaño de muestra igual a 30. 
#Además, cree una variable y=2+1*X1+2*X2+error, donde el error se distribuye normal 
#con media=0 y desviación estándar=3.  En estas condiciones, calcule la potencia del
#ANDEVA global de regresión.  

#potencia= 1-B
#=1-P(error tipoII)
#=1-P(no rechazar H0|H0 falso )
#=1-P(No rechazan Ho|H1*)
#=P(Rechazar H0|H1*)


#X_1~N(mu_1=1,sigma_1=1)
#X_2~N(mu_2=1,sigma_2=1)
#E~N(0,sigma=3)


#E(Y|X_1,X_2)=2+1*X_1+2*X_2+E

#H0: B_1=B_2=0
#H1: almenos un B_1 o B_2 =/ 0
#H1*: B_1=1 y B_2=2






almacen.potencia= rep(NA,1000)


for (i in 1:1000){
  X1=rnorm(30,mean=1,sd=1)
  X2=rnorm(30,mean=1,sd=2)
  Y=2+1*X1+2*X2+rnorm(30,mean=0,sd=3)
  
  mod=lm(Y~X1+X2)
  fvector=summary(mod)$fstatistic
  pvalue.f=1- pf(fvector[1],df1 = fvector[2],df2 = fvector[3])
  
  #mod.nulo= lm(Y~1)
  ##objeto.anova= anova(mod.nulo,mod)
  ##pvalue.f= 1*(objeto.anova$'Pr(F>)'[2],0.05)
  
  almacen.potencia[i]=1*(pvalue.f<0.05)
}

mean(almacen.potencia)
summary(mod)$coefficients[2,4]

#2.	Suponga que tiene dos variables independientes con distribución normal X1 y X2 
#con medias MU1 y Mu2, y desviaciones estándar sigma1 y sigma2, y tamaño de muestra n.
#Fije Mu1=5 y sigma1=1 y sigma2=1.  Además, cree una variable y=2+1*X1+2*X2+error, donde 
#el error se distribuye normal con media=0 y desviación estándar=10.  Calcule las 
#potencias de la prueba para la H0: Beta1=Beta2, para las siguientes combinaciones
#de n={10,20,30,40,50,60,70,80,90,100} y de Mu2={5,6,7,8,9,10}.  Haga un gráfico en el
#que muestre las potencias para los distintos niveles de n y Mu2, y diga para qué combinaciones 
#se llega a una potencia de 80%.  Use un alfa=0.05.

#H0: B_1=B_2
#H1: B_1=/B_2
#H1*: B_1=2 y B_2=2



matriz.potencia= matrix(rep(NA,60),nrow=6)
matriz.potencia

for(k in 1:10){
  n=k*10
  for(j in 1:6){
    almacen.rechazo=rep(NA,200)
    mu2=j+4
    
    for (i in 1:200){
      x1=rnorm(n,mean=5,sd=1)
      x2=rnorm(n,mean=mu2,sd=1)
      y=2+1*x1+2*x2+rnorm(n,mean=0,sd=10)
      
      mod.no.iguales=lm(y~x1+x2)
      mod.iguales=lm(y~I(x1+x2))
      
      #anova(mod.iguales,mod.no.iguales)
      pvalue.anova=anova(mod.iguales,mod.no.iguales)[2,6]
      almacen.rechazo[i]=1*(pvalue.anova<0.05)
      
      matriz.potencia[j,k]=mean(almacen.rechazo)

    }
  }
}

matriz.potencia
rownames(matriz.potencia)=seq(5:10)
colnames(matriz.potencia)=seq(10,100,10)
matriz.potencia





#3.	En modelos de tres variables predictoras, cree una función que calcule las 
#Sumas de Cuadrados Marginales para la variable Xi, controlando por Xj y Xk, y evalúe
#esa función en los datos sobre la fuerza laboral de la práctica 1.


#4. El CME es un estimador insesgado de la variancia única de los errores.  El estimador
#del CME es:  CME=SCE/(n-p)=(∑▒(x_i-x ̅ )^2 )/(n-p), por lo que es un promedio de los
#errores al cuadrado.  Se dice también que el estimador SCE/n=(∑▒(x_i-x ̅ )^2 )/n es
#la media aritmética de los errores al cuadrado, pero es un estimador sesgado de la
#variancia de los errores.  Simule 5000 veces un modelo de regresión simple con Y=2+5*X1+error, 
#donde el error tenga una variancia de 9, y calcule los dos estimadores, el sesgado y el insesgado,
#y compare la Esperanza matemática de estos estadísticos con el valor poblacional.  Usen tamaño de
#muestra de 20.  Conteste: ¿Se refleja el sesgo de ambos estimadores en la simulación?


almacen1=rep (NULL) #Insesgado
almacen2=rep (NULL) #Sesgado
almacen3=rep (NULL) #Insesgado
almacen4=rep(NULL)  #Sesgado


for (i in 1:5000) {
  var.errores=9
  x1=rnorm(20,mean=5,sd=2)
  y=2+5*x1+rnorm(20,mean=0,sd=(var.errores)^.5)
  
   mod=lm(y~-x1)
   
###Forma 1
y.techito=predict(mod)
residuo=y-y.techito

numerador=sum((residuo)^2)
insesgado=numerador/18
sesgado=numerador/20

almacen1 [i]=insesgado
almacen2[i]=sesgado

###Forma 2
#numerador2=anova(mod)[2,2]

#insesgado2=anova(mod)[2,3]
#sesgado2=numerador2/20

#almacen3[i]=insesgado2
#almacen4[i]=sesgado2

}
mean (almacen1)
mean (almacen2)
#mean (almacen3)
#mean (almacen4)


#Valor poblacional es 9



