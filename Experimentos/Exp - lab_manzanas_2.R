control=1; tapar=2 ; plastica=3 ; limon=4

"En tercer lugar deben compararse los dos tratamientos en que se cubrió y
verificar si en realidad el promedio del tratamiento cuando se usó bolsa es
menor que cuando se tapó."

H0: mu2=mu3
mu2-mu3
v1=c(0,1,-1,0)



#Primer objetivo 
H0: mu_1 = 1/3(mu_2+mu_3+mu_4)
H0: tau_1=1/3(tau_2+tau_3+tau_4) 
H1: tau_1>1/3(tau_2+tau_3+tau_4)
#segundo objetivo
H0: 1/2(tau_2+tau_3)=tau_4 
H1: 1/2(tau_2+tau_3)>tau_4 
#tercer objetivos
H0:tau_2=tau_3
H1:tau_2>tau_3
#1. Preparación:
#(a) Cargue el archivo manzanas.Rdata.
load("manzanas.Rdata")
#2. Pruebas de hipótesis:
#(a) Cambie al modelo de suma nula. Obtenga las estimaciones de los
#coeficientes, para esto debe usar la función lm.
options(contrasts = c("contr.sum","contr.poly"))
contrasts(base$trat)

mod=lm(base$color~base$trat)
mod_aov=aov(mod)

anova(mod)
summary(mod_aov)
mod$coef

mean(base$color)
(m=tapply(base$color,base$trat,mean))
m-3.3

mean(m)
#(b) Verifique cuál es el tratamiento que está codificando con -1. Use
#contrasts(base$trat).
contrasts(base$trat)
"en este caso el tratamiento es limon"
#(c) Plantee las hipótesis necesarias para cumplir con los objetivos del
#problema.
#Primer objetivo
H0: mu_1 = 1/3(mu_2+mu_3+mu_4)
#Segundo objetivo 
H0: 1/2(tau_2+tau_3)=tau_4
#Tercer objertivo
H0:tau_2=tau_3
#(d) Explique por qué da lo mismo escribir estas hipótesis en términos de los
#efectos que en términos de los promedios de cada tratamiento.
"Esto es por cuestiones de notacion y sus despejes ya que con los tau puedo obtener los mu y viceversa
por lo que es lo mismo utilizar una u otra notacion"
#(e) Defina los contrastes ortogonales (L1, L2 y L3) necesarios para probar las
#hipótesis.
mu1=mu2

1/2(mu1+mu2)=mu4
#Para el primer objetivo
#mu_1-1/3(mu_2+mu_3+mu_4)
#mu_1-1/3(mu_2)-1/3(mu_3)-1/3(mu_4)

L1=c(1,-1/3,-1/3,-1/3)

#Para el segundo objetivo
#1/2(tau_2+tau_3)=tau_4
#1/2(tau_2+tau_3)-tau_4
#0*mu_1+1/2(tau_2)+1/2(tau_3)-tau_4

L2=c(0,1/2,1/2,-1)

#Para el tercer objertivo
#H0:tau_2=tau_3
#tau_2-tau_3

L3=c(0,1,-1,0)
#(f) Verifique que los tres vectores (v1, v2 y v3) son ortogonales. Para esto debe
#usar los coeficientes originales.
crossprod(L1,L2)
crossprod(L1,L3)
crossprod(L2,L3)

L1%*%L2
#(g) En el modelo con suma nula se tiene la restricción τ4 = −(τ1 + τ2 + τ3).
#Para que todo quede en términos de los 3 efectos que son estimados con
#este modelo, haga la sustitución de τ4 en L1.
#mu_1-1/3(mu_2+mu_3+mu_4)
#mu_1-1/3(mu_2)-1/3(mu_3)-1/3(mu_4)
#0mu1+tau_1-1/3(tau_2)-1/3(tau_3)+1/3tau1+1/3tau2+1/3tau3
#0mu1+4/3tau1
L1.2=c(0,4/3,0,0)

#Para el segundo objetivo
#1/2(tau_2+tau_3)=tau_4
#1/2(tau_2+tau_3)-tau_4
#0*mu_1+1/2(tau_2)+1/2(tau_3)+tau1+tau2+tau3
#0*mu+tau1+3/2tau2+3/2tau3
L2.2=c(0,1,3/2,3/2)

#Para el tercer objertivo
#H0:tau_2=tau_3
#0mu1+tau_2-tau_3

L3.2=c(0,0,1,-1)
#(h) Cree una matriz con los coeficientes de los contrastes. Recuerde que debe
#incluir el intercepto.

h1 = c(0,4/3,0,0)
h2 = c(0,1,3/2,3/2)
h3 = c(0,0,1,-1)
(h = cbind(h1,h2,h3))
#(i) Estime los contrastes.
(m = tapply(base$color,base$trat,mean))
(L=t(h)%*%mod$coefficients)

#(j) Verifique los resultados anteriores estimando el contraste basado en las
#medias estimadas.
L11=m[1]-mean(m[2:4])
L22=mean(m[2:3])-m[4]
L33=m[2]-m[3]
c(L11,L22,L33)
#(k) Encuentre el error estándar de cada contraste (ee1, ee2 y ee3).

(ee = sqrt(diag(t(h)%*%vcov(mod)%*%h)))

#(l) Encuentre el valor estandarizado del contraste haciendo: t = L_j/ee_j
(t=L/ee)
#(m) Encuentre la probabilidad de obtener un valor igual o mayor al estadístico
#usando la distribución t. Aunque sean varias pruebas simultáneas, no se
#necesita hacer ninguna corrección porque los contrastes son ortogonales.
#Use los grados de libertad de los residuales. De esta forma debe hacer:
#pt(t,36,lower.tail=F).
p=pt(t,36,lower.tail=F)
round(p,3)
p>0.05
#(n) ¿Qué se concluye en términos de las hipótesis que se probaron?
"Puesto que los 3 contrastes son ortogonales y se tienen hipótesis de una cola,
las probabilidades obtenidas se comparan directamente contra el α = 0,05. Las
primeras dos probabilidades son menores a 0,05, por lo que se rechazan la
primera y la segunda hipótesis, es decir, se encontró que la media de color es
mayor cuando no se aplica nada que en los otros casos; además que usar limón
ácido controla mejor que cubriendo. Se compararon los dos tratamientos en que
se cubre y no se demostró que usar bolsa sea mejor que tapar."

#3. Cota para la diferencia:
#(a) Obtenga una cota inferior para la diferencia de las medias sólo en los casos
#en que se encontró una diferencia significativa.
1-0.05/4
t = qt(0.95,36)
(lim = L[1:2]-t*ee[1:2])

#(b) ¿Qué se concluye en términos de los objetivos del estudio? 
"El promedio de color cuando no se aplica nada es al menos 2,2 puntos mayor
que en los otros tres casos en conjunto. Sabiendo que la escala va de 1 a 6, tener
2 puntos en promedio más es una cantidad importante, por lo que se nota que
aplicar alguno de estos tratamientos ayuda a mejorar el color.
Por otra parte, aplicar limón da un color promedio al menos 0,55 puntos
menor que cubrir. Esta diferencia no es tan grande como para afirmar que
definitivamente el limón esté produciendo una mejora con respecto a cubrir.
Aquí la persona experta debe dar su valoración de cuánto es una diferencia que
para él o ella sea relevante"
#4. Comparaciones no ortogonales. Ahora se van a hacer dos nuevas
#comparaciones adicionales suponiendo que el investigador tenía como
#objetivo solamente saber si cubrir (tapar o bolsa) da mejores resultados que
#no hacer nada y también si poner limón da mejores resultados que no hacer
#nada (control). En ambos casos, si hubiera diferencia, se debería cuantificar la
#mejoría.  
m
#objetivo 1 
mu_1=1/2(mu_2+mu_3)
#objetivo 2
mu_1=mu_4
#(a) Escriba las hipótesis y los contrastes asociados. Verifique que no son
#ortogonales.
H0:mu_1=1/2(mu_2+mu_3)
mu_1-1/2(mu_2+mu_3)
v1=c(1,-1/2,-1/2,0)

H0:mu_1=mu_4
mu_1-mu_4
v2=c(1,0,0,-1)

crossprod(v1,v2) #por lo tanto no son ortogonales

#ahora en funcion de lo coeficientes
H0:mu_1=1/2(mu_2+mu_3)
mu_1-1/2(mu_2+mu_3)

mu+tau1-1/2(tau2+tau3)
h1=c(0,1,-1/2,-1/2)

H0:mu_1=mu_4
mu_1-mu_4
mu+tau1-(tau4)
mu+tau1+tau1+tau2+tau3
mu+(2)tau1+tau2+tau3
h2=c(0,2,1,1)

(h2=cbind(h1,h2))
#(b) Encuentre las estimaciones de los contrastes, los errores estándar y el valor
#estandarizado del contraste.
#a
(L2=t(h2)%*%mod$coefficients)
(l1=m[1]-mean(m[2:3]))
(l2=m[1]-m[4])
#b
(ee2 = sqrt(diag(t(h2)%*%vcov(mod)%*%h2)))
#c
(t2=L2/ee2)

#(c) Encuentre la probabilidad de obtener un valor igual o mayor al estadístico
#usando la distribución t. Como se trata de dos pruebas simultáneas con
#contrastes no ortogonales, debe hacer la corrección de Bonferroni. Esta
#consiste en dividir el nivel de significancia (α) por el número de pruebas
#que se realizan (d), entonces la probabilidad asociada a cada prueba se
#compara contra α/d.
"Como se tienen 2 contrastes entonces d = 2 por lo que las probabilidades
obtenidas se deben comparar contra α/2 = 0,025. En ambos casos la
probabilidad obtenida es menor a 0,025, por lo que se rechazan ambas hipótesis."

p=pt(t2,36,lower.tail=F)
p>0.05
#(d) Cuantifique las diferencias en los casos en que se detectaron
"Se tienen que calcular 2 cotas inferiores (d = 2), y se recurre a la corrección de
Bonferroni usando el cuantil de la distribución t para 1−α/d, con 36 grados de
libertad."
qt=qt(1-0.05/2,36)
(lim=L2-qt*ee2)

#(e) ¿Qué se concluye?

"Se espera con una confianza global de 95%, que el promedio de color cuando
se cubre sea menor al menos 1,62 puntos que cuando no se hace nada, y
similarmente cuando se usa limón sea al menos 2,70 puntos menor que cuando
no se hace nada"
    