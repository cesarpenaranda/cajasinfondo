#Simulacion del examen parcial 2

#3.	En los diseños para muestras pareadas se supone que la medición 1 (primera variable) tiene un cierto grado de correlación con la medición 2.  Adicionalmente, para contrastar la hipótesis nula de H0: µ1=µ2 en una muestra pareada, las dos técnicas no paramétricas son la prueba de Wilcoxon y la prueba de signos.  
#a.	En ciclos de 5000 corridas, genere 2 variables de tamaño 25 con distribución normal con media µ1=5 y µ2=6, y covarianza=0.5, y calcule la potencia de la prueba para los contrastes de Wilcoxon y de signos, con la H0: µ1=µ2, H1: µ1≠µ2 y H1*: µ1-µ2=1 (La H1* está implícita en los parámetros de la simulación).
#b.	Repita el mismo procedimiento con los mismos parámetros de la simulación, pero con tamaño de muestra igual a 200.
#c.	Llene el cuadro que se encuentra abajo y describa las diferencias en potencia entre ambas pruebas. (4 ptos).


##library(MultiRNG)

cuadro.resumen <- rbind(c(NA,NA),c(NA,NA))
muestras <- c(15,100)
for (j in 1:2) {
  almacen.wilcox <- rep(NA,5000)
  almacen.signos <- rep(NA,5000)
  
  for (i in 1:5000){
    
    mat.cor <- as.matrix(rbind(c(1,0.5),c(0.5,1)))
    vector.medias <- c(5,5.5)
    numeros <- draw.d.variate.normal(no.row=muestras[j],d=2,mean.vec=vector.medias,cov.mat=mat.cor) 
    x1 <- round(numeros[,1],2)
    x2 <- round(numeros[,2],2)
    wilcoxon <- suppressWarnings(wilcox.test(x1,x2,paired=TRUE))
    almacen.wilcox[i] <- 1*(wilcoxon$p.value<0.05)
    dif1 <- x1-x2
    dif=Recode(dif1,"-1000:-0.001=-1;0=NA;0.001:1000=1")
    tab.dif <- table(dif)
    signos<-binom.test(min(tab.dif),sum(tab.dif),p=0.5)
    almacen.signos[i] <- 1*(signos$p.value<0.05)
    
    
    
  }
  
  cuadro.resumen[j,1] <- mean(almacen.wilcox)
  cuadro.resumen[j,2] <- mean(almacen.signos)
  
}

rownames(cuadro.resumen) <- c("15","100")
colnames(cuadro.resumen) <- c("Wilcoxon","signos")
cuadro.resumen
