#libreria necesaria para los coeficientes 
library(moments)


almacen <- rbind(c(NA,NA), c(NA, NA))

#Funciones
coef_var = function(x) {
  sd(x) / mean(x) #Para el coeficiente de variacion
}
coef_asimetria = function(x){
  skewness(x) #Coeficiente de asimetria
}
kurto = function(x){
  kurtosis(x) #Para la kurtosis 
}

#Matrices
matriz.corr1 <- as.matrix(rbind(c(1,0.05),c(0.05,1)))

matriz.corr2<- as.matrix(rbind(c(1,0.15),c(0.15,1)))

matriz.corr3 <- as.matrix(rbind(c(1,0.30),c(0.30,1)))

matriz.corr4 <- as.matrix(rbind(c(1,0.55),c(0.55,1)))

matriz.corr5 <- as.matrix(rbind(c(1,0.70),c(0.70,1)))

matriz.corr6 <- as.matrix(rbind(c(1,0.95),c(0.95,1)))

#Tamaños de muestra

n <- c(20, 75, 150, 215, 300, 445, 550, 670, 800, 1000)
D <- 0.67


medias <- c(0,0)

for(j in 1:10){
  almacen_wilcox <- rep(NA,999)
  
  for(i in 1:999){
    numeros <- draw.d.variate.normal(
      no.row = n[j],
      d=2, 
      mean.vec = medias,
      cov.mat = matriz.corr1
      )
    
    var.norm.1 <- round(numeros[,1],2)
    var.norm.2 <- round(numeros[,2],2)
    prueba.wilcox <- suppressWarnings(
      wilcox.test(
        var.norm.1,
        var.norm.2,
        paired=TRUE)
      )
    almacen_wilcox[i] <- 1*(prueba.wilcox$p.value<0.05)
    
  }
}
coef_var(var.norm.1)
coef_var(var.norm.2)

coef_asimetria(var.norm.1)
coef_asimetria(var.norm.2)

kurto(var.norm.1)
kurto(var.norm.2)



