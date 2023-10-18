library(car)
attach(rios)
names(rios)
#Estime con R un modelo lineal en que se prediga el valor del pH en función de 
#la alcalinidad, el caudal y si el punto de recolección está río arriba o río abajo
anova.rios=lm(ph~alcalinidad+caudal+rioarriba)
summary(anova.rios)

qqPlot(anova.rios$residuals)
hist(anova.rios$residuals)


local.factor=as.factor(c(rep(0,14),rep(1,11)))
ph=c(6.85,
  6.96,
  6.98,
  7.06,
  7.19,
  7.32,
  7.4,
  7.68,
  7.72,
  7.9,
  7.9,
  7.96,
  8.12,
  8.97,
  6.83,
  7.12,
  7.14,
  7.37,
  7.51,
  7.53,
  7.56,
  7.99,
  8.02,
  8.22,
  8.31)
df=data.frame(ph,local.factor)
df

etiquetas=c("rio abajo","rio arriba")

boxplot(
  ph~local.factor, 
  names=etiquetas
)

leveneTest(ph,local.factor)
t.test(ph[rioarriba==1],ph[rioarriba==0],alternative = "two.sided",mu=0,var.equal = T)

#prueba fisher 
modalidad = c(250, 250, 350, 150)
modalidad_matriz = matrix(modalidad, nrow = 2, ncol = 2, byrow = T)
fisher_modalidad = fisher.test(modalidad_matriz)
fisher_modalidad

