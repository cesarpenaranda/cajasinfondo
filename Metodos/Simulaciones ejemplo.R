install.packages("moments") 
library(moments)

k = 0.88
coef_var = function(x) {
  sd(x) / mean(x)
}
coef_asimetria = function(x){
  skewness(x)
}
kurto = function(x){
  kurtosis(x)
}


#H0: Mu = 0.88
#H1(1): Mu = 1
#H1(2): Mu = 0.80
#H1(3): Mu = 0.95
#H1(4): Mu = 1.64
#H1(5): Mu = 0.59
#H1(6): Mu = 1.19

dist2 = rnorm(16, mean = k, sd = k )
dist3 = rnorm(26, mean = k, sd = k )
dist4 = rnorm(42, mean = k, sd = k )
dist5 = rnorm(67, mean = k, sd = k )
dist6 = rnorm(107, mean = k, sd = k )
dist7 = rnorm(171, mean = k, sd = k )
dist8 = rnorm(274, mean = k, sd = k )
dist9 = rnorm(438, mean = k, sd = k )
dist10 = rnorm(500, mean = k, sd = k )


alm.tt1 = rep(NA, 10000)
alm.ks1 = rep(NA, 10000)

#H0: Mu = 0.88
#H1(1): Mu = 1
for (i in 1:10000) {
  
  dist1 = rnorm(10, mean = k, sd = k)
  ost = t.test(dist1, mu = 0.88)
  kst = ks.test(dist1, "pnorm")
  
  alm.tt1[i] = ost$statistic
  alm.ks1[i] = kst$statistic
}

coef_var(x = alm.tt1)
coef_var(x = alm.ks1)

coef_asimetria(x = alm.tt1)
coef_asimetria(x = alm.ks1)

kurto(x = alm.tt1)
kurto(x = alm.ks1)

