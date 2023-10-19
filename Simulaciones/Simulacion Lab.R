
library(car)


base = subset(base12, select = c(mor.acci,ips,dih))
attach(base)

mod = lm(mor.acci~ips+dih)

ee = summary(mod)$sigma

error1 = rnorm(81, 0 , ee)

error2 = rt(81, 2 )
error2 = scale(error2)*ee

error3  = rcauchy(81, 0 , 1)
error3 = scale(error3)*ee

error4 = runif(81,-10, 10)
error4 = scale(error4)*ee

error5 = rgamma(81,2,4)
error5 = scale(error5)*ee

error6 = rgamma(81,1,7)
error6 = scale(error6)*ee

error7 = rgamma(81,9,0.5)
error7 = scale(error7)*ee

error8 = rgamma(81,5,9)
error8 = scale(error8)*ee

summary(mod)

y1 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error1
y2 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error2
y3 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error3
y4 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error4

y5 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error5
y6 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error6
y7 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error7
y8 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error8

mod1 = lm(y1~ips+dih)
mod2 = lm(y2~ips+dih)
mod3 = lm(y3~ips+dih)
mod4 = lm(y4~ips+dih)
mod5 = lm(y5~ips+dih)
mod6 = lm(y6~ips+dih)
mod7 = lm(y7~ips+dih)
mod8 = lm(y8~ips+dih)

almacen.pvalues = matrix(rep(NA, 1000),nrow=1000,ncol=8)
almacen.rechazo = matrix(rep(NA, 1000),nrow=1000,ncol=8)


for (i in 1:1000) {
    error1 = rnorm(81, 0 , ee)
    
    error2 = rt(81, 2 )
    error2 = scale(error2)*ee
    
    error3  = rcauchy(81, 0 , 1)
    error3 = scale(error3)*ee
    
    error4 = runif(81,-10, 10)
    error4 = scale(error4)*ee
    
    error5 = rgamma(81,2,4)
    error5 = scale(error5)*ee
    
    error6 = rgamma(81,1,7)
    error6 = scale(error6)*ee
    
    error7 = rgamma(81,9,0.5)
    error7 = scale(error7)*ee
    
    error8 = rgamma(81,5,9)
    error8 = scale(error8)*ee
    
    y1 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error1
    y2 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error2
    y3 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error3
    y4 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error4
    
    y5 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error5
    y6 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error6
    y7 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error7
    y8 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error8
    
    mod1 = lm(y1~ips+dih)
    mod2 = lm(y2~ips+dih)
    mod3 = lm(y3~ips+dih)
    mod4 = lm(y4~ips+dih)
    mod5 = lm(y5~ips+dih)
    mod6 = lm(y6~ips+dih)
    mod7 = lm(y7~ips+dih)
    mod8 = lm(y8~ips+dih)
    
    almacen.pvalues[i,1] = summary(mod1)$coefficients[2,4] < 0.05
    almacen.pvalues[i,2] =summary(mod2)$coefficients[2,4] < 0.05
    almacen.pvalues[i,3] = summary(mod3)$coefficients[2,4] < 0.05
    almacen.pvalues[i,4] = summary(mod4)$coefficients[2,4] < 0.05
    almacen.pvalues[i,5] = summary(mod5)$coefficients[2,4] < 0.05
    almacen.pvalues[i,6] = summary(mod6)$coefficients[2,4] < 0.05
    almacen.pvalues[i,7] = summary(mod7)$coefficients[2,4] < 0.05
    almacen.pvalues[i,8] = summary(mod8)$coefficients[2,4] < 0.05
  
    
}

potencia  = matrix(nrow = 1 , ncol= 8)

for (i in 1:8){
  potencia[1,i] = mean(almacen.pvalues[,i])
}

potencia

almacen.conf = matrix(rep(NA,1000),nrow = 1000,ncol = 8)


for (i in 1:1000) {
  error1 = rnorm(81, 0 , ee)
  
  error2 = rt(81, 2 )
  error2 = scale(error2)*ee
  
  error3  = rcauchy(81, 0 , 1)
  error3 = scale(error3)*ee
  
  error4 = runif(81,-10, 10)
  error4 = scale(error4)*ee
  
  error5 = rgamma(81,2,4)
  error5 = scale(error5)*ee
  
  error6 = rgamma(81,1,7)
  error6 = scale(error6)*ee
  
  error7 = rgamma(81,9,0.5)
  error7 = scale(error7)*ee
  
  error8 = rgamma(81,5,9)
  error8 = scale(error8)*ee
  
  y1 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error1
  y2 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error2
  y3 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error3
  y4 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error4
  
  y5 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error5
  y6 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error6
  y7 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error7
  y8 = 144.1889-1.4809*base[,2]-31.1540*base[,3]+error8
  
  mod1 = lm(y1~ips+dih)
  mod2 = lm(y2~ips+dih)
  mod3 = lm(y3~ips+dih)
  mod4 = lm(y4~ips+dih)
  mod5 = lm(y5~ips+dih)
  mod6 = lm(y6~ips+dih)
  mod7 = lm(y7~ips+dih)
  mod8 = lm(y8~ips+dih)
  
   lim.inf1 = confint(mod1)[2,1]
   lim.sup1 = confint(mod1)[2,2]
   
   lim.inf2 = confint(mod2)[2,1]
   lim.sup2 = confint(mod2)[2,2]
   
   lim.inf3 = confint(mod3)[2,1]
   lim.sup3 = confint(mod3)[2,2]
   
   lim.inf4 = confint(mod4)[2,1]
   lim.sup4 = confint(mod4)[2,2]
   
   lim.inf5 = confint(mod5)[2,1]
   lim.sup5 = confint(mod5)[2,2]
   
   lim.inf6 = confint(mod6)[2,1]
   lim.sup6 = confint(mod6)[2,2]
   
   lim.inf7 = confint(mod7)[2,1]
   lim.sup7 = confint(mod7)[2,2]
   
   lim.inf8 = confint(mod8)[2,1]
   lim.sup8 = confint(mod8)[2,2]
  
   almacen.conf[i,1] = ifelse(lim.inf1<(-1.4809) & lim.sup1>(-1.4809),1,0)
   almacen.conf[i,2] = ifelse(lim.inf2<(-1.4809) & lim.sup2>(-1.4809),1,0)
   almacen.conf[i,3] = ifelse(lim.inf3<(-1.4809) & lim.sup3>(-1.4809),1,0)
   almacen.conf[i,4] = ifelse(lim.inf4<(-1.4809) & lim.sup4>(-1.4809),1,0)
   almacen.conf[i,5] = ifelse(lim.inf5<(-1.4809) & lim.sup5>(-1.4809),1,0)
   almacen.conf[i,6] = ifelse(lim.inf6<(-1.4809) & lim.sup6>(-1.4809),1,0)
   almacen.conf[i,7] = ifelse(lim.inf7<(-1.4809) & lim.sup7>(-1.4809),1,0)
   almacen.conf[i,8] = ifelse(lim.inf8<(-1.4809) & lim.sup8>(-1.4809),1,0)
}

conf  = matrix(nrow = 1 , ncol= 8)

for (i in 1:8){
  conf[1,i] = mean(almacen.conf[,i])
}

conf
potencia
