load("MUL - datos_finales.Rdata")
datos_finales=MUL_datos_finales
library(dplyr)


datos_finales <- datos_finales %>%
  rename(y = `RR+`)  

datos_finales$y = as.factor(datos_finales$y)
datos_finales <- na.omit(datos_finales)


RNGkind(sample.kind = "Rounding")
set.seed(22122000)
n=nrow(datos_finales)
m=sample(1:n, n*0.8)
train = datos_finales[m,]
train = train[,c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)]
test=datos_finales[-m,]
test=test[,c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)]



formula= y ~ Tmax_max + Tmax_mean+Tmax_min+Tmin_max+Tmin_mean+Tmin_min + n_Tmax_Q3+ n_Tmin_Q1+amplitude_max_max+amplitude_max_mean+amplitude_max_min+amplitude_min_max+amplitude_min_mean+amplitude_min_min+n_amplitude_P90+n_amplitude_Q3+precip_max_max+precip_max_mean+precip_max_min+precip_mean_mean


#MULTINOMIAL
library(nnet)
mod1=multinom(formula, train)

      #Seleccion de umbral
    prob <- predict(mod1, test, type="prob")      
    probs <- c(seq(0.05,1,0.05))
    E=matrix(NA, nrow=20, ncol = 5)
    for(k in 1:20){
      pred <- ifelse(prob > probs[k], 1, 0)
      E[k,]=eval(test$y, as.numeric(pred))$indicadores
      print(k)
    }
    colnames(E)=c("e","FP", "FN","AUC","KS")
    rownames(E) = probs
    
    
    

pred=predict(mod1,test )
table(test$y, pred)
eval(test$y, as.numeric(pred))

#MULTINOMIAL STEP
mod2=step(mod1)
eval(test$y, as.numeric(predict(mod2, test)))


#KNN
library(caret)
library(DT)
library(ROCR)
library(class)
library(kknn)
library(e1071)
library(plotly)
library(adabag)
library(randomForest)

datos_est <- data.frame(scale(datos_finales[,-c(1,2,3,4,5,6,7)]))
datos_est$y <- datos_finales$y
trainknn = datos_est[m,]
testknn = datos_est[-m,]

#Euclidea
E=matrix(NA, nrow=15, ncol = 5)
for(k in 1:15){
  pred=knn(trainknn[,-23],testknn[,-23],trainknn$y, k=k)
  E[k,]=eval(test$y, as.numeric(pred))$indicadores
}
colnames(E)=c("e","FP", "FN","AUC","KS")




pred2=knn(trainknn[,-23], testknn[,-23],trainknn$y, k=9)
eval(testknn$y, as.numeric(pred2))

#Arboles
library(rpart)
library(rattle)
library(DT)
library(adabag)
library(randomForest)

mod3=rpart(y~., method="class", data=train)
pred3=predict(mod3, newdata = test, type="class")
eval(test$y, as.numeric(pred3))


##Seleccion de cp

E=matrix(NA, nrow=10, ncol = 5)
cp <- c(0.0005,0.001,0.0020,0.0040,0.0080,0.0160,0.0320,0.0640,0.01,0.02)
for(k in 1:10){
  mod4=rpart(y~., method="class", data=train, minsplit=560, minbucket=235, cp=cp[k])
  pred=predict(mod4, newdata = test, type="class")
  E[k,]=eval(test$y, as.numeric(pred))$indicadores
}
colnames(E)=c("e","FP", "FN","AUC","KS")
rownames(E) = cp
 #SELECCIONO 0.0005

#SELECCION DE MAXDEPTH
E=matrix(NA, nrow=15, ncol = 5)
for(k in 1:15){
  mod4=rpart(y~., method="class", data=train, minsplit=560, minbucket=235, cp=0.0005, maxdepth=k)
  pred=predict(mod4, newdata = test, type="class")
  E[k,]=eval(test$y, as.numeric(pred))$indicadores
}
colnames(E)=c("e","FP", "FN","AUC","KS")

  #selecciono 9

mod4=rpart(y~., method="class", data=train, minsplit=560, minbucket=235, cp=0.0005, maxdepth=9)
pred4=predict(mod4, newdata = test, type="class")
eval(test$y, as.numeric(pred4))



#Bagging
mod5=bagging(y~., method="class",data=train, mfinal=10)
pred5=predict(mod5, test)$class

#RandomForest
mod6=randomForest(y~.,method="class",data=train,ntree=500)
pred6=predict(mod6, test)
table(test$y, pred6)

  #sELECCION DE variables aleatorias
    mtry=c(seq(1,22,1))
    E=matrix(NA, nrow=22, ncol = 5)
    for(k in 1:22){
      mod6=randomForest(y~.,method="class",data=train,ntree=500, mtry=mtry[k])
      pred=predict(mod6, test)
      E[k,]=eval(test$y, as.numeric(pred))$indicadores
      print(k)
    }
    colnames(E)=c("e","FP", "FN","AUC","KS")
    #NO mucha diferencia, me quedo con default
    
    
  #NUmero de arboles
    ntree=c(seq(100,1000,100))
    E=matrix(NA, nrow=10, ncol = 5)
    for(k in 1:10){
      mod6=randomForest(y~.,method="class",data=train,ntree= ntree[k])
      pred=predict(mod6, test)
      E[k,]=eval(test$y, as.numeric(pred))$indicadores
      print(k)
    }
    colnames(E)=c("e","FP", "FN","AUC","KS")
    
    #Me quedo con 100
    mod6=randomForest(y~.,method="class",data=train,ntree= 100)
    pred=predict(mod6, test)
    eval(test$y,as.numeric(pred))    

#Boosting
mod7 = boosting(y~., train, boos = TRUE, mfinal = 25)
pred7 = predict(mod7, test)$class




#VALIDACIÓN CRUZADA

library(caret)
datos_finales <- datos_finales[,c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)]
cortes=createFolds(1:nrow(datos_finales))
res=matrix(nrow=10,ncol=5)
colnames(res)=c("e","FN","FP","AUC","KS")

#Multinomial
for(i in 1:10){
  train=datos_finales[-cortes[[i]],]
  test=datos_finales[cortes[[i]],]
  mod1=multinom(formula, train)
  prob <- predict(mod1, test, type="prob")      
  pred <- ifelse(prob > 0.45, 1, 0)
  res[i,]<-eval(test$y, as.numeric(pred))$indicadores
  
}

mediares<-apply(res, 2, mean)


#KNN
res=matrix(nrow=10,ncol=5)
colnames(res)=c("e","FN","FP","AUC","KS")


for(i in 1:10){
  train=datos_est[-cortes[[i]],]
  test=datos_est[cortes[[i]],]
  pred=knn(train[,-23], test[,-23],train$y, k=9)
  res[i,]<-eval(test$y, as.numeric(pred))$indicadores
}

mediares<-apply(res, 2, mean)

#Arboles
datos_finales <- datos_finales[,c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)]
res=matrix(nrow=10,ncol=5)
colnames(res)=c("e","FN","FP","AUC","KS")

for(i in 1:10){
  train=datos_finales[-cortes[[i]],]
  test=datos_finales[cortes[[i]],]
  mod=rpart(y~., method="class", data=train, minsplit=560, minbucket=235, cp=0.0005, maxdepth=9)
  pred=predict(mod, newdata = test, type="class")
  res[i,]<-eval(test$y, as.numeric(pred))$indicadores
  
}

mediares<-apply(res, 2, mean)


#Bosques aleatorios


res=matrix(nrow=10,ncol=5)
colnames(res)=c("e","FN","FP","AUC","KS")

for(i in 1:10){
  train=datos_finales[-cortes[[i]],]
  test=datos_finales[cortes[[i]],]
  mod=randomForest(y~.,method="class",data=train,ntree= 100)
  pred=predict(mod, test)
  res[i,]<-eval(test$y, as.numeric(pred))$indicadores
  
}

mediares<-apply(res, 2, mean)
