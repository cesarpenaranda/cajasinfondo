save(Base_Multi,file = "basemulti.Rdata")
df=subset(Base_Multi,select = -c(EDAD,SEXO,AREA,ANIOCARR,INTEXTR,INDGRUP,RECURADI,SOLPROF,TODOPAS))
df=scale(df,scale = T) #Estandarizacion
df2=subset(Base_Multi,select = c(INDGRUP,RECURADI,SOLPROF,TODOPAS))
df2$SOLPROF=ifelse(df2$SOLPROF==1,1,0)
df2$TODOPAS=ifelse(df2$TODOPAS==1,1,0)
df=cbind(df,df2)
apply(df,2,var)

df$INDGRUP=as.factor(df$INDGRUP)
df$RECURADI=as.factor(df$RECURADI)
df$SOLPROF=as.factor(df$SOLPROF)
df$TODOPAS=as.factor(df$TODOPAS)

#Extremos
leverage=hat(df)
lim=2*mean(leverage)
plot(leverage)
abline(h=lim,col="red")


df=df[-90,]
id_filas <- which(hat(df) > lim)

#Distancia
d <- daisy(df, metric = "gower")
dist


CERCANO= hclust(d,method = "single")
LEJANO= hclust(d,method = "complete")
PROMEDIO=hclust(d,method = "average")
WARD= hclust(d,method = "ward.D")


#Dendogramas
par(mfrow=c(2, 2))
plot(CERCANO, xlab="", main="DISTANCIA single/DISTANCIA gower", ylab="Distancia", sub="")
plot(LEJANO, xlab="", main="DISTANCIA complete/DISTANCIA gower", ylab="Distancia", sub="")
plot(PROMEDIO, xlab="", main="DISTANCIA average/DISTANCIA gower", ylab="Distancia", sub="")
plot(WARD, xlab="", main="DISTANCIA ward.D/DISTANCIA gower", ylab="Distancia", sub="")

#SDG
SCDG1 = c()
for (k in 1:8) SCDG1[k] = kmeans(df, centers = k)$tot.withinss
plot(SCDG1,type = "b",xlab = "numero de grupos")

#Evaluacion 
library(factoextra)
fviz_nbclust(df,kmeans,method = "wss",diss = d)


#Grupos
km = kmeans(df, centers = 3)
kmedoids = pam(df, 3)

table(km$cluster,kmedoids$clustering)

#Complete
cs= hclust(d,method = "complete")
clust.single = cutree(cs, k=3)
table(clust.single)
plot(cs, xlab="", main="DISTANCIA complete/DISTANCIA gower", ylab="Distancia", sub="")

#ward.D
cs= hclust(d,method = "ward.D")
clust.single = cutree(cs, k=3)
table(clust.single)
plot(cs, xlab="", main="DISTANCIA ward.D/DISTANCIA gower", ylab="Distancia", sub="")

#numero de grupos
fviz_nbclust(df,kmeans,method = "silhouette",diss = d)

fviz_nbclust(df,kmeans,method = "gap",diss = d) #gap alto es bueno
