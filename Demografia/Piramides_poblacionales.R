load("pyramid.Rdata")
#install.packages("pyramid")
library(pyramid)

piramide$saraphp=(piramide$saraph/(sum(piramide$saraptotal)))*100
piramide$sarapmp=(piramide$sarapm/(sum(piramide$saraptotal)))*100

pyramids(piramide$saraphp,piramide$sarapmp,Center=piramide$edad,
         Llab="Hombres",Rlab="Mujeres",
         main="titulo",Clab="edad",
         Laxis=c(0,0.25,0.5,1,1.25,1.53),Cstep=5,Cgap=0.1)
