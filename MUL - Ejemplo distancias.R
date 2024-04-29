#Matrices de trabajo
MA= matrix(c(-3,-2,-5),ncol = 1)
MB= matrix(c(6.632,6.638,3,6.368,12.526,3.579,3,3.579,5.945),nrow = 3)

#Euclidea 
sqrt(t(MA)%*%MA)

#Manhatan
sum(abs(MA))

#Mahalanobis
A= t(MA)%*%solve(MB)%*%MA
sqrt(A)
#2.1436

