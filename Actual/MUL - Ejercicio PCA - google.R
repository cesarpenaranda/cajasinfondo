#https://github.com/cosmoduende/r-marvel-vs-dc


respca<-prcomp(charactersStats, scale = TRUE)

library(textshape)

base_datos<- charactersStats[!duplicated(charactersStats$Name), ]
base_datos<-textshape::column_to_rownames(base_datos, loc = 1)

df<-as.data.frame(base_datos)
df <- subset(df, select = -c(Alignment,Total) )
View(df)

library(stats)
#prcomp() Forma rápida de implementar PCA sobre una matriz de datos.
respca<-prcomp(df, scale = TRUE)

names(respca)

head(respca$rotation)[, 1:5] #las coordenadas de los datos en el nuevo sistema rotado de coordenadas. 
#Estas coordenadas se corresponden con los scores de los componentes principales.

dim(respca$rotation) #Número de distintos componentes

head(respca$x)[,1:5] #los vectores de los scores.

respca$sdev #las desviaciones estándares de cada CP.

respca$sdev^2  ## Varianza explicada por cada componente

summary(respca)

#comprobemos la importancia del componente 1
xx<-respca$x
xx<-as.data.frame(xx)
base_datos$PC1<-xx$PC1
base_datos$PC2<-xx$PC2
base_datoscor <- subset(base_datos, select = -c(Alignment))
head(base_datoscor)
round(cor(base_datoscor),4)
#otra función
#princomp() 

respca1 <- princomp(df, cor = TRUE)

#respca1 <- princomp(~ Speed + Power,
#                  data = df, na.action = na.exclude, cor = TRUE)

names(respca1)

respca1$sdev

summary(respca1)

library(FactoMineR)

#PCA() #PCA con resultados más detallados. Los valores ausentes se reemplazan por la media de cada columna.
#Pueden incluirse variables categóricas suplementarias. Estandariza automáticamente los datos.


respca2 <- PCA(X = df, scale.unit = FALSE, ncp = 6, graph = TRUE)

print(respca2)


head(respca2$eig) #como ejemplo


library(factoextra)

get_pca(respca2) #Extrae la información sobre las variables.

get_pca_var(respca2) #Extrae la información sobre las variables.

get_pca_ind(respca2) #Extrae la información sobre las observaciones.


#visualización

fviz_eig(respca2) #visualizar eigenvalores (scree plot)

fviz_screeplot(respca2) #visualizar eigenvalores (scree plot)

fviz_pca_ind(respca2) #Representación de observaciones sobre componentes principales.

fviz_pca_ind(respca2,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)

fviz_pca_var(respca2) #Representación de variables sobre componentes principales.

fviz_pca_var(respca2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(respca2, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)
#mil ejemplos de como visualizar aquí:
#http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining

fviz_contrib(respca2,choice = "var") #Representa la contribución de filas/columnas de los resultados de un pca.
fviz_contrib(respca2,choice = "ind")

biplot(x = respca1, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

fviz_pca_biplot(respca1, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

fviz_pca_biplot(respca1, repel = TRUE,
                #geom= "point", #o text o geom=c("point", "text")
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969", # Individuals color
                select.ind = list(contrib = 30)
)

grupo<-as.factor(base_datos$Alignment)

#inistalable en las versiones de R ahora. 
#remotes::install_github("vqv/ggbiplot", force = TRUE) #ggplot no puede estar en uso.
#library(ggbiplot)

#función contraida
ggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                     obs.scale = 1 - scale, var.scale = scale, 
                     groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                     labels = NULL, labels.size = 3, alpha = 1, 
                     var.axes = TRUE, 
                     circle = FALSE, circle.prob = 0.69, 
                     varname.size = 3, varname.adjust = 1.5, 
                     varname.abbrev = FALSE, ...)
{
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  
  stopifnot(length(choices) == 2)
  
  # Recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
  } else if(inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  } else {
    stop('Expected a object of class prcomp, princomp, PCA, or lda')
  }
  
  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))
  
  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])
  
  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)
  
  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  
  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  
  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }
  
  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  
  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }
  
  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }
  
  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    df.v$varname <- rownames(v)
  }
  
  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal()
  
  if(var.axes) {
    # Draw circle
    if(circle) 
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, color = muted('white'), 
                         size = 1/2, alpha = 1/3)
    }
    
    # Draw directions
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(1/2, 'picas')), 
                   color = muted('red'))
  }
  
  # Draw either labels or points
  if(!is.null(df.u$labels)) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    } else {
      g <- g + geom_text(aes(label = labels), size = labels.size)      
    }
  } else {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    } else {
      g <- g + geom_point(alpha = alpha)      
    }
  }
  
  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    
    ell <- ddply(df.u, 'groups', function(x) {
      if(nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
                 groups = x$groups[1])
    })
    names(ell)[1:2] <- c('xvar', 'yvar')
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  
  # Label the variable axes
  if(var.axes) {
    g <- g + 
      geom_text(data = df.v, 
                aes(label = varname, x = xvar, y = yvar, 
                    angle = angle, hjust = hjust), 
                color = 'darkred', size = varname.size)
  }
  # Change the name of the legend for groups
  # if(!is.null(groups)) {
  #   g <- g + scale_color_brewer(name = deparse(substitute(groups)), 
  #                               palette = 'Dark2')
  # }
  
  # TODO: Add a second set of axes
  
  return(g)
}

ggbiplot(respca2)

ggbiplot(respca2,ellipse=TRUE,  labels=rownames(df), groups=grupo)

ggbiplot(respca2, ellipse=TRUE, choices=c(3,4), labels=rownames(df), groups=grupo)


fviz_pca_ind(respca2,
             col.ind = grupo, # color by groups
             palette = c("#00AFBB", "#FC4E07","#696969"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = FALSE
)

fviz_pca_biplot(respca1,
                col.ind = grupo, # color by groups
                palette = c("#00AFBB", "#FC4E07","#696969"),
                addEllipses = TRUE, # Concentration ellipses
                ellipse.type = "confidence",
                legend.title = "Groups",
                repel = FALSE
)
