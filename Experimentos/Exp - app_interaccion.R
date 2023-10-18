library(shiny)
library(shinydashboard)
library(ggplot2)
library(car)


ui <- dashboardPage(
  
  dashboardHeader(title =  "EJEMPLO EFECTOS DE INTERACCION",titleWidth = 600),
  
  dashboardSidebar(
    width=300,
    sidebarMenu(
      menuItem("Segundo Factor",
               menuSubItem("Dos niveles",tabName = "uno"),
               menuSubItem("Tres niveles",tabName = "dos")),
      sliderInput("v",
                  "Varianza:",
                  min = 1,
                  max = 50,
                  value = 1),
      sliderInput("r",
                  "Número de réplicas por tratamiento:",
                  min = 1,
                  max = 50,
                  value = 4),
      h5("Ver líneas sin interacción"),
                   checkboxInput("line","Ver líneas",value = F),
      h5("Ver las observaciones de cada tratamiento"),
                   checkboxInput("punt","Ver puntos",value = F),
      h5("Efectos de interacción"),
                   checkboxInput("listo","Ver Efectos",value=F),
      h5("Significancia de interacción"),
                   checkboxInput("listo2","Ver significancia",value=F),
      actionButton("simu","Simular")
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "uno",fluidPage(
        sliderInput("e",
                    "Efecto de interacción:",
                    min = -10,
                    max = 10,
                    value = 1),
        
        plotOutput("interaccion"),
        verbatimTextOutput("efs"),
        verbatimTextOutput("efs2")
      )),
      tabItem(tabName = "dos",fluidPage(
        sliderInput("e2",
                    "Primer efecto de interacción:",
                    min = -10,
                    max = 10,
                    value = 1),
        sliderInput("e3",
                    "Segundo efecto de interacción:",
                    min = -10,
                    max = 10,
                    value = 1),
        plotOutput("interaccion1"),
        verbatimTextOutput("efs1"),
        verbatimTextOutput("efs3")
      ))
    )
  )
)


server <- function(input, output) {
  options(contrasts=c("contr.sum","contr.poly"))
  grafico <- eventReactive(input$simu,{
    datos <- reactiveValues()
    k  = 2
    d1 = 50
    d2 = 50
    mu = 100
    
    if (k == 2)
      datos$int = runif(1, 0, 50)
    else
      datos$int = c(runif(2, 0, 50))
    
    n1 <- 2 * input$r * k
    
    datos$normal <- rnorm(n1)
    
    datos$normal2 <- rnorm(n1,0,sqrt(input$v))
    
    n1 <- 2 * input$r * k
    X1 <- factor(rep(1:2, each=input$r * k))
    X2 <- factor(rep(1:k, each=input$r,2))
    
    X <- model.matrix(lm(datos$normal~X1*X2))
    alfa = d1/2
    
    if(k==2) 
      beta1 = d2/2  
    else 
      beta1 = c(d2/2,0) 
    
    B <- matrix(c(mu,alfa,beta1,input$e),ncol=1)
    y <- X%*%B + datos$normal2
    pred <- predict(lm(y~X1+X2))
    
    base <- data.frame(X1,X2,y,pred)
    
    base$X2=recode(base$X2,"1='B1';2='B2';;else='B3'" )
    base$X1=recode(base$X1,"1='A1';2='A2';;else='A3'" )
    
    return(base)
  })
  
  
  grafico2 <-eventReactive(input$simu,{
    datos1 <- reactiveValues()
    k <- 3
    d1 = 50
    d2 = 50
    mu = 100
    
    if (k == 2)
      datos1$int = runif(1, 0, 50)
    else
      datos1$int = c(runif(2, 0, 50))
    
    n1 <- 2 * input$r * k
    
    datos1$normal <- rnorm(n1)
    datos1$normal2 <- rnorm(n1,0,sqrt(input$v))
    
    n1 <- 2 * input$r * k
    X1 <- factor(rep(1:2,each=input$r * k))
    X2 <- factor(rep(1:k, each=input$r,2))
    
    X <- model.matrix(lm(datos1$normal~X1*X2))
    alfa = d1/2
    
    if(k==2) 
      beta1 = d2/2  
    else 
      beta1 = c(d2/2,0) 
    
    B <- matrix(c(mu,alfa,beta1,c(input$e2,input$e3)),ncol=1)
    y <- X%*%B + datos1$normal2
    pred <- predict(lm(y~X1+X2))
    base <- data.frame(X1,X2,y,pred)
    base$X2=recode(base$X2,"1='B1';2='B2';;else='B3'" )
    base$X1=recode(base$X1,"1='A1';2='A2';;else='A3'" )
   return(base)
  })
  
  output$interaccion <- renderPlot({
    base <- grafico()
    
    g <- ggplot(base,aes(x = X1, group = X2)) +
      stat_summary(fun = "mean", geom = "line",aes(linetype = X2,y = y))+
      labs(x = "Factor A", linetype = "Factor", y = "Respuesta")+ 
      theme_bw() +ylim(40,200)+
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"))+
      stat_summary(fun = "mean",
                   geom = "point",
                   aes(y = y),
                   size=3,
                   colour = "black")
    if(input$punt){
      g <- g+
        geom_jitter(data = base,aes(y=y,colour=X2),
                    position = position_jitter(width = 0.1, height = 0.1,seed=30),
                    size=2)+ labs(colour="Factor")
      
    }else{
      g
    }
    if(input$line){
      g <- g+
        stat_summary(fun = "mean",
                     geom = "line",
                     aes(linetype = X2,y = pred),
                     colour = "red")+
        stat_summary(fun = "mean",
                     geom = "point",
                     aes(y = pred),
                     size=3,
                     colour = "red")
    }else{
      g 
    }
    
    return(g)
  })
  
  output$interaccion1 <- renderPlot({
    base <- grafico2()
        g <- ggplot(base,aes(x = X1, group = X2)) +
      stat_summary(fun = "mean", geom = "line",aes(linetype = X2,y = y))+
      labs(x = "Factor A", linetype = "Factor ", y = "Respuesta")+ 
      theme_bw() +ylim(40,200)+
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"))+
      stat_summary(fun = "mean",
                       geom = "point",
                       aes(y = y),
                       size=3,
                       colour = "black")
    
    if(input$line){
      g <- g+
        stat_summary(fun = "mean",
                     geom = "line",
                     aes(linetype = X2,y = pred),
                     colour = "red")+
        stat_summary(fun = "mean",
                     geom = "point",
                     aes(y = pred),
                     size=3,
                     colour = "red")
    }else{
      g
    }

    if(input$punt){
          g <- g+
            geom_jitter(data = base,aes(y=y,colour=X2),
                        position = position_jitter(width = 0.1, height = 0.1,seed=30),
                        size=2)+ labs(colour="Factor")
      
    }else{
      g
    }
    return(g)
  })
 
   output$efs <- renderPrint({
     base <- grafico()
     A <- base[[1]]
    B<- base[[2]]
     y <- base[[3]]
    efec <- if(input$listo){
      ef.int <- t(round( model.tables(aov(lm(y~A*B)))$tables$`A:B`,2))
    
    }else{
      efec=NULL
    }
    print(efec)
  })
  
   output$efs1 <- renderPrint({
     base <- grafico2()
     A <- base[[1]]
     B <- base[[2]]
     y <- base[[3]] 
   
    efec <- if(input$listo){
      ef.int <- t(round(model.tables(aov(lm(y~A*B)))$tables$`A:B`,2))
      
    }else{
      efec=NULL
    }
   
    print(efec)
  })
  
   output$efs2 <- renderPrint({
     base <- grafico()
     A <- base[[1]]
     B <- base[[2]]
     y <- base[[3]] 
     
     sig <- if(input$listo2){
       anova(aov(lm(y~A*B)))[3,]
       
     }else{
       sig=NULL
     }
     print(sig)
   })
   
   output$efs3 <- renderPrint({
     base <- grafico2()
     A <- base[[1]]
     B <- base[[2]]
     y <- base[[3]] 
     
     sig <- if(input$listo2){
       anova(aov(lm(y~A*B)))[3,]
       
     }else{
       sig=NULL
     }
     print(sig)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

