at.win = 0
bar.win = 0
re.win = 0
se.win = 0
val.win = 0
zar.win = 0

at.gol = 0
bar.gol = 0
re.gol = 0
se.gol = 0
val.gol = 0
zar.gol = 0

for (j in 1:2) {
  
  at1 = floor(runif(1, min = 0, max = 9) )
  ba = floor(runif(1, min = 0, max = 9) )
  
  at2 = floor(runif(1, min = 0, max = 9) )
  re = floor(runif(1, min = 0, max = 9) ) 
  
  at3 = floor(runif(1, min = 0, max = 9) ) 
  se = floor(runif(1, min = 0, max = 9) ) 
  
  at4 = floor(runif(1, min = 0, max = 9) ) 
  va = floor(runif(1, min = 0, max = 9) ) 
  
  at5 = floor(runif(1, min = 0, max = 9) ) 
  rza = floor(runif(1, min = 0, max = 9) )
  
  if (at1 > ba){
    
    at.win = at.win + 1
    
  } else if (at1 < ba){
    
    bar.win = bar.win + 1
    
  }
  
  if (at2 > re){
    
    at.win = at.win + 1
    
  } else if (at2 < re) {
    
    re.win = re.win + 1
    
  }
  
  if (at3 > se){
    
    at.win = at.win + 1
    
  } else if (at3 < se) {
    
    se.win = se.win + 1
    
  }
  
  if (at4 > va){
    
    at.win = at.win + 1
    
  } else if (at4 < va) {
    
    val.win = val.win + 1
    
  }
  
  if (at5 > rza){
    
    at.win = at.win + 1
    
  } else if (at5 < rza) {
    
    zar.win = zar.win + 1
    
  }
  
  at.gol = at.gol + at1 + at2 + at3 + at4 + at5
  bar.gol = bar.gol + ba
  re.gol = re.gol + re
  se.gol = se.gol + se
  val.gol = val.gol + va
  zar.gol = zar.gol + rza
}

for (k in 1:2) {
  
  ba1 = floor(runif(1, min = 0, max = 9) ) 
  re = floor(runif(1, min = 0, max = 9) ) 
  
  ba2 = floor(runif(1, min = 0, max = 9) ) 
  se = floor(runif(1, min = 0, max = 9) ) 
  
  ba3 = floor(runif(1, min = 0, max = 9) ) 
  va = floor(runif(1, min = 0, max = 9) ) 
  
  ba4 = floor(runif(1, min = 0, max = 9) ) 
  rza = floor(runif(1, min = 0, max = 9) ) 
  
  if (ba1 > re){
    
    bar.win = bar.win + 1
    
  } else if (ba1 < re) {
    
    re.win = re.win + 1
    
  }
  
  if (ba2 > se){
    
    bar.win = bar.win + 1
    
  } else if (ba2 < se) {
    
    se.win = se.win + 1
    
  }
  
  if (ba3 > va){
    
    bar.win = bar.win + 1
    
  } else if (ba3 < va) {
    
    val.win = val.win + 1
    
  }
  
  if (ba4 > rza){
    
    bar.win = bar.win + 1
    
  } else if (ba4 < rza) {
    
    zar.win = zar.win + 1
    
  }
  
  bar.gol = bar.gol + ba1 + ba2 + ba3 + ba4
  re.gol = re.gol + re
  se.gol = se.gol + se
  val.gol = val.gol + va
  zar.gol = zar.gol + rza
}

for (l in 1:2) {
  
  re1 = floor(runif(1, min = 0, max = 9) ) 
  se = floor(runif(1, min = 0, max = 9) ) 
  
  re2 = floor(runif(1, min = 0, max = 9) ) 
  va = floor(runif(1, min = 0, max = 9) ) 
  
  re3 = floor(runif(1, min = 0, max = 9) ) 
  rza = floor(runif(1, min = 0, max = 9) ) 
  
  if (re1 > se){
    
    re.win = re.win + 1
    
  } else if (re1 < se) {
    
    se.win = se.win + 1
    
  }
  
  if (re2 > va){
    
    re.win = re.win + 1
    
  } else if (re2 < va) {
    
    val.win = val.win + 1
    
  }
  
  if (re3 > rza){
    
    re.win = re.win + 1
    
  } else  if (re3 < rza) {
    
    zar.win = zar.win + 1
    
  }
  
  re.gol = re.gol + re1 + re2 + re3
  se.gol = se.gol + se
  val.gol = val.gol + va
  zar.gol = zar.gol + rza
}

for (m in 1:2) {
  
  se1 = floor(runif(1, min = 0, max = 9) ) 
  va = floor(runif(1, min = 0, max = 9) ) 
  
  se2 = floor(runif(1, min = 0, max = 9) ) 
  rza = floor(runif(1, min = 0, max = 9) ) 
  
  if (se1 > va){
    
    se.win = se.win + 1
    
  } else if (se1 < va) {
    
    val.win = val.win + 1
    
  }
  
  if (se2 > rza){
    
    se.win = se.win + 1
    
  } else if (se2 < rza) {
    
    zar.win = zar.win + 1
    
  }
  
  se.gol = se.gol + se1 + se2
  val.gol = val.gol + va
  zar.gol = zar.gol + rza
}

for (o in 1:2) {
  
  
  va = floor(runif(1, min = 0, max = 9) ) 
  rza = floor(runif(1, min = 0, max = 9) ) 
  
  if (va > rza){
    
    val.win = val.win + 1
    
  } else if (va < rza) {
    
    zar.win = zar.win + 1
    
  }
  
  val.gol = val.gol + va
  zar.gol = zar.gol + rza
}

wins.vec = c(at.win, bar.win, re.win, se.win, val.win, zar.win)

equi.vec = c("Atletico de Madrid", "FC Barcelo", "Real Madrid CF", "Sevilla CF", "Valencia CF", "Real Zaragoza")
tab = data.frame(wins.vec, equi.vec)

tab2 = tab[tab$wins.vec %in% tail(sort(tab$wins.vec),2),]

print( paste0("A la final van los equipos ", tab2[1, 2], " Y ", tab2[2,2] ))

goles = c(1:9)

fin = c(sample(goles, size = 2, replace = T)) 

print( paste0( "El resultado de la final es ", tab2[1,2], " ", fin[1], " - ", fin[2] ," ", tab2[2,2]))
} else {
  print( paste0("El numero ingresado no es valido."))