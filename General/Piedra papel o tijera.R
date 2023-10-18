# Función para generar la elección aleatoria de la computadora
computadora <- function() {
  opciones <- c("piedra", "papel", "tijera")
  return(sample(opciones, 1))
}

# Función para comparar la elección del usuario y de la computadora
juego <- function(usuario, computadora) {
  if (usuario == computadora) {
    return("Empate")
  } else if (usuario == "piedra" && computadora == "tijera" ||
             usuario == "papel" && computadora == "piedra" ||
             usuario == "tijera" && computadora == "papel") {
    return("Ganaste!")
  } else {
    return("Perdiste :(")
  }
}

# Función para iniciar el juego
jugar_ppt <- function() {
  usuario <- readline("Elige piedra, papel o tijera: ")
  usuario <- tolower(usuario)
  
  if (usuario != "piedra" && usuario != "papel" && usuario != "tijera") {
    cat("Opción no válida. Vuelve a intentarlo.\n")
    jugar_ppt()
  } else {
    computadora_eleccion <- computadora()
    resultado <- juego(usuario, computadora_eleccion)
    
    cat("Elegiste", usuario, "\n")
    cat("La computadora eligió", computadora_eleccion, "\n")
    cat(resultado, "\n")
  }
}

# Iniciar el juego
jugar_ppt()
