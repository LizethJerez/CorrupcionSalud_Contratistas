# prueba de la libreria Stringdist

library(stringdist)

x <- data.frame(municipio = c("bogota", "barranquilla", "barrancabermeja", "bojaca", "bolivar", "bacata"))
y <- data.frame(municipio = c("bogota dc", "barraquilla", "bmeja", "bojacaa", "bolivar sder", "bacata"))

# metodo jaccard
stringdistmatrix(x$municipio, y$municipio, method = "jaccard", q = 2)

# metodo coseno
stringdistmatrix(x$municipio, y$municipio, method = "cosine", q = 2)

# metodo estadistico
stringdistmatrix(x$municipio, y$municipio, method = "jw", p = 0.1)

#ALGORITMO
for(i in 1: nrow(x)){
  
  # metodo estadistico
  z <- stringdist(x[i, "municipio"], y$municipio, method = "jw", p = 0.1)
  
  posicion <- which.min(z)
  
  if(z[posicion] < 0.15){
    x[i, "valor"] <- z[posicion]
    x[i, "asignacion"] <- y[posicion, "municipio"]
  } else{
    x[i, "valor"] <- "1"
    x[i, "asignacion"] <- "ninguno"
  }
}

