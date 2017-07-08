#IA TF

library(prob)

# Cantidad de Retweets realizados por un usuario
Retweets = data.frame(
  Retweets = c("Muchos","Pocos"),
  probs = c(0.30, 0.70)
)

# Indica si la cuenta es verificada o no
Verificada = data.frame(
  Verificada = c("Si","No"),
  probs = c(0.10, 0.90)
)

# Cantidad de Seguidores que tiene un usuario
Seguidores = data.frame(
  Seguidores = c("Muchos","Pocos"),
  probs = c(0.25, 0.75)
)

# Cantidad de Amigos que tiene un usuario
Amigos = data.frame(
  Amigos = c("Muchos","Pocos"),
  probs = c(0.45, 0.55)
)

# Cantidad de Amigos que tiene un usuario
Favoritos = data.frame(
  Favoritos = c("Muchos","Pocos"),
  probs = c(0.4, 0.6)
)

# Cantidad de Menciones realizadas al usuario
Menciones = data.frame(
  Menciones = c("Muchos","Pocos"),
  probs = c(0.35, 0.65)
)

# containsHashtag
# containslink

# Método para obtener data de un archivo
putExternalData <- function(X, fileName) {
  
  Datos <- read.table(file = fileName)
  
  X <- data.frame(X, Datos)
  X <- probspace(X)
  
  for(i in 1:128) {
    X$probs[i] = X$V1[i]/sum(X$V1)
  }
  return(X)
}

# Varibale a analizar
Malicioso = expand.grid(
  Retweets = c("Muchos","Pocos"),
  Verificada = c("Si","No"),
  Seguidores = c("Muchos","Pocos"),
  Amigos = c("Muchos","Pocos"),
  Favoritos = c("Muchos","Pocos"),
  Menciones = c("Muchos","Pocos"),
  Malicioso = c("Si","No")
)

# Carpeta por fefault Mis Documentos usar setwd() para cambiarla si se desea
Malicioso <- putExternalData(Malicioso, "Data.txt")

##### Inferencias #####
#Ver tabla en R Studio para saber a que fila pertenece el caso

# Fila 1
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 2
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 3
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 4
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 5
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 6
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 7
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 8
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 9
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 10
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 11
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 12
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 13
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 14
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 15
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 16
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 17
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 18
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 19
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 20
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 21
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 22
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 23
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 24
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 25
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 26
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 27
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 28
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 29
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 30
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 31
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 32
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 33
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 34
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 35
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 36
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 37
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 38
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 39
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 40
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 41
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 42
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 43
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 44
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 45
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 46
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 47
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 48
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 49
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 50
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 51
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 52
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 53
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 54
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 55
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 56
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 57
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 58
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 59
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 60
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 61
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 62
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 63
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 64
Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 65
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 66
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 67
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 68
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 69
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 70
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 71
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 72
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 73
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 74
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 75
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 76
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 77
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 78
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 79
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 80
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 81
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 82
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 83
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 84
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 85
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 86
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 87
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 88
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 89
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 90
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 91
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 92
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 93
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 94
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 95
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 96
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')

# Fila 97
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 98
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 99
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 100
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 101
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 102
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 103
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 104
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 105
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 106
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 107
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 108
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 109
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 110
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 110
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 112
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 113
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 114
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 115
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 116
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 117
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 118
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 119
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 120
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 121
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 122
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 123
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 124
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 125
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 126
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 127
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')

# Fila 128
Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')
