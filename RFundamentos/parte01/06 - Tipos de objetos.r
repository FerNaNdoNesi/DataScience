### Objetos em R

# Vetor: possui 1 dimensao e 1 tipo de dado

vetor1 <- c(1:10)
vetor1
length(vetor1)
mode(vetor1)
class(vetor1)
typeof(vetor1)


# Matriz: possui 2 dimensoes e 1 tipo de dado 

matriz1 <- matrix(1:10, nrow =2)
matriz1
length(matriz1)
mode(matriz1)
class(matriz1)
typeof(matriz1)


# Array: possui 2 ou mais dimensoes e 1 tipo de dado 

array2 <- array(1:5, dim=c(3,3,3))
array2 <- array(1:5, dim=c(4,4,4))
array1
array2
length(array1)
mode(array1)
class(array1)
typeof(array1)


# Data Frames: dados de diferentes tipos
# Maneira mais facil de explicar data frames: eh uma matriz com diferentes tipos de dados

View(iris)
length(iris)
mode(iris)
class(iris)
typeof(iris)


# Listas: colecao de diferentes objetos
# Diferentes tipos de dados sÃ£o possiveis e comuns

lista1 <- list(a=matriz1, b=vetor1)
lista3 <- list(a=matriz1, b=vetor1, c=View(iris))
lista1
lista3
length(lista1)
mode(lista1)
class(lista1)
typeof(lista1)


# FunÃ§Ãµes tambem sÃ£o vistas como objetos em R

func1 <- function(x) {
  var1 <- x * x
  return(var1)
}

func1(5)
class(func1)


# Removendo objetos
objects()
rm(array1, func1)
objects()





