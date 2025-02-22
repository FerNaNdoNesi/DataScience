# Matrizes

# Criando Matrizes

# Numero de Linhas (nr) number of row
matrix (c(1,2,3,4,5,6), nr = 2)
a2 <- matrix (c(1,2,3,4,5,6), nr = 3)
matrix (c(1,2,3,4,5,6), nr = 6)
a2

# Numero de Colunas (nc) number of column
matrix ( c ( 1,2,3,4,5,6), nc = 2)


# Help
?matrix


# Matrizes precisam ter um numero par de valores
matrix ( c (1,2,3,4,5), nc = 2) # precisam ser multiplo ou submultiplo de n


# Criando matrizes a partir de vetores e preenchendo a partir das linhas
meus_dados = c(1:10)
matrix(data = meus_dados, nrow = 5, ncol = 2, byrow = T) #preencher orientando por linha
matrix(data = meus_dados, nrow = 5, ncol = 2)


# Fatiando a Matriz
mat <- matrix(c(2,3,4,5), nr = 2)
mat
mat[1,2]
mat[2,2]
mat[1,3]
mat[,2] #somente coluna dois
mat[2,] #somente linha dois
mat[,]


# Criando uma matriz diagonal
matriz = 1:3
matriz
diag (matriz)


# Extraindo vetor de uma matriz diagonal
matDiag = diag(matriz)
diagonal <- diag (matDiag)


# Transposta da matriz
W <- matrix ( c (2,4,8,12 ), nr =2, ncol = 2)
W
t(W)
U <- t(W)
U


# Obtendo uma matriz inversa
solve(W)


# Multiplicacao de Matrizes
mat1 <- matrix(c(2,3,4,5), nr = 2)
mat1
mat2 <- matrix(c(6,7,8,9), nr = 2)
mat2
mat1 * mat2
mat1 / mat2
mat1 + mat2
mat1 - mat2


# Multiplicando Matriz com Vetor
x = c(1:4)
x
y <- matrix(c(2,3,4,5), nr = 2)
x * y 


# Nomeando a Matriz
mat3 <- matrix(c('Futebol', 'Natacao', 'Campo', 'Piscina'), nr = 2)
mat3
dimnames(mat3) = (list( c("Linha1", "Linha2"), c("Coluna1", "Coluna2")))
mat3


# Identificando linhas e colunas no momento de criacao da Matriz
matrix (c(1,2,3,4), nr = 2, nc = 2, dimnames = list(c("Linha 1", "Linha 2" ), c( "Coluna 1", " Coluna 2") ))
matrix (c('Futebol', 'Natacao', 'Campo', 'Piscina'), nr = 2, nc = 2, dimnames = list(c("Linha 1", "Linha 2" ), c( "Coluna 1", " Coluna 2") ))



# Combinando Matrizes
mat4 <- matrix(c(2,3,4,5), nr = 2)
mat4
mat5 <- matrix(c(6,7,8,9), nr = 2)
mat5
cbind(mat4, mat5) #união baseada em colunas
rbind(mat4, mat5) #união baseada em linhas


# Desconstruindo a Matriz
c(mat4)

