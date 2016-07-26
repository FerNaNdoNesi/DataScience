# Exercicios Capitulo 3

# Exercicio 1 - Crie uma funcao que receba e vetores como parametro, converta-os em um dataframe e imprima
vec1 <- (10:13)
vec2 <- c("a", "b", "c", "d")

myfunc <- function(x, y){
  df = data.frame(cbind(x, y))
  print(df)
}

myfunc(vec1, vec2)


# Exercicio 2 - Crie uma matriz com 4 linhas e 4 colunas preenchida com numeros inteiros e calcule a media de cada linha
mat <- matrix(c(1:16), nrow = 4, ncol = 4)
mat
apply(mat, 1, mean)


# Exercicio 3 - Considere o dataframe abaixo. Calcule a media por disciplina
escola <- data.frame(Aluno = c('Alan', 'Alice', 'Alana', 'Aline', 'Alex', 'Ajay'),
                     Matematica = c(90, 80, 85, 87, 56, 79),
                     Geografia = c(100, 78, 86, 90, 98, 67),
                     Quimica = c(76, 56, 89, 90, 100, 87))
escola
apply(escola[, c(2, 3, 4)], 2, mean)


# Exercicio 4 - Cria uma lista com 3 elementos, todos numÃ©ricos e calcule a soma de todos os elementos da lista
lst <- list(a = 1:10, b = 1:5)
lst
do.call(sum, lst)


# Exercicio 5 - Transforme a lista anterior um vetor
unlist(lst)


# Exercicio 6 - Considere a string abaixo. Substitua a palavra textos por frases
str <- c("Expressoes", "regulares", "em linguagem R",
         "permitem a busca de padroes", "e exploracao de textos",
         "podemos buscar padroes em digitos",
         "como por exemplo",
         "10992451280")

gsub("textos", "frases", str)


# Exercicio 7 - Usando o dataset mtcars, crie um grafico com ggplot do tipo scatter plot. Use as colunas disp e mpg nos eixos x e y respectivamente
library(ggplot2)
ggplot(data = mtcars, aes(x = disp, y = mpg)) + geom_point()


# Exercicio 8 - Usando o exemplo anterior, explore outros tipos de grÃ¡ficos
