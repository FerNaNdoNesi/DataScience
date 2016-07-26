### Strings

# String
texto <- "Isso eh uma string!"
texto

x = as.character(3.14) 
x
class(x) 


# Concatenando Strings
nome = "Barack"; sobrenome = "Obama" 
a <- paste(nome, sobrenome)
b <- cat(nome, sobrenome)
a
b


# Formatando a saida
sprintf("%s governa os EUA hÃ¡ %d anos", "Barack Obama", 8) 


# Extraindo parte da string
texto <- "Isso eh uma string!"
b <- substr(texto, start=5, stop=17) 
?substr
b

# Contando o numero de caracteres
nchar(texto)


# Alterando a capitalizacao
tolower("Histogramas e Elementos de Dados")
toupper("Histogramas e Elementos de Dados")


# Usando stringr
library(stringr)


# Dividindo uma string em caracteres
strsplit("Histogramas e Elementos de Dados", NULL)
?strsplit


# Dividindo uma string em caracteres, apos o caracter espaÃ§o
strsplit("Histogramas e Elementos de Dados", " ")


# Trabalhando com strings
string1 <- c("Esta Ã© a primeira parte da minha string e serah a primeira parte do meu vetor", 
                "Aqui a minha string continua, mas serÃ¡ transformada no segundo vetor")

string1

string2 <- c("Precisamos testar outras strings",
                  "Anaise de Dados em R")

string2

# Adicionando 2 strings
str_c(c(string1, string2), sep="")


# Podemos contar quantas vezes um caracter aparece no texto
str_count(string2, "s")


# Localiza a primeira e ultima posicao em que o caracter aparece na string
str_locate_all(string2, "s")


# Substitui a primeira ocorrencia de um caracter
str_replace(string2, "\\s", "")


# Substitui todas as ocorrencias de um caracter
str_replace_all(string2, "\\s", "")


# Detectando padroes nas strings
string1 <- "23 mai 2000"
string2 <- "1 mai 2000"
padrao <- "mai 20"
grepl(pattern = padrao, x = string1)
padrao <- "mai20"
grepl(pattern = padrao, x = string1)


# Importando arquivo txt
# http://www.gutenberg.org/cache/epub/100/pg100.txt

arquivo <- read.csv("http://www.gutenberg.org/cache/epub/100/pg100.txt")

head(arquivo)
tail(arquivo)

str_count(arquivo, "7")
str_locate_all(arquivo, "7")


# Criando funcoes para manipular strings
strtail <- function(s,n=1) {
  if(n<0) 
    substring(s,1-n) 
  else 
    substring(s,nchar(s)-n+1) }

strtail("String de teste", 0)



