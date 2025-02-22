# Pacotes e Instalacao de Pacotes

# De onde vem as funcoes? Pacotes (conjuntos de funcoes)
# Quando vocÃª inicia o RStudio, alguns pacotes sao
# carregados por padrÃ£o

search()

install.packages(c("ggvis", "tm", "dplyr"))
library(ggvis)
library(tm)
require(dplyr)

search()
?require
detach(package:dplyr)
?attach

ls(pos = "package:tm")
ls(getNamespace("tm"), all.names=TRUE)

lsf.str("package:tm")
lsf.str("package:ggplot2")
library(ggplot2)
lsf.str("package:ggplot2")


## R possui um conjunto de datasets preinstalados. Verificar datasets package

library(MASS)
data()

?lynx
head(lynx)
head(iris)
tail(lynx)
summary(lynx)

plot(lynx)
hist(lynx)
head(iris)
iris$Sepal.Length
sum(Sepal.Length)

attach(iris)
sum(Sepal.Length)
