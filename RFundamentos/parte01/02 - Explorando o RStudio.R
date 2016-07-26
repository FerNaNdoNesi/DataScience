#02 - Explorando o RStudio

#Nome dos contributors
contributors()
license()

#Informações sobre a seção
sessionInfo()

#Imprimir na tela
print('Print em R')

#Gerar Gráfico
plot(1:30)

#Instalar pacotes
install.packages('randomForest')
install.packages('ggplot2')

#Carregar pacotes instalados
library(ggplot2) #Para ver pacores carregados Painel inferior-esquedo > packeage

#Descarregar Pacote
detach(package:ggplot2)

#Saber pelo NOME como a função funciona (busca dentro do manual do R)
help(mean)
?mean

#Se não souber o nome da função (Busca todas as referencias sobre)
help.search('randomForest')
help.search('matplot')
??randomFlorest

#Busca direto no site
RSiteSearch('matplot')

example('matplot')
