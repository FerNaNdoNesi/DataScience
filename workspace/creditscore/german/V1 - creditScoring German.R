install.packages("readr") # Usando o pacote readr
install.packages("dplyr") #tratamento de dados em memória
install.packages("tidyr") # Objetivo de remodelar os dados
install.packages("caTools") #dividir os conjuntos de dados
install.packages("rpart.plot") # visualizaco da arvore ficar mais legivel
install.packages("gmodels") # Matriz de confusão
install.packages("ROCR") #criar curva ROC
install.packages("gplots")
library(readr) 
library(dplyr)
library(tidyr)
library(caTools)
library(rpart.plot)
library(gmodels)
library(ROCR)
library(gplots)


print("Credit Scoring German Data Set")

setwd("~/git/dataScience/workspace/creditscore/german")


################ Conjunto de dados ################

arq_german_credit_data <- read_csv("german-credit-data.csv")
mod_arq_german_credit_data <- read_csv("german-credit-data.csv") #Carrega dados
#View(arq_german_credit_data)
glimpse(arq_german_credit_data) #Visualização rápida
table(arq_german_credit_data$class)
prop.table(table(arq_german_credit_data$class)) #verifica proporção


################ Conjunto de dados ################

######################### ETAPA 01: Limpeza dos dados #########################
any(is.na(arq_german_credit_data)) # se existir valores nulos é necessário trata-los
# No conjunto German não existe valores null

#any(is.na(dataSet$NumDep))
#any(is.na(dataSet$Renda))

#summary(dataSet$NumDep) #média 0
#prop.table(table(dataSet$NumDep))
#dataSet$NumDep[is.na(dataSet$NumDep)] <- 0 #0.5814 -> 0.5920 percent de 0

#summary(dataSet$Renda) #média: 5081
#prop.table(table(dataSet$Renda))
#dataSet$Renda[is.na(dataSet$Renda)] <- 5081 #NA's 1903
#glimpse(dataSet)

######################### Valores categóricos em numéricos ######################### 
#arq_german_credit_data <- read_csv("german-credit-data.csv") #Carrega dados
#mod_arq_german_credit_data <- read_csv("german-credit-data.csv") #Carrega dados
#glimpse(arq_german_credit_data)
#glimpse(mod_arq_german_credit_data)
#table(arq_german_credit_data$StatusCc)
#summary(arq_german_credit_data$StatusCc)
#prop.table(table(arq_german_credit_data$StatusCc))
#scale(arq_german_credit_data$PercentRend)
#mod_arq_german_credit_data$StatusCc <- factor(arq_german_credit_data$StatusCc, levels = c("A11", "A12", "A13", "A14"), labels = c(1.0, 2.0, 3.0, 4.0)) #Classe como fator
#arq_german_credit_data$class <- factor(arq_german_credit_data$class, levels = c(1,2), labels = c("Bom", "Ruim")) #Classe como fator

#dados_norm <- as.data.frame(lapply(arq_german_credit_data$MesesCc, numer))
#glimpse(mod_arq_german_credit_data)
#summary(dados_norm$MesesCc)
#dados_norm <- as.data.frame(lapply(arq_german_credit_data, normalizar))
#glimpse(dados_norm)

#INICIO
#arq_german_credit_data <- read_csv("german-credit-data.csv") #Carrega dados
#mod_arq_german_credit_data <- read_csv("german-credit-data.csv") #Carrega dados
glimpse(arq_german_credit_data)
glimpse(mod_arq_german_credit_data)
mod_arq_german_credit_data$StatusCc <- as.double(factor(arq_german_credit_data$StatusCc, levels = labels(c(table(arq_german_credit_data$StatusCc))), labels = c(1:length(c(table(arq_german_credit_data$StatusCc))))))
mod_arq_german_credit_data$MesesCc <- as.double(mod_arq_german_credit_data$MesesCc)
mod_arq_german_credit_data$HistCredit <- as.double(factor(arq_german_credit_data$HistCredit, levels = labels(c(table(arq_german_credit_data$HistCredit))), labels = c(1:length(c(table(arq_german_credit_data$HistCredit))))))
mod_arq_german_credit_data$PropEmp <- as.double(factor(arq_german_credit_data$PropEmp, levels = labels(c(table(arq_german_credit_data$PropEmp))), labels = c(1:length(c(table(arq_german_credit_data$PropEmp))))))
mod_arq_german_credit_data$MontEmp <- as.double(mod_arq_german_credit_data$MontEmp)
mod_arq_german_credit_data$Invest <- as.double(factor(arq_german_credit_data$Invest, levels = labels(c(table(arq_german_credit_data$Invest))), labels = c(1:length(c(table(arq_german_credit_data$Invest))))))
mod_arq_german_credit_data$TrabAtual <- as.double(factor(arq_german_credit_data$TrabAtual, levels = labels(c(table(arq_german_credit_data$TrabAtual))), labels = c(1:length(c(table(arq_german_credit_data$TrabAtual))))))
mod_arq_german_credit_data$PercentRend <- as.double(mod_arq_german_credit_data$PercentRend)
mod_arq_german_credit_data$EstCivil <- as.double(factor(arq_german_credit_data$EstCivil, levels = labels(c(table(arq_german_credit_data$EstCivil))), labels = c(1:length(c(table(arq_german_credit_data$EstCivil))))))
mod_arq_german_credit_data$OutrosDev <- as.double(factor(arq_german_credit_data$OutrosDev, levels = labels(c(table(arq_german_credit_data$OutrosDev))), labels = c(1:length(c(table(arq_german_credit_data$OutrosDev))))))
mod_arq_german_credit_data$ResidAtual <- as.double(mod_arq_german_credit_data$ResidAtual)
mod_arq_german_credit_data$TipoResid <- as.double(factor(arq_german_credit_data$TipoResid, levels = labels(c(table(arq_german_credit_data$TipoResid))), labels = c(1:length(c(table(arq_german_credit_data$TipoResid))))))
mod_arq_german_credit_data$Idade <- as.double(mod_arq_german_credit_data$Idade)
mod_arq_german_credit_data$OutrosParc <- as.double(factor(arq_german_credit_data$OutrosParc, levels = labels(c(table(arq_german_credit_data$OutrosParc))), labels = c(1:length(c(table(arq_german_credit_data$OutrosParc))))))
mod_arq_german_credit_data$TipoMoradia <- as.double(factor(arq_german_credit_data$TipoMoradia, levels = labels(c(table(arq_german_credit_data$TipoMoradia))), labels = c(1:length(c(table(arq_german_credit_data$TipoMoradia))))))
mod_arq_german_credit_data$NumEmpAtivo <- as.double(mod_arq_german_credit_data$NumEmpAtivo)
mod_arq_german_credit_data$TipoTrab <- as.double(factor(arq_german_credit_data$TipoTrab, levels = labels(c(table(arq_german_credit_data$TipoTrab))), labels = c(1:length(c(table(arq_german_credit_data$TipoTrab))))))
mod_arq_german_credit_data$NumPessoas <- as.double(mod_arq_german_credit_data$NumPessoas)
mod_arq_german_credit_data$Telefone <- as.double(factor(arq_german_credit_data$Telefone, levels = labels(c(table(arq_german_credit_data$Telefone))), labels = c(1:length(c(table(arq_german_credit_data$Telefone))))))
mod_arq_german_credit_data$TrabEstr <- as.double(factor(arq_german_credit_data$TrabEstr, levels = labels(c(table(arq_german_credit_data$TrabEstr))), labels = c(1:length(c(table(arq_german_credit_data$TrabEstr))))))
glimpse(mod_arq_german_credit_data)
dados_norm <- as.data.frame(lapply(mod_arq_german_credit_data, scale))
glimpse(dados_norm)
dados_norm$class <- factor(dados_norm$class, levels = as.double(labels(c(table(dados_norm$class)))), labels = c("Bom", "Ruim")) #Classe como fator
mod_arq_german_credit_data$class <- factor(arq_german_credit_data$class, levels = c(1,2), labels = c("Bom", "Ruim")) #Classe como fator
arq_german_credit_data$class <- factor(arq_german_credit_data$class, levels = c(1,2), labels = c("Bom", "Ruim")) #Classe como fator

#Datasets prontos#
glimpse(arq_german_credit_data) #dataset original categorico/num
glimpse(mod_arq_german_credit_data) #dataset numerico
glimpse(dados_norm) #dataset numerico normalizado

####################### ETAPA 02: Análise exploratória ########################
#1 - Atributo classe deve ser do tipo Fator
str(arq_german_credit_data$class) #retorna tipo variavel
#table(arq_german_credit_data$class) #retorna percentual de distribuição
#arq_german_credit_data$class <- factor(arq_german_credit_data$class, levels = c(1,2), labels = c("Good", "Bad"))
#original_german_credit_data$class <- factor(original_german_credit_data$class, levels = c(1,2), labels = c("Good", "Bad"))
#table(arq_german_credit_data$class)
#str(arq_german_credit_data$class) #agora é um fator

#2 - Discretização dos atributos ## Explorando variaveis numericas
#Verificar medidas de tendência central
table(arq_german_credit_data$MontEmp) #retorna percentual de distribuição
prop.table(table(arq_german_credit_data$MontEmp)) #verifica proporção
count(arq_german_credit_data, MontEmp) #conta quantas vezes cada valor aparece

table(arq_german_credit_data$MesesCc) #retorna percentual de distribuição
prop.table(table(arq_german_credit_data$MesesCc)) #verifica proporção
count(arq_german_credit_data, MesesCc) #conta quantas vezes cada valor aparece

table(arq_german_credit_data$Idade) #retorna percentual de distribuição
prop.table(table(arq_german_credit_data$Idade)) #verifica proporção
count(arq_german_credit_data, Idade) #conta quantas vezes cada valor aparece

#Medidas e tendencias centais
#não precisam discretizar:
summary(arq_german_credit_data[c("PercentRend", "ResidAtual", "NumEmpAtivo", "NumPessoas")])
#Precisam discretizar: (valores muito distintos)
summary(arq_german_credit_data[c("MesesCc", "MontEmp", "Idade")])

# Medidas de Dispersao
# Ao interpretar a variancia, numeros maiores indicam que 
# os dados estao espalhados mais amplamente em torno da 
# media. O desvio padrao indica, em media, a quantidade 
# de cada valor diferente da media.
var(arq_german_credit_data$MontEmp) #Correlação, variância
sd(arq_german_credit_data$MontEmp) #desvio padrão

var(arq_german_credit_data$MesesCc) #Correlação, variância
sd(arq_german_credit_data$MesesCc) #desvio padrão

var(arq_german_credit_data$Idade) #Correlação, variância
sd(arq_german_credit_data$Idade) #desvio padrão

quantile(arq_german_credit_data$MontEmp) #quartis normais
quantile(arq_german_credit_data$MontEmp, seq( from = 0, to = 1, by = 0.20)) #quartis modificados
boxplot(arq_german_credit_data$MontEmp, main = "Boxplot para montante de emprestimo", ylab = "(R$)")
hist(arq_german_credit_data$MontEmp, main = "Histograma para montante de emprestimo", xlab = "(R$)")

quantile(arq_german_credit_data$MesesCc) #quartis normais
quantile(arq_german_credit_data$MesesCc, seq( from = 0, to = 1, by = 0.20)) #quartis modificados
boxplot(arq_german_credit_data$MesesCc, main = "Boxplot para meses com conta corrente", ylab = "Meses")
hist(arq_german_credit_data$MesesCc, main = "Histograma para meses com conta corrente", xlab = "Meses")

quantile(arq_german_credit_data$Idade) #quartis normais
quantile(arq_german_credit_data$Idade, seq( from = 0, to = 1, by = 0.20)) #quartis modificados
boxplot(arq_german_credit_data$Idade, main = "Boxplot para montante de emprestimo", ylab = "(R$)")
hist(arq_german_credit_data$Idade, main = "Histograma para montante de emprestimo", xlab = "(R$)")

## Explorando variaveis categoricas
# A FAZER função discretizar em R utilizando quantile

#3 - Normalização dos atributos ## Explorando variaveis numericas
#Detectar problemas de escala entre os dados distâncias muito distintas precisam ser normalizadas
#normalizar <- function(x){
#  return ((x - min(x)) / (max(x) - min(x)))
#}
#numer <- function(x){
#  return (x * 1.0)
#}
#dados_norm <- as.data.frame(lapply(arq_german_credit_data[c("MesesCc", "MontEmp", "Idade")], normalizar))
head(dados_norm)
# SOMENTE EXEMPLO (nesta base de dados não temos este problema pois os 3 anteriores foram discretizados) sobrando apenas o seguintes:
summary(arq_german_credit_data[c("PercentRend", "ResidAtual", "NumEmpAtivo", "NumPessoas")])

#Quantizar atributos
## Funcao para converter variaveis numericas para fator
quantize.num <- function(x, nlevs = 5, maxval = 1000, 
                         minval = 0, ordered = TRUE){
  cuts <- seq(min(x), max(x), length.out = nlevs + 1)
  cuts[1] <- minval
  cuts[nlevs + 1] <- maxval
  print(cuts)
  x <- cut(x, breaks = cuts, order_result = ordered)
}
var <- arq_german_credit_data$Idade
var <- arq_german_credit_data$MesesCc
var <- arq_german_credit_data$MontEmp
length(table(var))
min(var)
max(var)
quantize.num(var, 5, maxval = max(var), min(var), FALSE)
quantize.num(var, 5, maxval = 100, 0, FALSE)


################# ETAPA 03: Separando conjunto Treino e Teste #################
#MODIFICADO PARA FAZER ESTA ETAPA JUNTO COM TREINAMENTO
#Exemplo 1:
#dados_treino <- arq_german_credit_data[1:700, ]
#glimpse(dados_treino)
#dados_teste <- arq_german_credit_data[701:1000, ]
#glimpse(dados_teste)
#Exemplo 2:
# Dividindo base de dados (Treino e Teste) de forma randomica
#install.packages("caTools")
#library(caTools)
#set.seed(101) 
#amostra <- sample.split(arq_german_credit_data, SplitRatio = 0.80)
# ***** Treinamos nosso modelo nos dados de treino *****
# *****   Fazemos as predicoes nos dados de teste  *****
#dados_treino = subset(arq_german_credit_data, amostra == TRUE)
#glimpse(dados_treino)
#dados_teste = subset(arq_german_credit_data, amostra == FALSE)
#glimpse(dados_teste)

################# ETAPA 04: Criando o modelo #################
# Usando a funcao rpart para criar a arvore de decisao
#install.packages("rpart")
#library(rpart)
#modelo_arvore <- rpart(class ~ . , method = 'class', data = dados_treino)
#class(modelo_arvore)
#modelo_arvore

#printcp(modelo_arvore) # Para examinar o resultado de uma arvore de decisao, existem diversas funcoes
#plot(modelo_arvore, uniform = TRUE, main = "Arvore de Decisao em R")
#text(modelo_arvore, use.n = TRUE, all = TRUE)
#install.packages('rpart.plot') # Este outro pacote faz a visualizaco ficar mais legivel
#library(rpart.plot)
#prp(modelo_arvore)

################# ETAPA 05: Avaliando o modelo #################
## Predict como funcao para trazer a probabilidade do cliente ser mau/bom
#test_tree_predict = predict(modelo_arvore, newdata = dados_teste);
#head(test_tree_predict)

## Predict com tipo 'classe' retorna se é bom ou mau
#test_tree_predict = predict(modelo_arvore, newdata = dados_teste, type = "class");
#head(test_tree_predict)

#install.packages("gmodels")
#library(gmodels)
#CrossTable(x = test_tree_predict, y = dados_teste$class, prop.chisq = FALSE) ## confusion matrix
#Linha representa a classificação correta
#Coluna representa a classificação do modelo
#172 disse que era Good e realmente eram Good (acertou)
#49 disse que era Good mas eram Bad (errou)
#35 disse que era Bad mas eram Good (errou)
#44 disse que era Bad e realmente eram Bad (acertou)

#Calculando a taxa de erro
#taxa_erro = mean(dados_teste$class != test_tree_predict)*100
#taxa_acerto = mean(dados_teste$class == test_tree_predict)*100
#taxa_acerto
#taxa_erro

#prop.table(table(test_tree_predict))

#### Avaliando Modelo DataScience #####
#result_previsto <- data.frame( actual = dados_teste$class,
#                               previsto = predict(modelo_arvore, newdata = dados_teste, type = "class")
#)
#head(result_previsto)
#função manual para criar a matriz de confusão
#confusionMatrix <- matrix(unlist(Map(function(x, y){sum(ifelse(result_previsto[, 1] == x & result_previsto[, 2] == y, 1, 0) )},
#                                     c("Good", "Bad", "Good", "Bad"), c("Good", "Good", "Bad", "Bad"))), nrow = 2)
#confusionMatrix

## Criando um dataframe com as estatisticas dos testes
#df_mat <- data.frame( Category = c("Credito Ruim", "Credito Bom"),
#                      Classificado_como_ruim = c(confusionMatrix[1,1], confusionMatrix[2,1]),
#                      Classificado_como_bom = c(confusionMatrix[1,2], confusionMatrix[2,2]),
#                      Accuracy_Recall = c(Accuracy(confusionMatrix), Recall(confusionMatrix)),
#                      Precision_WAcc = c(Precision(confusionMatrix), W_Accuracy(confusionMatrix)))

#print(df_mat)

# Gerando uma curva ROC em R
#install.packages("ROCR")
#install.packages("gplots")
#library("ROCR")
#library("gplots")

# Gerando as classes de dados
#class1 <- predict(modelo_arvore, newdata = dados_teste, type = 'prob')
#class2 <- dados_teste$class

# Gerando a curva ROC
#?prediction
#?performance
#pred <- prediction(class1[,2], class2)
#perf <- performance(pred, "fpr", "tpr") 
#plot(perf, col = rainbow(10))

# Gerando Confusion Matrix com o Caret
#library(caret)
#?confusionMatrix
#confusionMatrix(result_previsto$actual, result_previsto$previsto)

################# ETAPA 06: Seleção de características #################

#Feature Selection: Remover do dataSet  as variáveis que não serão úteis pra a criação do modelo preditivo
#Métodos:
#1. Teste do Qui-quadrado
#2. Coeficiente de correlação
#3. Algoritmos de Eliminação Recursiva
#4. Algoritmos de regularização (Lasso, Elastic, Net, Ridge Regression)
#Feature selection <> Redução de dimensionalidade
#(Redução de dimensionalidade = Criação de novas combinações dos atributos)
#(Feature selection = Calcular o nível de significância de cada variável e eliminar aquelas com significância mais baixa)
# Criando um modelo randomForest para calcular a significância de cada variável

# Criando um modelo para identificar os atributos com maior importancia para o modelo preditivo
#require(randomForest)
#library(randomForest)

glimpse(mod_arq_german_credit_data) # Avalidando a importância de todas as variaveis
atributosGerman <- randomForest(class ~ . ,
                                   data = mod_arq_german_credit_data,
                                   ntree = 300, nodesize = 10,
                                   importance = TRUE)

varImpPlot(atributosGerman) # Plotando as variaveis por grau de importancia
varImpPlot(atributosReal)

# Removendo variaveis colineares
modelo_atributos_2 <- randomForest(class ~ . - NumPessoas
                                   - NumEmpAtivo
                                   - PercentRend
                                   - Telefone
                                   - TrabEstr
                                   - OutrosDev
                                   - TipoMoradia
                                   - OutrosParc
                                   - TipoTrab
                                   - ResidAtual
                                   - EstCivil, 
                                   data = dados_norm, 
                                   ntree = 100, nodesize = 10,
                                   importance = TRUE)

varImpPlot(modelo_atributos_2) # Plotando as variaveis por grau de importancia

# Gravando o resultado
df_saida <- bikes[, c("cnt", rownames(modelo$importance))]

############# Formulas ################
Accuracy <- function(x){
  (x[1,1] + x[2,2]) / (x[1,1] + x[1,2] + x[2,1] + x[2,2])
}

Recall <- function(x){  
  x[1,1] / (x[1,1] + x[1,2])
}

Precision <- function(x){
  x[1,1] / (x[1,1] + x[2,1])
}

W_Accuracy  <- function(x){
  (x[1,1] + x[2,2]) / (x[1,1] + 5 * x[1,2] + x[2,1] + x[2,2])
}

F1 <- function(x){
  2 * x[1,1] / (2 * x[1,1] + x[1,2] + x[2,1])
}

AUC <- function(x){
  0.5 * ( 1 + (x[1,1]/(x[1,1]+x[1,2])) * (x[2,1]/(x[2,1]+x[2,2])) ) 
  #* x[1,1] / (2 * x[1,1] + x[1,2] + x[2,1])
}
#CT$t[1,1] #VP
#CT$t[1,2] #FN
#CT$t[2,1] #FP
#CT$t[2,2] #VN
#Accuracy(confusionMatrix)
#Recall(confusionMatrix)
#Precision(confusionMatrix)
#W_Accuracy(confusionMatrix)
#F1(confusionMatrix)
#AuxMat <- matrix(nrow = 10, ncol = 2)
#matr

################# RESUMINDO DECISION TREE: Treinando e Testando Modelo #################INICIO
#arq_german_credit_data <- read_csv("german-credit-data.csv") #Carrega dados
#arq_german_credit_data$class <- factor(arq_german_credit_data$class, levels = c(1,2), labels = c("Good", "Bad")) #Classe como fator
#set.seed(101) 
#AuxMat <- matrix(nrow = 10, ncol = 2)
#for (i in 1:10) {
#amostra <- sample.split(arq_german_credit_data, SplitRatio = 0.50) #Split dataSet
#dados_treino = subset(arq_german_credit_data, amostra == TRUE) #DataSet Treino
#dados_teste = subset(arq_german_credit_data, amostra == FALSE) #DataSet Testte
#modelo_arvore <- rpart(class ~ . , method = 'class', data = dados_treino) #Treino Modelo
#test_tree_predict = predict(modelo_arvore, newdata = dados_teste, type = "class"); #Testa Modelo
#CT <- CrossTable(x = test_tree_predict, y = dados_teste$class, prop.chisq = FALSE) ## confusion matrix
#acerto <- mean(dados_teste$class == test_tree_predict)*100 #Calculando a taxa de acerto
#erro <- mean(dados_teste$class != test_tree_predict)*100 #Calculando a taxa de erro
#AuxMat[i,1] = acerto
#AuxMat[i,2] = erro
#}
#AuxMat
#prp(modelo_arvore)

#??CrossTable
## classes para gerar curva ROC
#class1 <- predict(modelo_arvore, newdata = dados_teste, type = 'prob')
#class2 <- dados_teste$class
#pred <- prediction(class1[,2], class2)
#perf <- performance(pred, "fpr", "tpr") 
#plot(perf, col = rainbow(10))

## Matriz de confusão manual
#result_previsto <- data.frame( actual = dados_teste$class,
#                               previsto = predict(modelo_arvore, newdata = dados_teste, type = "class")
#)
#confusionMatrix <- matrix(unlist(Map(function(x, y){sum(ifelse(result_previsto[, 1] == x & result_previsto[, 2] == y, 1, 0) )},
#                                     c("Good", "Bad", "Good", "Bad"), c("Good", "Good", "Bad", "Bad"))), nrow = 2)
## Criando um dataframe com as estatisticas dos testes
#df_mat <- data.frame( Category = c("Credito Ruim", "Credito Bom"),
#                      Classificado_como_ruim = c(confusionMatrix[1,1], confusionMatrix[2,1]),
#                      Classificado_como_bom = c(confusionMatrix[1,2], confusionMatrix[2,2]),
#                      Accuracy_Recall = c(Accuracy(confusionMatrix), Recall(confusionMatrix)),
#                      Precision_WAcc = c(Precision(confusionMatrix), W_Accuracy(confusionMatrix)))

#print(df_mat)
################# RESUMINDO DECISION TREE: Treinando e Testando Modelo #################FIM
################# RESUMINDO RANDOM FOREST: Treinando e Testando Modelo #################INICIO

## Otimizando o modelo && utilizando Floresta
# Criando uma Cost Function
#install.packages("randomForest")
#install.packages("C50")
#library(randomForest)
#library(C50)
#Cost_func <- matrix(c(0.0, 1.5, 1.0, 0.0), nrow = 2, dimnames = list(c("Good", "Bad"), c("Good", "Bad")))
#modelo_floresta <- C5.0( class ~ StatusCc
#                         + MesesCc
#                         + HistCredit
#                         + PropEmp
#                         + MontEmp
#                         + Invest
#                         + TrabAtual
#                         + PercentRend
#                         + EstCivil
#                         + OutrosDev
#                         + ResidAtual
#                         + TipoResid
#                         + Idade
#                         + OutrosParc
#                         + TipoMoradia
#                         + NumEmpAtivo
#                         + TipoTrab
#                         + NumPessoas
#                         + Telefone
#                         + TrabEstr, 
#                         data = dados_treino, 
#                         ntree = 100, nodesize = 10) #, cost = Cost_func)
#print(modelo_floresta)

## classes para gerar curva ROC
#class1F <- predict(modelo_floresta, newdata = dados_teste, type = 'prob')
#class2F <- dados_teste$class
#predF <- prediction(class1F[,2], class2F)
#perfF <- performance(predF, "tpr", "fpr") 
#plot(perfF, col = rainbow(10))

## Matriz de confusão manual
#result_previstoF <- data.frame( actual = dados_teste$class,
#                                previsto = predict(modelo_floresta, newdata = dados_teste, type = "class")
#)
#confusionMatrixF <- matrix(unlist(Map(function(x, y){sum(ifelse(result_previstoF[, 1] == x & result_previstoF[, 2] == y, 1, 0) )},
#                                     c("Good", "Bad", "Good", "Bad"), c("Good", "Good", "Bad", "Bad"))), nrow = 2)
## Criando um dataframe com as estatisticas dos testes
#df_matF <- data.frame( Category = c("Credito Ruim", "Credito Bom"),
#                      Classificado_como_ruimF = c(confusionMatrixF[1,1], confusionMatrixF[2,1]),
#                      Classificado_como_bomF = c(confusionMatrixF[1,2], confusionMatrixF[2,2]),
#                      Accuracy_RecallF = c(Accuracy(confusionMatrixF), Recall(confusionMatrixF)),
#                      Precision_WAccF = c(Precision(confusionMatrixF), W_Accuracy(confusionMatrixF)))

#print(df_matF)

################
#plot(perf, col = rainbow(10))
#plot(perfF, col = rainbow(10))
#print(df_mat)
#print(df_matF)
#library(caret)
#confusionMatrix(result_previsto$actual, result_previsto$previsto)
#confusionMatrix(result_previstoF$actual, result_previstoF$previsto)
################# RESUMINDO RANDOM FOREST: Treinando e Testando Modelo #################FIM


################# MODELO RANDOM FOREST: Treinando e Testando Modelo #################INICIO
library(C50)
AuxMatRF <- matrix(nrow = 13, ncol = 11)
dimnames(AuxMatRF) = (list( c("T01","T02","T03","T04","T05","T06","T07","T08","T09","T10","Média","Mediana","DesvioP"),
                          c("acertos","erros","VP","FN","FP","VN","Precisão","recall","acuracia","F1","AUC")))
head(AuxMatRF)
glimpse(arq_german_credit_data)
glimpse(mod_arq_german_credit_data)
glimpse(dados_norm)

dataset <- arq_german_credit_data
Cost_func <- matrix(c(0.0, 4.5, 3.0, 0.0), nrow = 2, dimnames = list(c("Good", "Bad"), c("Good", "Bad")))
for (i in 1:10) {
  #set.seed(6374) 
  amostra <- sample.split(dataset, SplitRatio = 0.90) #Split dataSet
  dados_treino = subset(dataset, amostra == TRUE) #DataSet Treino
  dados_teste = subset(dataset, amostra == FALSE) #DataSet Testte
  #forest_model <- C5.0( class ~ ., data = dados_treino, ntree = 100, nodesize = 10, cost = Cost_func)
  #forest_model <- C5.0( class ~ ., data = dados_treino, ntree = 750, nodesize = 10)
  forest_model <- C5.0( class ~ + StatusCc
                                 + MesesCc
                                 + HistCredit
                                 + MontEmp
                                 + Invest
                                 + OutrosDev
                                 + TipoResid
                                 + OutrosParc
                                 + Idade
                                 + PropEmp,
                        #         + PercentRend
                        #         + TrabAtual
                        #         + ResidAtual
                        #         + TipoMoradia,
                        data = dados_treino, ntree = 100, nodesize = 10)
  test_forest_predict = predict(forest_model, newdata = dados_teste, type = "class"); #Testa Modelo
  CT <- CrossTable(x = test_forest_predict, y = dados_teste$class, prop.chisq = FALSE) ## confusion matrix
  acerto <- mean(dados_teste$class == test_forest_predict)*100 #Calculando a taxa de acerto
  erro <- mean(dados_teste$class != test_forest_predict)*100 #Calculando a taxa de erro
  AuxMatRF[i,1] = acerto
  AuxMatRF[i,2] = erro
  AuxMatRF[i,3] = CT$t[1,1] #VP
  AuxMatRF[i,4] = CT$t[1,2] #FN
  AuxMatRF[i,5] = CT$t[2,1] #FP
  AuxMatRF[i,6] = CT$t[2,2] #VN
  AuxMatRF[i,7] = Precision(CT$t)
  AuxMatRF[i,8] = Recall(CT$t)
  AuxMatRF[i,9] = Accuracy(CT$t)
  AuxMatRF[i,10] = F1(CT$t)
  AuxMatRF[i,11] = AUC(CT$t)
}
for (j in 1:11) {
  AuxMatRF[i+1,j] = mean(AuxMatRF[1:10,j])
  AuxMatRF[i+2,j] = median(AuxMatRF[1:10,j])
  AuxMatRF[i+3,j] = sd(AuxMatRF[1:10,j])
}
AuxMatRF
View(AuxMatRF)
#View(AuxMatRF1 <- AuxMatRF)
#View(AuxMatRF2 <- AuxMatRF)
#View(AuxMatRF3 <- AuxMatRF)

class1F <- predict(forest_model, newdata = dados_teste, type = 'prob')
class2F <- dados_teste$class
predF <- prediction(class1F[,2], class2F)
perfF <- performance(predF, "tpr", "fpr") 
plot(perfF, col = rainbow(10))


################# MODELO RANDOM FOREST: Treinando e Testando Modelo #################INICIO
################# MODELO REDE DE CRENÇA PROFUNDA: Treinando e Testando Modelo #################INICIO
library(darch)

AuxMatDBN <- matrix(nrow = 13, ncol = 11)
dimnames(AuxMatDBN) = (list( c("T01","T02","T03","T04","T05","T06","T07","T08","T09","T10","Média","Mediana","DesvioP"),
                            c("acertos","erros","VP","FN","FP","VN","Precisão","recall","acuracia","F1","AUC")))
head(AuxMatDBN)
glimpse(arq_german_credit_data)
glimpse(mod_arq_german_credit_data)
glimpse(dados_norm)

dataset <- dados_norm
for (i in 1:10) {
  amostra <- sample.split(dataset, SplitRatio = 0.70) #Split dataSet
  dados_treino = subset(dataset, amostra == TRUE) #DataSet Treino
  dados_teste = subset(dataset, amostra == FALSE) #DataSet Testte
  #dbn_model <- darch(class ~ ., data = dados_treino, layers = c(20, 10, 2),
  #                   darch.numEpochs = 100, darch.stopClassErr = 0, retainData = T) #Treino Modelo
  dbn_model <- darch(class ~ + StatusCc
                             + MesesCc
                             + HistCredit
                             + Invest
                             + MontEmp
                             + OutrosDev
                             + OutrosParc
                             + Idade, data = dados_treino, layers = c(20, 10, 2),
                     darch.numEpochs = 5, darch.stopClassErr = 0, retainData = T,
                     darch.layerFunctionDefault = "sigmoid",
                     rbm.numCD = 10
                     #rbm.numEpochs = 20
                     ) #Treino Modelo
  
  test_dbn_predict <- predict(dbn_model, newdata = dados_teste, type = "class"); #Testa Modelo
  CT <- CrossTable(x = test_dbn_predict, y = dados_teste$class, prop.chisq = FALSE) ## confusion matrix
  acerto <- mean(dados_teste$class == test_dbn_predict)*100 #Calculando a taxa de acerto
  erro <- mean(dados_teste$class != test_dbn_predict)*100 #Calculando a taxa de erro
  AuxMatDBN[i,1] = acerto
  AuxMatDBN[i,2] = erro
  AuxMatDBN[i,3] = CT$t[1,1] #VP
  AuxMatDBN[i,4] = CT$t[1,2] #FN
  AuxMatDBN[i,5] = CT$t[2,1] #FP
  AuxMatDBN[i,6] = CT$t[2,2] #VN
  AuxMatDBN[i,7] = Precision(CT$t)
  AuxMatDBN[i,8] = Recall(CT$t)
  AuxMatDBN[i,9] = Accuracy(CT$t)
  AuxMatDBN[i,10] = F1(CT$t)
  AuxMatDBN[i,11] = AUC(CT$t)
}
for (j in 1:11) {
  AuxMatDBN[i+1,j] = mean(AuxMatDBN[1:10,j])
  AuxMatDBN[i+2,j] = median(AuxMatDBN[1:10,j])
  AuxMatDBN[i+3,j] = sd(AuxMatDBN[1:10,j])
}
AuxMatDBN
View(AuxMatDBN)

class1F <- predict(dbn_model, newdata = dados_teste)
class2F <- dados_teste$class
predF <- prediction(class1F[,2], class2F)
perfF <- performance(predF, "tpr", "fpr") 
plot(perfF, col = rainbow(10))


################# MODELO REDE DE CRENÇA PROFUNDA: Treinando e Testando Modelo #################FIM

################# RESUMINDO DEEP LEARNING #################
#install.packages("deepnet") # deepnet
#library(deepnet)


#Example 01 DNN
#Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
#Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
#x <- matrix(c(Var1, Var2), nrow = 100, ncol = 2)
#y <- c(rep(1, 50), rep(0, 50))
#dnn <- dbn.dnn.train(x, y, hidden = c(5, 5))

#test_Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
#test_Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
#test_x <- matrix(c(test_Var1, test_Var2), nrow = 100, ncol = 2)
#dnn
#test_x
#y
#nn.test(dnn, test_x, y, t=0.5)
#??nn.test
#nn.predict(dnn, test_x)

#Teste 01 DNN
#as.matrix(dados_treino[c("MesesCc", "MontEmp", "Idade")])
#as.matrix(dados_teste[c("MesesCc", "MontEmp", "Idade")])
#dnn <- dbn.dnn.train(as.matrix(dados_treino[c("MesesCc", "MontEmp", "Idade")]), dados_treino$class, hidden = c(5, 5))
#dnn

#test_x <- matrix(as.matrix(dados_teste[c("MesesCc", "MontEmp", "Idade")]), nrow = 238, ncol = 3)
#nn.test(dnn, test_x, dados_teste$class)

#Example 02 RBM (Restricted Boltzmann Machine)
#Var1 <- c(rep(1, 50), rep(0, 50))
#Var1
#Var2 <- c(rep(0, 50), rep(1, 50))
#Var2
#x3 <- matrix(c(Var1, Var2), nrow = 100, ncol = 2)
#x3
#r1 <- rbm.train(x3, 3, numepochs = 20, cd = 10)
#r1
#v <- c(0.2, 0.8)
#v
#h <- rbm.up(r1, v)
#h

 #Teste 02 RBM (Restricted Boltzmann Machine)
#Var1 <- c(rep(1, 50), rep(0, 50))
#Var2 <- c(rep(0, 50), rep(1, 50))
#x3 <- matrix(as.matrix(dados_teste[c("MesesCc", "MontEmp", "Idade")]), nrow = 238, ncol = 3)
#r1 <- rbm.train(x3, 3, numepochs = 20, cd = 10)
#r1
#v <- c(0.2, 0.8)
#h <- rbm.up(r1, v)


################# RESUMINDO DEEP LEARNING (DARCH) #################
#install.packages("darch") # deepnet
#library(darch)

## Not run:
#data(iris)
#model <- darch(Species ~ ., iris)
#print(model)
#plot(model)
#predictions <- predict(model, newdata = iris, type = "class")
#cat(paste("Incorrect classifications:", sum(predictions != iris[,5])))
#trainData <- matrix(c(0,0,0,1,1,0,1,1), ncol = 2, byrow = TRUE)
#trainTargets <- matrix(c(0,1,1,0), nrow = 4)
#model2 <- darch(trainData, trainTargets, layers = c(2, 10, 1),
#                darch.numEpochs = 500, darch.stopClassErr = 0, retainData = T)
#e <- darchTest(model2)
#cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
#           e[2], "%)\n"))
#plot(model2)
## End(Not run)

#modelo_arvore <- darch(class ~ . , method = 'class', data = dados_treino) #Treino Modelo
#print(modelo_arvore)
#plot(modelo_arvore)

#dbn_model <- darch(dados_treino[c("MesesCc", "MontEmp", "Idade", "class")],
#                        dados_treino$class,
#                        method = 'class',
#                        layers = c(3, 10, 2),
#                        darch.numEpochs = 100,
#                        darch.stopClassErr = 0,
#                        retainData = T)
#e <- darchTest(dbn_model)
#print(dbn_model)
#plot(dbn_model)
#e
#test_tree_predict = predict(dbn_model, newdata = dados_teste[c("MesesCc", "MontEmp", "Idade", "class")], type = "class"); #Testa Modelo

#CT <- CrossTable(x = test_tree_predict, y = dados_teste$class, prop.chisq = FALSE) ## confusion matrix
#acerto <- mean(dados_teste$class == test_tree_predict)*100 #Calculando a taxa de acerto
#erro <- mean(dados_teste$class != test_tree_predict)*100 #Calculando a taxa de erro
#AuxMat[i,1] = acerto
#AuxMat[i,2] = erro
#acerto
#erro


################# REDE DE CRENÇA PROFUNDA: Treinando e Testando Modelo #################
#arq_german_credit_data <- read_csv("german-credit-data.csv") #Carrega dados
#arq_german_credit_data$class <- factor(arq_german_credit_data$class, levels = c(1,2), labels = c(1, 2)) #Classe como fator
#set.seed() 
#AuxMat <- matrix(nrow = 10, ncol = 2)
#for (i in 1:10) {
#  amostra <- sample.split(arq_german_credit_data[c("MesesCc", "MontEmp", "Idade", "class")], SplitRatio = 0.80) #Split dataSet
#  dados_treino = subset(arq_german_credit_data[c("MesesCc", "MontEmp", "Idade", "class")], amostra == TRUE) #DataSet Treino
#  dados_teste = subset(arq_german_credit_data[c("MesesCc", "MontEmp", "Idade", "class")], amostra == FALSE) #DataSet Testte
#  dbn_model <- darch(class ~ .,
#                   method = 'class',
#                     data = dados_treino,
#                     #x = dados_treino,
#                     #y = dados_treino$class,
#                     layers = c(4, 100, 3), #Número de neurônios de cada camada
#                     darch.numEpochs = 50,
#                     darch.stopClassErr = 0,
#                     retainData = T#,
  #                   bp.learnRate = .1,
#                     darch.isClass = T, #Se a saída deve ser tratada como rótulos de classe durante o ajuste fino e taxas de classificação devem ser impresso.
 #                    darch.fineTuneFunction = "backpropagation"
#                    ) #Treino Modelo
#  test_dbn_predict = predict(dbn_model, newdata = dados_teste, type = "class"); #Testa Modelo
#  CT <- CrossTable(x = test_dbn_predict, y = dados_teste$class, prop.chisq = FALSE) ## confusion matrix
#  mean(dados_teste$class == test_dbn_predict)*100 #Calculando a taxa de acerto
#  mean(dados_teste$class != test_dbn_predict)*100 #Calculando a taxa de erro

#  AuxMat[i,1] = acerto
#  AuxMat[i,2] = erro
#}
#AuxMat
#prp(dbn_model)

## R=un:############# TESTANDO UMA REDE DE CRENÇA PROFUNDA (IRIS)##
glimpse(iris)
#Split exemplo 1
irisMod = subset(iris, Species != 'virginica')
amostra <- sample.split(irisMod, SplitRatio = 0.50) #Split dataSet
dados_treino = subset(irisMod, amostra == TRUE) #DataSet Treino
dados_teste = subset(irisMod, amostra == FALSE) #DataSet Teste
glimpse(dados_treino)
glimpse(dados_teste)
model <- darch(Species ~ ., dados_treino, layers = c(4, 10, 2),
               darch.numEpochs = 100, retainData = T)
predictions <- predict(model, newdata = dados_teste, type = "class")

CT <- CrossTable(x = predictions, y = dados_teste$Species, prop.chisq = FALSE) ## confusion matrix
CT
## End(R=un)




######## Testes estatísticos T-Student ##############

precisao_rf_german <- c(0.8408163,0.8553719,0.8523207,
                        0.8510638,0.8609866,0.8281938,
                        0.8709677,0.8051948,0.9173913,
                        0.8781513)

precisao_dbn_german <- c(0.904762,0.870293,0.901786,
                         0.792373,0.873418,0.876068,
                         0.873874,0.905579,0.837004,
                         0.828452)

acuracia_rf_german <- c(0.7544910,0.7514970,0.7327327,
                        0.6955224,0.7048193,0.6886228,
                        0.7297297,0.6916168,0.7349398,
                        0.7417417)

acuracia_dbn_german <- c(0.773134,0.766467,0.715569,
                         0.751497,0.756757,0.746988,
                         0.757485,0.734328,0.738739,
                         0.739521)
t.test(precisao_rf_german, precisao_dbn_german, alternative = 'less')
t.test(acuracia_rf_german, acuracia_dbn_german, alternative = 'less')

#intervalo de confiança - variabilidade
var.test(precisao_rf_german,precisao_dbn_german, alternative = 'two.sided')
var.test(acuracia_rf_german,acuracia_dbn_german, alternative = 'two.sided')

# Verificar se são normais 
qqplot(precisao_rf_german); qqline(precisao_rf_german)
qqplot(precisao_dbn_german); qqline(precisao_dbn_german)

# Verificar se as variâncias são iguais
var(precisao_rf_german) ; var(precisao_dbn_german)
var.test(precisao_rf_german, precisao_dbn_german)

# Teste t
t.test(precisao_rf_german, precisao_dbn_german, alternative = 'two.sided', conf.level = 0.45)
t.test(acuracia_rf_german, acuracia_dbn_german, alternative = 'two.sided')
