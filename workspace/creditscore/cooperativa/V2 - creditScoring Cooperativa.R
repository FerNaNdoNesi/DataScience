install.packages("readr") # Usando o pacote readr
install.packages("dplyr") #tratamento de dados em memória
install.packages("tidyr") # Objetivo de remodelar os dados
install.packages("caTools") #dividir os conjuntos de dados
install.packages("rpart.plot") # visualizaco da arvore ficar mais legivel
install.packages("gmodels") # Matriz de confusão
install.packages("ROCR") #criar curva ROC
install.packages("gplots")
install.packages("randomForest") #criação do modelo para filter selection
library(readr) 
library(dplyr)
library(tidyr)
library(caTools)
library(rpart.plot)
library(gmodels)
library(ROCR)
library(gplots)
library(randomForest)

print("Credit Scoring German Data Set")

setwd("~/git/dataScience/workspace/creditscore/cooperativa")

################ Conjunto de dados ################
arq_coop_credit_data <- read_csv("dataSet_treino_final.csv")
glimpse(arq_coop_credit_data) #Visualização rápida
table(arq_coop_credit_data$class)
prop.table(table(arq_coop_credit_data$class)) #verifica proporção

dados0 <- subset(arq_coop_credit_data, class == 0)
dados1 <- subset(arq_coop_credit_data, class == 1)
table(dados0$class)
table(dados1$class)
dados0 <- dados0[1:7000, ]
dados1 <- dados1[1:3000, ]
dataSet <- rbind(dados0, dados1)
glimpse(dataSet)
table(dataSet$class)
prop.table(table(dataSet$class)) #verifica proporção
################ Conjunto de dados ################

#arq_coop_credit_data <- read_csv("coop-dataset.csv")
#arq_coop_credit_data <- read_csv("mod_dataSet01.csv")
#arq_coop_credit_data <- read_csv("mod_dataSet01_equ.csv")
#orig <- read_csv("orig.csv")
#glimpse(arq_coop_credit_data) #Visualização rápida
#glimpse(orig) #Visualização rápida
#dimnames(orig) = labels(arq_coop_credit_data)
#arq_coop_credit_data <- orig
#glimpse(arq_coop_credit_data)

######################### ETAPA 01: Limpeza dos dados #########################
any(is.na(dataSet)) # se existir valores nulos é necessário trata-los
any(is.na(dataSet$NumDep))
any(is.na(dataSet$Renda))


summary(dataSet$NumDep) #média 0
prop.table(table(dataSet$NumDep))
dataSet$NumDep[is.na(dataSet$NumDep)] <- 0 #0.5814 -> 0.5920 percent de 0

summary(dataSet$Renda) #média: 5081
prop.table(table(dataSet$Renda))
dataSet$Renda[is.na(dataSet$Renda)] <- 5081 #NA's 1903

glimpse(dataSet)
######################### Valores numéricos em categóricos ######################### 

#Quantizar atributos
## Funcao para converter variaveis numericas para fator
quantize.num <- function(x, nlevs = 10, maxval = 1000, 
                         minval = 0, ordered = TRUE){
  cuts <- seq(min(x), max(x), length.out = nlevs + 1)
  cuts[1] <- minval
  cuts[nlevs + 1] <- maxval
  print(cuts)
  x <- cut(x, breaks = cuts, order_result = ordered)
}
#toFactors <- c("Renda", "PercentRend", "Idade", "Carteira")
#maxVals <- c(1000000, 1000000, 100, 1000000)
#facNames <- unlist(lapply(toFactors, function(x) paste(x, "_f", sep = "")))
#dataSet[, facNames] <- Map(function(x, y) quantize.num(dataSet[, x], maxval = y), toFactors, maxVals)

glimpse(dataSet)
table(dataSet$class)
prop.table(table(dataSet$class)) # 0.75 0.25
## Encontrando os atributos com muitos valores distintos
lapply(dataSet, table)
lapply(lapply(dataSet, table),prop.table)
lapply(lapply(dataSet, table),length)
#4 atributos encontrados
lapply(lapply(dataSet[c("Renda", "PercentRend", "Idade", "Carteira")], table),length)
summary(dataSet[c("Renda", "PercentRend", "Idade", "Carteira")])


## Fazendo a quantização destes atributos - Não funcionando corretamente ##
##Map(function(x, y) quantize.num(dataSet$PercentRend, minval = min(dataSet$PercentRend), maxval = max(dataSet$PercentRend), nlevs = 10), toFactors, maxVals)
#dataSet$PercentRend <- quantize.num(dataSet$PercentRend, minval = min(dataSet$PercentRend), maxval = max(dataSet$PercentRend), ordered = FALSE, nlevs = 10)
#dataSet$Carteira <- quantize.num(dataSet$Carteira)
#dataSet$Renda <- quantize(dataSet$Renda)
#dataSet$Idade <- quantize(dataSet$Idade)
#lapply(lapply(dataSet[c("Renda", "PercentRend", "Idade", "Carteira")], table),length)
#lapply(lapply(dataSet, table),length)


## convertendo novamente para númericos - Não funcionando corretamente ##
#factor(dataSet$Renda, levels = levels(dataSet$Renda), labels = c(1:length(table(dataSet$Renda))))
#factor(dataSet$PercentRend, levels = levels(dataSet$PercentRend), labels = c(1:length(table(dataSet$PercentRend))))
#factor(dataSet$Idade, levels = levels(dataSet$Idade), labels = c(1:length(table(dataSet$Idade))))
#factor(dataSet$Carteira, levels = levels(dataSet$Carteira), labels = c(1:length(table(dataSet$Carteira))))

####################### ETAPA 02: Análise exploratória ########################

#1 - atributos devem ser do tipo double
#Ajuste convertendo para double
dataSet$Idade <- as.double(dataSet$Idade)
dataSet$ParcMin <- as.double(dataSet$ParcMin)
dataSet$EmpAtiv <- as.double(dataSet$EmpAtiv)
dataSet$ParcMax <- as.double(dataSet$ParcMax)
dataSet$Invest <- as.double(dataSet$Invest)
dataSet$ParcMid <- as.double(dataSet$ParcMid)
glimpse(dataSet)

#2 - Criando um segundo dataSet normalidado
# Normalizando conjunto de dados
dados_norm <- as.data.frame(lapply(dataSet, scale))
glimpse(dados_norm)

#3 - Atributo classe deve ser do tipo Fator (nos dois dataSet)
dataSet$class <- factor(dataSet$class, levels = c(0,1), labels = c("Bom", "Ruim")) #Classe como fator
dados_norm$class <- dataSet$class 

glimpse(dataSet)
prop.table(table(dataSet$class))

glimpse(dados_norm)
prop.table(table(dados_norm$class))

################# ETAPA 03: Seleção de características #################

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

glimpse(dataSet)
# Avalidando a importância de todas as variaveis
modelo_atributos_1 <- randomForest(class ~ . ,
                          data = dataSet, ntree = 100, nodesize = 10,
                          importance = TRUE)

varImpPlot(modelo_atributos_1) # Plotando as variaveis por grau de importancia

# Removendo variaveis colineares
modelo_atributos_2 <- randomForest(class ~ . - NumDep
                       - EmpAtiv
                       - Invest, 
                       data = dataSet, 
                       ntree = 100, nodesize = 10,
                       importance = TRUE)

varImpPlot(modelo_atributos_2) # Plotando as variaveis por grau de importancia

# Gravando o resultado
df_saida <- bikes[, c("cnt", rownames(modelo$importance))]

################# ETAPA 04: Criando o modelo #################

## Separando conjunto Treino e Teste ##
##Exemplo 1:
#dados_treino <- arq_german_credit_data[1:700, ]
#glimpse(dados_treino)
#dados_teste <- arq_german_credit_data[701:1000, ]
#glimpse(dados_teste)

##Exemplo 2: Dividindo base de dados (Treino e Teste) de forma randomica
#install.packages("caTools")
#library(caTools)

#amostra <- sample.split(dataSet, SplitRatio = 0.70)
#dados_treino = subset(dataSet, amostra == TRUE)
#dados_teste = subset(dataSet, amostra == FALSE)

#Fazendo criação dos modelos ao fim do script (Random Forest)
#Fazendo criação dos modelos ao fim do script (DBN)
################# ETAPA 05: Avaliando o modelo #################

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

################# MODELO RANDOM FOREST: Treinando e Testando Modelo #################INICIO
library(C50)

glimpse(dataSet)
table(dataSet$class) #Bom Ruim #7000   3000

AuxMat <- matrix(nrow = 13, ncol = 11)
dimnames(AuxMat) = (list( c("T01","T02","T03","T04","T05","T06","T07","T08","T09","T10","Média","Mediana","DesvioP"),
                          c("acertos","erros","VP","FN","FP","VN","Precisão","recall","acuracia","F1","AUC")))
dataset <- dataSet
Cost_func <- matrix(c(0.0, 4.5, 3.0, 0.0), nrow = 2, dimnames = list(c("Good", "Bad"), c("Good", "Bad")))
for (i in 1:10) {
  #set.seed(6374) 
  amostra <- sample.split(dataset, SplitRatio = 0.70) #Split dataSet
  dados_treino = subset(dataset, amostra == TRUE) #DataSet Treino
  dados_teste = subset(dataset, amostra == FALSE) #DataSet Teste
  #modelo_floresta <- C5.0( class ~ . - NumDep - EmpAtiv - Invest, data = dados_treino, ntree = 750, nodesize = 10)
  modelo_floresta <- C5.0( class ~ ., data = dados_treino, ntree = 750, nodesize = 10)
  test_forest_predict = predict(modelo_floresta, newdata = dados_teste, type = "class"); #Testa Modelo
  CT <- CrossTable(x = test_forest_predict, y = dados_teste$class, prop.chisq = FALSE) ## confusion matrix
  acerto <- mean(dados_teste$class == test_forest_predict)*100 #Calculando a taxa de acerto
  erro <- mean(dados_teste$class != test_forest_predict)*100 #Calculando a taxa de erro
  AuxMat[i,1] = acerto
  AuxMat[i,2] = erro
  AuxMat[i,3] = CT$t[1,1] #VP
  AuxMat[i,4] = CT$t[1,2] #FN
  AuxMat[i,5] = CT$t[2,1] #FP
  AuxMat[i,6] = CT$t[2,2] #VN
  AuxMat[i,7] = Precision(CT$t)
  AuxMat[i,8] = Recall(CT$t)
  AuxMat[i,9] = Accuracy(CT$t)
  AuxMat[i,10] = F1(CT$t)
  AuxMat[i,11] = AUC(CT$t)
}
for (j in 1:11) {
AuxMat[i+1,j] = mean(AuxMat[1:10,j])
AuxMat[i+2,j] = median(AuxMat[1:10,j])
AuxMat[i+3,j] = sd(AuxMat[1:10,j])
}
AuxMat
View(AuxMat)
################# MODELO RANDOM FOREST: Treinando e Testando Modelo #################FIM

################# MODELO REDE DE CRENÇA PROFUNDA: Treinando e Testando Modelo #################INICIO
#install.packages("darch")
library(darch)

glimpse(dataSet)
table(dataSet$class) #7000 3000

glimpse(dados_norm)
table(dados_norm$class) #7000 3000

AuxMat <- matrix(nrow = 13, ncol = 11)
dimnames(AuxMat) = (list( c("T01","T02","T03","T04","T05","T06","T07","T08","T09","T10","Média","Mediana","DesvioP"),
                          c("acertos","erros","VP","FN","FP","VN","Precisão","recall","acuracia","F1","AUC")))
dataset <- dados_norm #dataset <- dataSet
for (i in 1:10) {
  amostra <- ?sample.split(dataset, SplitRatio = 0.70) #Split dataSet
  dados_treino = subset(dataset, amostra == TRUE) #DataSet Treino
  dados_teste = subset(dataset, amostra == FALSE) #DataSet Testte
  
  dbn_model <- darch(class ~ ., data = dados_treino, layers = c(10, 5, 2),
                     darch.numEpochs = 10, darch.stopClassErr = 0, retainData = T) #Treino Modelo
  test_dbn_predict <- predict(dbn_model, newdata = dados_teste, type = "class"); #Testa Modelo
  CT <- CrossTable(x = test_dbn_predict, y = dados_teste$class, prop.chisq = FALSE) ## confusion matrix
  acerto <- mean(dados_teste$class == test_dbn_predict)*100 #Calculando a taxa de acerto
  erro <- mean(dados_teste$class != test_dbn_predict)*100 #Calculando a taxa de erro
  AuxMat[i,1] = acerto
  AuxMat[i,2] = erro
  AuxMat[i,3] = CT$t[1,1] #VP
  AuxMat[i,4] = CT$t[1,2] #FN
  AuxMat[i,5] = CT$t[2,1] #FP
  AuxMat[i,6] = CT$t[2,2] #VN
  AuxMat[i,7] = Precision(CT$t)
  AuxMat[i,8] = Recall(CT$t)
  AuxMat[i,9] = Accuracy(CT$t)
  AuxMat[i,10] = F1(CT$t)
  AuxMat[i,11] = AUC(CT$t)
}
for (j in 1:11) {
  AuxMat[i+1,j] = mean(AuxMat[1:10,j])
  AuxMat[i+2,j] = median(AuxMat[1:10,j])
  AuxMat[i+3,j] = sd(AuxMat[1:10,j])
}
AuxMat
View(AuxMat)
################# MODELO REDE DE CRENÇA PROFUNDA: Treinando e Testando Modelo #################FIM

table(dataSet$class)
table(dados_treino$class)
table(dados_teste$class)
prop.table(table(dataSet$class)) # 0.75 0.25
prop.table(table(dados_treino$class)) # 0.75 0.25
prop.table(table(dados_teste$class)) # 0.75 0.25

