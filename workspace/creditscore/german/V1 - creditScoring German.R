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

read_lines("german-credit-data.csv")
arq_german_credit_data <- read_csv("german-credit-data.csv")
original_german_credit_data <- read_csv("german-credit-data.csv")

View(arq_german_credit_data)
glimpse(arq_german_credit_data) #Visualização rápida

######################### ETAPA 01: Limpeza dos dados #########################
any(is.na(arq_german_credit_data)) # se existir valores nulos é necessário trata-los


####################### ETAPA 02: Análise exploratória ########################
#1 - Atributo classe deve ser do tipo Fator
str(arq_german_credit_data$class) #retorna tipo variavel
table(arq_german_credit_data$class) #retorna percentual de distribuição
arq_german_credit_data$class <- factor(arq_german_credit_data$class, levels = c(1,2), labels = c("Good", "Bad"))
original_german_credit_data$class <- factor(original_german_credit_data$class, levels = c(1,2), labels = c("Good", "Bad"))
table(arq_german_credit_data$class)
str(arq_german_credit_data$class) #agora é um fator

#2 - Discretização dos atributos ## Explorando variaveis numericas
#Verificar medidas de tendência central
table(arq_german_credit_data$MontEmp) #retorna percentual de distribuição
prop.table(table(arq_german_credit_data$MontEmp)) #verifica proporção
count(arq_german_credit_data, MontEmp) #conta quantas vezes cada valor aparece
hist(arq_german_credit_data$MontEmp) #visualizar em grafico histograma

table(arq_german_credit_data$MesesCc) #retorna percentual de distribuição
prop.table(table(arq_german_credit_data$MesesCc)) #verifica proporção
count(arq_german_credit_data, MesesCc) #conta quantas vezes cada valor aparece
hist(arq_german_credit_data$MesesCc) #visualizar em grafico histograma
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
var(arq_german_credit_data$MesesCc) #Correlação, variância
sd(arq_german_credit_data$MesesCc) #desvio padrão

var(arq_german_credit_data$MontEmp) #Correlação, variância
sd(arq_german_credit_data$MontEmp) #desvio padrão

quantile(arq_german_credit_data$MesesCc) #quartis normais
quantile(arq_german_credit_data$MesesCc, seq( from = 0, to = 1, by = 0.20)) #quartis modificados
boxplot(arq_german_credit_data$MesesCc, main = "Boxplot para meses com conta corrente", ylab = "Meses")
hist(arq_german_credit_data$MesesCc, main = "Histograma para meses com conta corrente", xlab = "Meses")
boxplot(arq_german_credit_data$MontEmp, main = "Boxplot para montante de emprestimo", ylab = "(R$)")
hist(arq_german_credit_data$MontEmp, main = "Histograma para montante de emprestimo", xlab = "(R$)")

## Explorando variaveis categoricas
# A FAZER função discretizar em R utilizando quantile

#3 - Normalização dos atributos ## Explorando variaveis numericas
#Detectar problemas de escala entre os dados distâncias muito distintas precisam ser normalizadas
normalizar <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
dados_norm <- as.data.frame(lapply(arq_german_credit_data[c("MesesCc", "MontEmp", "Idade")], normalizar))
head(dados_norm)
# SOMENTE EXEMPLO (nesta base de dados não temos este problema pois os 3 anteriores foram discretizados) sobrando apenas o seguintes:
summary(arq_german_credit_data[c("PercentRend", "ResidAtual", "NumEmpAtivo", "NumPessoas")])


################# ETAPA 03: Separando conjunto Treino e Teste #################
#Exemplo 1:
dados_treino <- arq_german_credit_data[1:700, ]
glimpse(dados_treino)
dados_teste <- arq_german_credit_data[701:1000, ]
glimpse(dados_teste)
#Exemplo 2:
# Dividindo base de dados (Treino e Teste) de forma randomica
install.packages("caTools")
library(caTools)
set.seed(101) 
amostra <- sample.split(arq_german_credit_data, SplitRatio = 0.80)
# ***** Treinamos nosso modelo nos dados de treino *****
# *****   Fazemos as predicoes nos dados de teste  *****
dados_treino = subset(arq_german_credit_data, amostra == TRUE)
glimpse(dados_treino)
dados_teste = subset(arq_german_credit_data, amostra == FALSE)
glimpse(dados_teste)

################# ETAPA 04: Criando o modelo #################
# Usando a funcao rpart para criar a arvore de decisao
install.packages("rpart")
library(rpart)
modelo_arvore <- rpart(class ~ . , method = 'class', data = dados_treino)
class(modelo_arvore)
modelo_arvore

printcp(modelo_arvore) # Para examinar o resultado de uma arvore de decisao, existem diversas funcoes
#plot(modelo_arvore, uniform = TRUE, main = "Arvore de Decisao em R")
#text(modelo_arvore, use.n = TRUE, all = TRUE)
install.packages('rpart.plot') # Este outro pacote faz a visualizaco ficar mais legivel
library(rpart.plot)
prp(modelo_arvore)

################# ETAPA 05: Avaliando o modelo #################
## Predict como funcao para trazer a probabilidade do cliente ser mau/bom
test_tree_predict = predict(modelo_arvore, newdata = dados_teste);
head(test_tree_predict)

## Predict com tipo 'classe' retorna se é bom ou mau
test_tree_predict = predict(modelo_arvore, newdata = dados_teste, type = "class");
head(test_tree_predict)

install.packages("gmodels")
library(gmodels)
CrossTable(x = test_tree_predict, y = dados_teste$class, prop.chisq = FALSE) ## confusion matrix
#Linha representa a classificação correta
#Coluna representa a classificação do modelo
#172 disse que era Good e realmente eram Good (acertou)
#49 disse que era Good mas eram Bad (errou)
#35 disse que era Bad mas eram Good (errou)
#44 disse que era Bad e realmente eram Bad (acertou)

#Calculando a taxa de erro
taxa_erro = mean(dados_teste$class != test_tree_predict)*100
taxa_acerto = mean(dados_teste$class == test_tree_predict)*100
taxa_acerto
taxa_erro

prop.table(table(test_tree_predict))

#### Avaliando Modelo DataScience #####
result_previsto <- data.frame( actual = dados_teste$class,
                               previsto = predict(modelo_arvore, newdata = dados_teste, type = "class")
)
head(result_previsto)
#função manual para criar a matriz de confusão
confusionMatrix <- matrix(unlist(Map(function(x, y){sum(ifelse(result_previsto[, 1] == x & result_previsto[, 2] == y, 1, 0) )},
                                     c("Good", "Bad", "Good", "Bad"), c("Good", "Good", "Bad", "Bad"))), nrow = 2)
confusionMatrix

## Criando um dataframe com as estatisticas dos testes
df_mat <- data.frame( Category = c("Credito Ruim", "Credito Bom"),
                      Classificado_como_ruim = c(confusionMatrix[1,1], confusionMatrix[2,1]),
                      Classificado_como_bom = c(confusionMatrix[1,2], confusionMatrix[2,2]),
                      Accuracy_Recall = c(Accuracy(confusionMatrix), Recall(confusionMatrix)),
                      Precision_WAcc = c(Precision(confusionMatrix), W_Accuracy(confusionMatrix)))

print(df_mat)

# Gerando uma curva ROC em R
install.packages("ROCR")
install.packages("gplots")
library("ROCR")
library("gplots")

# Gerando as classes de dados
class1 <- predict(modelo_arvore, newdata = dados_teste, type = 'prob')
class2 <- dados_teste$class

# Gerando a curva ROC
?prediction
?performance
pred <- prediction(class1[,2], class2)
perf <- performance(pred, "fpr", "tpr") 
plot(perf, col = rainbow(10))

# Gerando Confusion Matrix com o Caret
library(caret)
?confusionMatrix
confusionMatrix(result_previsto$actual, result_previsto$previsto)

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

################# RESUMINDO DECISION TREE: Treinando e Testando Modelo #################
arq_german_credit_data <- read_csv("german-credit-data.csv") #Carrega dados
arq_german_credit_data$class <- factor(arq_german_credit_data$class, levels = c(1,2), labels = c("Good", "Bad")) #Classe como fator
set.seed(101) 
amostra <- sample.split(arq_german_credit_data, SplitRatio = 0.70) #Split dataSet
dados_treino = subset(arq_german_credit_data, amostra == TRUE) #DataSet Treino
dados_teste = subset(arq_german_credit_data, amostra == FALSE) #DataSet Testte
modelo_arvore <- rpart(class ~ . , method = 'class', data = dados_treino) #Treino Modelo
test_tree_predict = predict(modelo_arvore, newdata = dados_teste, type = "class"); #Testa Modelo
CT <- CrossTable(x = test_tree_predict, y = dados_teste$class, prop.chisq = FALSE) ## confusion matrix
mean(dados_teste$class != test_tree_predict)*100 #Calculando a taxa de erro
mean(dados_teste$class == test_tree_predict)*100 #Calculando a taxa de acerto
prp(modelo_arvore)

## classes para gerar curva ROC
class1 <- predict(modelo_arvore, newdata = dados_teste, type = 'prob')
class2 <- dados_teste$class
pred <- prediction(class1[,2], class2)
perf <- performance(pred, "fpr", "tpr") 
plot(perf, col = rainbow(10))

## Matriz de confusão manual
result_previsto <- data.frame( actual = dados_teste$class,
                               previsto = predict(modelo_arvore, newdata = dados_teste, type = "class")
)
confusionMatrix <- matrix(unlist(Map(function(x, y){sum(ifelse(result_previsto[, 1] == x & result_previsto[, 2] == y, 1, 0) )},
                                     c("Good", "Bad", "Good", "Bad"), c("Good", "Good", "Bad", "Bad"))), nrow = 2)
## Criando um dataframe com as estatisticas dos testes
df_mat <- data.frame( Category = c("Credito Ruim", "Credito Bom"),
                      Classificado_como_ruim = c(confusionMatrix[1,1], confusionMatrix[2,1]),
                      Classificado_como_bom = c(confusionMatrix[1,2], confusionMatrix[2,2]),
                      Accuracy_Recall = c(Accuracy(confusionMatrix), Recall(confusionMatrix)),
                      Precision_WAcc = c(Precision(confusionMatrix), W_Accuracy(confusionMatrix)))

print(df_mat)

## Otimizando o modelo && utilizando Floresta
# Criando uma Cost Function
install.packages("randomForest")
install.packages("C50")
library(randomForest)
library(C50)
Cost_func <- matrix(c(0.0, 1.5, 1.0, 0.0), nrow = 2, dimnames = list(c("Good", "Bad"), c("Good", "Bad")))
modelo_floresta <- C5.0( class ~ StatusCc
                         + MesesCc
                         + HistCredit
                         + PropEmp
                         + MontEmp
                         + Invest
                         + TrabAtual
                         + PercentRend
                         + EstCivil
                         + OutrosDev
                         + ResidAtual
                         + TipoResid
                         + Idade
                         + OutrosParc
                         + TipoMoradia
                         + NumEmpAtivo
                         + TipoTrab
                         + NumPessoas
                         + Telefone
                         + TrabEstr, 
                         data = dados_treino, 
                         ntree = 100, nodesize = 10, cost = Cost_func)
print(modelo_floresta)

## classes para gerar curva ROC
class1F <- predict(modelo_floresta, newdata = dados_teste, type = 'prob')
class2F <- dados_teste$class
predF <- prediction(class1F[,2], class2F)
perfF <- performance(predF, "fpr", "tpr") 
plot(perfF, col = rainbow(10))

## Matriz de confusão manual
result_previstoF <- data.frame( actual = dados_teste$class,
                                previsto = predict(modelo_floresta, newdata = dados_teste, type = "class")
)
confusionMatrixF <- matrix(unlist(Map(function(x, y){sum(ifelse(result_previstoF[, 1] == x & result_previstoF[, 2] == y, 1, 0) )},
                                     c("Good", "Bad", "Good", "Bad"), c("Good", "Good", "Bad", "Bad"))), nrow = 2)
## Criando um dataframe com as estatisticas dos testes
df_matF <- data.frame( Category = c("Credito Ruim", "Credito Bom"),
                      Classificado_como_ruimF = c(confusionMatrixF[1,1], confusionMatrixF[2,1]),
                      Classificado_como_bomF = c(confusionMatrixF[1,2], confusionMatrixF[2,2]),
                      Accuracy_RecallF = c(Accuracy(confusionMatrixF), Recall(confusionMatrixF)),
                      Precision_WAccF = c(Precision(confusionMatrixF), W_Accuracy(confusionMatrixF)))

print(df_matF)

################
plot(perf, col = rainbow(10))
plot(perfF, col = rainbow(10))
print(df_mat)
print(df_matF)
library(caret)
confusionMatrix(result_previsto$actual, result_previsto$previsto)
confusionMatrix(result_previstoF$actual, result_previstoF$previsto)
################# RESUMINDO DEEP LEARNING #################
install.packages("deepnet") # deepnet
library(deepnet)


#Example 01 DNN
Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
x <- matrix(c(Var1, Var2), nrow = 100, ncol = 2)
y <- c(rep(1, 50), rep(0, 50))
dnn <- dbn.dnn.train(x, y, hidden = c(5, 5))

test_Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
test_Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
test_x <- matrix(c(test_Var1, test_Var2), nrow = 100, ncol = 2)
nn.test(dnn, test_x, y)

#Teste 01 DNN
as.matrix(dados_treino[c("MesesCc", "MontEmp", "Idade")])
as.matrix(dados_teste[c("MesesCc", "MontEmp", "Idade")])
dnn <- dbn.dnn.train(as.matrix(dados_treino[c("MesesCc", "MontEmp", "Idade")]), dados_treino$class, hidden = c(5, 5))
dnn

test_x <- matrix(as.matrix(dados_teste[c("MesesCc", "MontEmp", "Idade")]), nrow = 238, ncol = 3)
nn.test(dnn, test_x, dados_teste$class)

#Example 02 RBM (Restricted Boltzmann Machine)
Var1 <- c(rep(1, 50), rep(0, 50))
Var2 <- c(rep(0, 50), rep(1, 50))
x3 <- matrix(c(Var1, Var2), nrow = 100, ncol = 2)
x3
r1 <- rbm.train(x3, 3, numepochs = 20, cd = 10)
r1
v <- c(0.2, 0.8)
v
h <- rbm.up(r1, v)
h

 #Teste 02 RBM (Restricted Boltzmann Machine)
Var1 <- c(rep(1, 50), rep(0, 50))
Var2 <- c(rep(0, 50), rep(1, 50))
x3 <- matrix(as.matrix(dados_teste[c("MesesCc", "MontEmp", "Idade")]), nrow = 238, ncol = 3)
r1 <- rbm.train(x3, 3, numepochs = 20, cd = 10)
r1
v <- c(0.2, 0.8)
h <- rbm.up(r1, v)
