sessionInfo() # Visualizar alguns pacotes carregados


print("Hello R!")
remove(va3) # remove variavel

# Usando o pacote readr
install.packages("readr")
#tratamento de dados em memória
install.packages("dplyr") 
library(readr)
library(dplyr)
# Objetivo de remodelar os dados
install.packages("tidyr")
library(tidyr)
#criar novas colunas remodeladas
install.packages("gmodels")
library(gmodels)
#Pacote que contem o classificador kNN
install.packages("class")
library(class)
# Existem diversos pacotes para arvores de recisao em R. Usaremos aqui o rpart.
install.packages("rpart")
install.packages("tree")
install.packages("randomForest")
library(tree)
library(rpart)
library(randomForest)
install.packages("gplots")
library(gplots)
# Este outro pacote faz a visualizaco da arvore ficar mais legivel
install.packages('rpart.plot')
library(rpart.plot)

# mostrar problemas
problems(lista_exemplo)
# Parsing
parse_date("01/02/15", "%m/%d/%y")
locale("pt")

# Ler linhas arquivo
read_lines("cs-training-kaggle.csv")
read_lines("german-credit-data.csv")

# 11 atributos # 150.000 linhas
arq_training_kaggle <- read_csv("cs-training-kaggle.csv") #importa em um dataframe
# 12 atributos # 101.503 linhas
arq_test_kaggle <- read_csv("cs-test-kaggle.csv") #importa em um dataframe
# 21 atributos # 1.000 linhas
arq_german_credit_data <- read_csv("german-credit-data.csv")

# Exportando BKP dos dataframes
write_csv(arq_training_kaggle, "exportanto_teste.csv")

head(arq_training_kaggle)
class(arq_training_kaggle) #informação do tipo da variavel
str(arq_training_kaggle) #resumo sobre as variaveis
View(arq_training_kaggle)

# Olhar dados e testar (sem modificação real)
glimpse(arq_training_kaggle) #dar uma olhada rápida
glimpse(mutate(arq_training_kaggle, new_column = age / 2)) #modificar e testar

#contagem de valores distintos
count(arq_training_kaggle, age) #conta quantas vezes cada valor aparece
hist(arq_training_kaggle$age) #visualizar em grafico histograma

#Tras uma amostra do conjunto de dados
amostra1 <- sample_n(arq_training_kaggle, size = 30)
amostra2 <- sample_n(arq_training_kaggle, size = 30)

# select() de um subset de dados
subSetTraining1 <- select(arq_training_kaggle, age, NumberOfDependents) #seleciona duas colunas
head(subSetTraining1)
class(subSetTraining1)
glimpse(subSetTraining1)
subSetTraining1 <- select(arq_training_kaggle, age:DebtRatio) #seleciona entre as colunas

# filter() para filtar um subset de dados
filter(arq_training_kaggle, age >= 26)
subSetTraining2 <- filter(arq_training_kaggle, age >= 26, NumberOfDependents >= 2)
filter(arq_training_kaggle, cidade %in% c("Recife", "Curitiba")) #exemplo
head(subSetTraining2)

# arrange() para organizer um subset de dados
subSetTraining3 <- arq_training_kaggle %>%
  select(age, MonthlyIncome, NumberOfDependents) %>%
  arrange(desc(MonthlyIncome))
head(subSetTraining3)
View(subSetTraining3)

# mutate() para incluir nova coluna no subset de dados
subSetTraining4 <- arq_training_kaggle %>%
  select(age, MonthlyIncome, NumberOfDependents) %>%
  mutate(rendaPorFilho = MonthlyIncome/NumberOfDependents)
head(subSetTraining4)
View(subSetTraining4)

#summarise() resumindo
arq_training_kaggle %>%
  summarise(mediaIdade = mean(age),
            minIdade = min(age),
            maxIdade = max(age),
            total = n())

# group_by() funções po grupo
arq_training_kaggle %>%
  group_by(NumberOfDependents) %>%
  summarise(avgIdade = mean(age),
            minIdade = min(age),
            maxIdade = max(age),
            total = n())


# Criando um objeto tbl (dataframe mais organizado)
tbl_training_kaggle <- tbl_df(arq_training_kaggle)
arq_training_kaggle
tbl_training_kaggle


# Análise exploratória
head(arq_training_kaggle)
str(arq_training_kaggle)
glimpse(arq_training_kaggle)

#Medidas e tendencias centais
summary(arq_training_kaggle$age)
summary(arq_training_kaggle[c('age','MonthlyIncome')])

## Explorando variaveis numericas

# Usando as funcoes
mean(arq_training_kaggle$age)
median(arq_training_kaggle$age)
quantile(arq_training_kaggle$age)
quantile(arq_training_kaggle$age, probs = c(0.01, 0.99))
quantile(arq_training_kaggle$age, seq( from = 0, to = 1, by = 0.20))
IQR(arq_training_kaggle$age) #Diferença entre Q3 e Q1
range(arq_training_kaggle$age) #min e max
diff(range(arq_training_kaggle$age)) #diferença

# Boxplot
# Leitura de Baixo para Cima - Q1, Q2 (Mediana) e Q3
boxplot(arq_training_kaggle$age, main = "Boxplot para a Km de Carros Usados", ylab = "Kilometragem (R$)")
hist(arq_training_kaggle$age, main = "Histograma para a Km de Carros Usados", breaks = 5, ylab = "Kilometragem (R$)")

# Medidas de Dispersao
# Ao interpretar a variancia, numeros maiores indicam que 
# os dados estao espalhados mais amplamente em torno da 
# media. O desvio padrao indica, em media, a quantidade 
# de cada valor diferente da media.
var(arq_training_kaggle$age)
sd(arq_training_kaggle$age)

#Analise exploratorio - Parte 03
#table(arq_training_kaggle$MonthlyIncome)
#MonthlyIncome/NumberOfDependents

#CrossTable(x = arq_training_kaggle$age, y = arq_training_kaggle$MonthlyIncome, chisq = TRUE)
#chisq.test(x = arq_training_kaggle$age, y = arq_training_kaggle$MonthlyIncome)

#24 - Classificação usando nearest neighbor - Parte 02

#Fazer a limpeza dos dados
##Não DIZ?

#Fazer analise exploratória
glimpse(arq_training_kaggle)
#1-excluir variaveis do tipo ID
dados <- dados[-1]
#2-verificar se existe informação nula
any(is.na(arq_training_kaggle)) # se existir é necessário trata-los
#3-verificar se é fator? (variavel que iremos prever) Se não for converte
table(arq_training_kaggle$SeriousDlqin2yrs)
str(arq_training_kaggle$SeriousDlqin2yrs) #retorna tipo variavel
arq_training_kaggle$SeriousDlqin2yrs <- factor(arq_training_kaggle$SeriousDlqin2yrs, levels = c(0,1), labels = c(TRUE, FALSE))
table(arq_training_kaggle$SeriousDlqin2yrs)
str(arq_training_kaggle$SeriousDlqin2yrs) #agora é um fator

#continuar aula 24 - 6 minutos

# TREINAMENTO German DataSet
# Carrega dados 21 atributos # 1.000 linhas
arq_german_credit_data <- read_csv("german-credit-data.csv")
#Fazer limpeza

#Fazer analise exploratória
glimpse(arq_german_credit_data)
any(is.na(arq_german_credit_data)) # se existir é necessário trata-los
#Alguns classificadores requerem que as variáveis (atributo class) sejam do tipo Fator
#3-verificar se é fator? (variavel que iremos prever) Se não for converte
table(arq_german_credit_data$class) #retorna percentual de distribuição
str(arq_german_credit_data$class) #retorna tipo variavel
arq_german_credit_data$class <- factor(arq_german_credit_data$class, levels = c(1,2), labels = c("Good", "Bad"))
table(arq_german_credit_data$class)
str(arq_german_credit_data$class) #agora é um fator

#Verificando proporção
prop.table(table(arq_german_credit_data$class))

#Verificar medidas de tendência central
#Detectar problemas de escala entre os dados distâncias muito distintas precisam ser normalizadas
#Ex: O cálculo distância do kNN é dependente das medidas de escala nos dados
summary(arq_german_credit_data[c("MesesCc", "MontEmp", "PercentRend", "ResidAtual", "Idade", "NumEmpAtivo", "NumPessoas")])
#Função de normalização
normalizar <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
#testando função
normalizar(c(1,2,3,4,5))
normalizar(c(10,20,30,40,50))

#Aplicando a função ao conjunto de dados
dados_norm <- as.data.frame(lapply(arq_german_credit_data[c("MesesCc", "MontEmp", "PercentRend", "ResidAtual", "Idade", "NumEmpAtivo", "NumPessoas")], normalizar))  #lapply aplica a função a cada coluna
dados_origin <- as.data.frame(arq_german_credit_data[c("StatusCc", "HistCredit", "PropEmp", "Invest", "TrabAtual", "EstCivil", "OutrosDev", "TipoResid", "OutrosParc", "TipoMoradia", "TipoTrab", "Telefone", "TrabEstr", "class")])
dados_prontos <- join_all(dados_norm,dados_origin); 
glimpse(dados_prontos)
glimpse(arq_german_credit_data)

dados_treino <- arq_german_credit_data[1:800, ] #Novo dataSet 700x21
dados_treino
dados_teste <- arq_german_credit_data[801:1000, ] #Novo dataSet 300x21
dados_teste
dados_treino <- sample_n(arq_german_credit_data, size = 700)
dados_teste <- sample_n(arq_german_credit_data, size = 300)
#dados_teste
dados_treino_class <- arq_german_credit_data[1:800,21]
dados_treino_class
dados_teste_class <- arq_german_credit_data[801:1000,21]
dados_teste_class
dados_treino
dados_treino_class

# Dividindo base de dados (Treino e Teste) de forma randomica
install.packages("caTools")
library(caTools)
?sample.split
set.seed(101) 
amostra <- sample.split(arq_german_credit_data, SplitRatio = 0.80)
amostra
# ***** Treinamos nosso modelo nos dados de treino *****
# *****   Fazemos as predicoes nos dados de teste  *****

# Criando dados de treino - 70% dos dados
dados_treino = subset(arq_german_credit_data, amostra == TRUE)

# Criando dados de teste - 30% dos dados
dados_teste = subset(arq_german_credit_data, amostra == FALSE)

modelo <- rpart(train = dados_treino,
              test  = dados_teste,
              cl = dados_treino_class)

class(modelo)


# Vamos utilizar um dataset que eh disponibilizado junto com o pacote rpart
str(kyphosis)
head(kyphosis)

# Ussamos a funcao rpart para criar a arvore de decisao
#arvore <- rpart(Kyphosis ~ . , method = 'class', data = kyphosis)
arvore <- rpart(class ~ . , method = 'class', data = dados_treino)
#arvore2 <- tree(class ~ . , method = 'class', data = dados_treino)
library(randomForest)
set.seed(17)
floresta <- randomForest(class ~ ., dados_treino, ntree = 10)
??randomForest
class(arvore)
arvore

# Para examinar o resultado de uma arvore de decisao, existem diversas funcoes
printcp(arvore)

# Visualizando a arvore (execute uma funcao para o plot e outra para o texto no plot)
# Utilize o zoom para visualizar melhor o grafico

plot(arvore, uniform = TRUE, main = "Arvore de Decisao em R")
text(arvore, use.n = TRUE, all = TRUE)

# Este outro pacote faz a visualizaco ficar mais legivel
install.packages('rpart.plot')
library(rpart.plot)
prp(arvore)

## Predict como funcao para trazer a probabilidade do cliente ser mau/bom
test_tree_predict = predict(arvore, newdata = dados_teste);
head(test_tree_predict);

## Predict com tipo 'classe' retorna se é bom ou mau
test_tree_predict = predict(arvore, newdata = dados_teste, type = "class");
head(test_tree_predict);
test_tree_predict
dados_teste_class
## confusion matrix
table(test_tree_predict, dados_teste$class)


# Etapa 4: AVALIANDO O MODELO
install.packages("gmodels")
library(gmodels)
CrossTable(x = test_tree_predict, y = dados_teste$class, prop.chisq = FALSE)
#Linha representa a classificação correta
#Coluna representa a classificação do modelo
#172 disse que era Good e realmente eram Good (acertou)
#49 disse que era Good mas eram Bad (errou)
#35 disse que era Bad mas eram Good (errou)
#44 disse que era Bad e realmente eram Bad (acertou)

#Calculando a taxa de erro
taxa_erro = NULL
taxa_acerto = NULL

taxa_erro = mean(dados_teste$class != test_tree_predict)*100
taxa_acerto = mean(dados_teste$class == test_tree_predict)*100
taxa_acerto
taxa_erro

#Etapa 5: Otimizando a performace do modelo
