# Text Mining

# Exemplo 1
install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer"))
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Carregando o dataset
df <- read.csv('questoes.csv', stringsAsFactors = FALSE)
head(df)

# Criando um Corpus
dfCorpus <- Corpus(VectorSource(df$Question))
class(dfCorpus)

# Convertendo Corpus em um documento de texto plano
dfCorpus <- tm_map(dfCorpus, PlainTextDocument)

# Remove pontuaÃ§Ã£o
dfCorpus <- tm_map(dfCorpus, removePunctuation)

# Remover palavras especÃ­ficas do inglÃªs
dfCorpus <- tm_map(dfCorpus, removeWords, stopwords('english'))

# Neste processo, vÃ¡rias versÃµes de uma palavras sÃ£o convertidas em uma Ãºnica instÃ¢ncia
dfCorpus <- tm_map(dfCorpus, stemDocument)

# Removendo palavras especÃ­ficas
dfCorpus <- tm_map(dfCorpus, removeWords, c('the', 'this', stopwords('english')))

# wordcloud
wordcloud(dfCorpus, max.words = 100, random.order = FALSE)



# Exemplo 2
install.packages("tm")
install.package("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Lendo o arquivo
arquivo <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
texto <- readLines(arquivo)

# Carregando os dados como Corpus
docs <- Corpus(VectorSource(texto))

# Pre-processamento
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Converte o texto para minÃºsculo
docs <- tm_map(docs, content_transformer(tolower))

# Remove numeros
docs <- tm_map(docs, removeNumbers)

# Remove as palavras mais comuns do idioma ingles
docs <- tm_map(docs, removeWords, stopwords("english"))

# Voce pode definir um vetor de palavras (stopwords) a serem removidas do texto
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))

# Remove pontuacao
docs <- tm_map(docs, removePunctuation)

# Elimina espaÃ§os extras
docs <- tm_map(docs, stripWhitespace)

# Text stemming
docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


# Tabela de frequencia
findFreqTerms(dtm, lowfreq = 4)
findAssocs(dtm, terms = "freedom", corlimit = 0.3)
head(d, 10)

# Graficos de barras com as palavras mais frequentes
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
