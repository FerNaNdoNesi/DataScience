# ggplot2

# Um sistema grafico completo, alternativo ao sistema basico
# de grÃ¡ficos do R
# Oferece mais opcoes de modificacao, legendas prontas,
# formatacao mais solida

install.packages("ggplot2")
library(ggplot2)

# Plotando um grafico basico com qplot()
data(tips, package = 'reshape2')
qplot(total_bill, tip, data = tips, geom = "point")


# Camada 1
camada1 <- geom_point(
  mapping = aes(x = total_bill, y = tip, color = sex),
  data = tips,
  size = 3
)
ggplot() + camada1

?aes
??aes

# Contruindo um modelo de regressao
modelo_base <- lm(tip ~ total_bill, data = tips)
modelo_fit <- data.frame(
  total_bill = tips$total_bill,
  predict(modelo_base, interval = "confidence")
)

head(modelo_fit)


# Camada 2
camada2 <- geom_line(
  mapping = aes(x = total_bill, y = fit),
  data = modelo_fit,
  color = "darkred"
)
ggplot() + camada1 + camada2


# Camada 3
camada3 <- geom_ribbon(
  mapping = aes(x = total_bill, ymin = lwr, ymax = upr),
  data = modelo_fit,
  alpha = 0.3
)
ggplot() + camada1 + camada2 + camada3


# VersÃ£o final otimizada
ggplot(tips, aes(x = total_bill, y = tip)) +
  geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm')


# Gravando o grafico em um objeto
myplot <- ggplot(tips, aes(x = total_bill, y = tip)) +
  geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm')

class(myplot)
print(myplot)


# ScatterPlot com linha de regressao

# Dados
data = data.frame(cond = rep(c("Obs 1", "Obs 2"),
                           each = 10), var1 = 1:100 +
                           rnorm(100,sd = 9), var2 = 1:100 +
                           rnorm(100,sd = 16))


# Plot
ggplot(data, aes(x = var1, y = var2)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm , color = "red", se = FALSE)

?lm

# Bar Plot

# Dados
data = data.frame(grupo = c("A ","B ","C ","D ") ,
                  valor = c(33,62,56,67) ,
                  num_obs = c(100,500,459,342))

# Gerando a massa de dados
data$right=cumsum(data$num_obs) + 30 * c(0:(nrow(data)-1))
data$left=data$right - data$num_obs

# Plot
ggplot(data, aes(ymin = 0)) +
    geom_rect(aes(xmin = left, xmax = right,
                  ymax = valor, colour = grupo, fill = grupo)) +
                  xlab("NÃºmero de obs") + ylab("Valor")



# Usando mtcars

head(mtcars)
ggplot(data = mtcars, aes(x = disp, y = mpg)) + geom_point()


# Outro aspecto que pode ser mapeado nesse grafico eh a cor dos pontos
ggplot(data = mtcars,
       aes(x = disp, y = mpg,
           colour = as.factor(am))) + geom_point()


# No entanto, tambem podemos mapear uma variÃ¡vel continua a cor dos pontos:
ggplot(mtcars, aes(x = disp, y = mpg, colour = cyl)) + geom_point()


# Tambem podemos mapear o tamanho dos pontos a uma variÃ¡vel de interesse:
# A legenda Ã© inserida no grÃ¡fico automaticamente
ggplot(mtcars, aes(x = disp, y = mpg, colour = cyl, size = wt)) + geom_point()


# Os geoms definem qual forma geomÃ©trica sera utilizada para a visualizacao dos dados no grafico.
ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) + geom_boxplot()


# Histogramas
ggplot(mtcars, aes(x = mpg), binwidth = 30) + geom_histogram()


# Grafico de Barras
ggplot(mtcars, aes(x = as.factor(cyl))) + geom_bar()


# Personalizando os graficos
colors()

ggplot(mtcars, aes(x = as.factor(cyl), y = mpg,
                   colour = as.factor(cyl))) + geom_boxplot()

ggplot(mtcars, aes(x = as.factor(cyl), y = mpg,
                   fill = as.factor(cyl))) + geom_boxplot()

ggplot(mtcars,
       aes(x = as.factor(cyl), y = mpg)) +
  geom_boxplot(color = "blue", fill = "seagreen4")


# Podemos alterar os eixos
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram() +
  xlab("Milhas por galao") + ylab("Frequencia")


# Alterar os limites do grafico
ggplot(mtcars, aes(x = mpg)) + geom_histogram() + xlab("Milhas por galao") + ylab("FrequÃªncia") +
  xlim(c(0, 40)) +
  ylim(c(0,8))


# Legendas
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) +
  geom_bar() +
  labs(fill = "cyl")

# Trocando a posicao da legenda
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) +
  geom_bar() +
  labs(fill = "cyl") +
  theme(legend.position="top")


# Sem legenda
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) +
  geom_bar() +
  guides(fill=FALSE)


# Facets
ggplot(mtcars, aes(x = mpg, y = disp, colour = as.factor(cyl))) +
  geom_point() +
  facet_grid(am~.)

ggplot(mtcars, aes(x = mpg, y = disp, colour = as.factor(cyl))) +
  geom_point() +
  facet_grid(.~am)


# Plots diferentes juntos (diferente de Facet)
library(gridExtra)
library(ggplot2)

# Dataset diamonds
data(diamonds)

# Histograma como plot1
plot1 <- qplot(price, data = diamonds, binwidth = 1000)

# ScatterPlot como plot2
plot2 <- qplot(carat, price, data = diamonds, colour = cut)

# Combina os 2 plots na mesma Ã¡rea
grid.arrange(plot1, plot2, ncol = 1)


# Graficos de Densidade

ggplot(data = diamonds, aes(x = price, group = cut, fill = cut)) +
    geom_density(adjust = 1.5)

ggplot(data = diamonds, aes(x = price, group = cut, fill = cut)) +
    geom_density(adjust = 1.5 , alpha = 0.2)

ggplot(data = diamonds,aes(x = price, group = cut, fill = cut)) +
    geom_density(adjust = 1.5, position = "fill")


# Facets com reshape
library(reshape2)
library(plotly)

sp <- ggplot(tips, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)
sp + facet_grid(sex ~ .)
ggplotly()
sp + facet_grid(. ~ sex)
ggplotly()
sp + facet_wrap( ~ day, ncol = 2)
ggplotly()


ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~manufacturer)
ggplotly()
