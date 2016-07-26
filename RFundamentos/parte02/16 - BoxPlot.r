# Boxplots

?boxplot
?sleep

# Permite utilizar as colunas sem especificar o mome do dataset
attach(sleep)

# Construcao do boxplot
sleepboxplot = boxplot(data = sleep, extra ~ group,
                       main = "DuraÃ§Ã£o do Sono",
                       col.main = "red", ylab="Horas", xlab="Droga")

# Calculo da media
means = by(extra, group, mean)

# Adiciona a media ao grafico
points(means, col = "red")


#Boxplot horizontal
horizontalboxplot = boxplot(data = sleep, extra ~ group,
                            ylab="", xlab="", horizontal = T)

horizontalboxplot = boxplot(data = sleep, extra ~ group,
                            ylab="", xlab="", horizontal = T,
                            col = c("blue", "red") )
