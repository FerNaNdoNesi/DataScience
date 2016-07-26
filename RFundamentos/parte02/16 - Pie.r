# Graficos de Pizza

?pie

# Criando as fatias
fatias = c(40, 20, 40)

# Nomeando os labels
paises = c("Brasil", "Argentina", "Chile")

# Unindo paises e fatias
paises = paste(paises, fatias)

# Especificar os labÃ©us
paises = paste(paises, "%", sep = "")

colors()

# Construindo um grafico
pie(fatias, labels = paises,
    col=c("darksalmon", "gainsboro", "lemonchiffon4"),
    main ="Dsitribuicao de Vendas")


# Trabalhando com dataframes
?iris
attach(iris)
Values = table(Species)
labels = paste(names(Values))
pie(Values, labels = labels, main = "DistribuiÃ§Ã£o de Especies")


# 3D
library(plotrix)

pie3D(fatias, labels = paises, explode = 0.05,
      col = c("steelblue1", "tomato2", "tan3"),
      main = "DsitribuiÃ§Ã£o de Vendas")
