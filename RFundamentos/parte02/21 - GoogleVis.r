# googleVis - Visualizacoes Interativas

# Baseado no Google Chart (visualizacao de dados na web)
# googlevis eh um pacote que fornece interface entre R e o Google Charts
# Utiliza JavaScript e arquivos JSON. Os dados sao transformados em formato JSON
# O resultado eh gerado em HTML5 ou Flash
# Pose-se criar os mais variados tipos de graficos e mapas. Inclusive Google Maps
# Voce pode incorporar os grÃ¡ficos criados em paginas HTML ou apps
# Verificar termos de uso, antes de utilizar

install.packages("googleVis")
library(googleVis)
?googleVis


# Criando um Dataframe
df = data.frame(PaÃ­s = c("BR", "CH", "AR"),
                ExportaÃ§Ãµes = c(10,13,14),
                ImportaÃ§Ãµes = c(23,12,32))


# Grafico de Linha
Line <- gvisLineChart(df)
plot(Line)


# Grafico de Barras
Column <- gvisColumnChart(df)
plot(Column)


# Grafico de Barras Horizontais
Bar <- gvisBarChart(df)
plot(Bar)


# GrÃ¡fico de Pizza
Pie <- gvisPieChart(CityPopularity)
plot(Pie)


# Graficos Combinados
Combo <- gvisComboChart(df, xvar = "PaÃ­s",
                        yvar = c("ExportaÃ§Ãµes", "ImportaÃ§Ãµes"),
                        options = list(seriesType = "bars",
                                     series='{1: {type:"line"}}'))
plot(Combo)


# Scatter Chart
Scatter <- gvisScatterChart(women,
                            options=list(
                              legend="none",
                              lineWidth=2, pointSize=0,
                              title="Women", vAxis="{title:'weight (lbs)'}",
                              hAxis="{title:'height (in)'}",
                              width=300, height=300))
plot(Scatter)


# Bubble
Bubble <- gvisBubbleChart(Fruits, idvar="Fruit",
                          xvar="Sales", yvar="Expenses",
                          colorvar="Year", sizevar="Profit",
                          options=list(
                            hAxis='{minValue:75, maxValue:125}'))
plot(Bubble)


# Customizando
M <- matrix(nrow=6,ncol=6)
M[col(M)==row(M)] <- 1:6
dat <- data.frame(X=1:6, M)
SC <- gvisScatterChart(dat,
                       options=list(
                         title="Customizing points",
                         legend="right",
                         pointSize=30,
                         series="{
                         0: { pointShape: 'circle' },
                         1: { pointShape: 'triangle' },
                         2: { pointShape: 'square' },
                         3: { pointShape: 'diamond' },
                         4: { pointShape: 'star' },
                         5: { pointShape: 'polygon' }
                         }"))
plot(SC)


# Gauge
Gauge <-  gvisGauge(CityPopularity,
                    options=list(min=0, max=800, greenFrom=500,
                                 greenTo=800, yellowFrom=300, yellowTo=500,
                                 redFrom=0, redTo=300, width=400, height=300))
plot(Gauge)


# Mapas
Intensity <- gvisIntensityMap(df)
plot(Intensity)


# Geo Chart
Geo=gvisGeoChart(Exports, locationvar="Country",
                 colorvar="Profit",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)


# Google Maps
AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip",
                     options=list(showTip=TRUE,
                                  showLine=TRUE,
                                  enableScrollWheel=TRUE,
                                  mapType='terrain',
                                  useMapTypeControl=TRUE))
plot(AndrewMap)


# Dados em Grafico. Nivel de analfabetismo nos EUA
require(datasets)
states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US",
                                       displayMode="regions",
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)



# Mais exemplos
demo(googleVis)
