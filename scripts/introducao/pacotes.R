# trabalhando com pacotes

# instala o pacote caret (machine learning)
install.packages("caret")

# carrega o pacote caret
library(caret)

# carga temporária
caret::featurePlot(x = iris[, 1:4], y = iris$Species) # plot das variáveis do conjunto de dados iris, usando o pacote caret
