#Pacotes

pacman::p_load(car, caret, corrplot, data.table, dplyr, forcats, funModeling, mltools)

library(magrittr)
library(knitr)
library(dplyr)
library(shiny)
library(plotly)
library(devtools)
library(readxl)

## ajuste em regressão


ENADE2019_PRODUCAO <- read.csv2('https://raw.githubusercontent.com/hugoavmedeiros/cd_com_r/master/bases_tratadas/ENADE2019_PRODUCAO.csv', stringsAsFactors = T) # carregando a base já tratada para o ambiente do R

ENADE2019_PRODUCAO$codCasos <- seq(1:nrow(ENADE2019_PRODUCAO))

# Análise exploratória de casos
ENADE2019_PRODUCAO <- ENADE2019_PRODUCAO %>% filter(CO_ORGACAD == 'Universidade')

summary(ENADE2019_PRODUCAO)

status(ENADE2019_PRODUCAO)

p1 <-plot_ly(y = ENADE2019_PRODUCAO$CO_ORGACAD, type = "box", text = ENADE2019_PRODUCAO$codCasos, boxpoints = "all", jitter = 0.3)

p2 <-plot_ly(y = ENADE2019_PRODUCAO$NU_IDADE, type = "box", text = ENADE2019_PRODUCAO$codCasos, boxpoints = "all", jitter = 0.3)

p3 <-plot_ly(y = ENADE2019_PRODUCAO$media, type = "box", text = ENADE2019_PRODUCAO$codCasos, boxpoints = "all", jitter = 0.3)

p4 <-plot_ly(y = ENADE2019_PRODUCAO$QE_I08, type = "box", text = ENADE2019_PRODUCAO$codCasos, boxpoints = "all", jitter = 0.3)

p5 <-plot_ly(y = ENADE2019_PRODUCAO$QE_I15, type = "box", text = ENADE2019_PRODUCAO$codCasos, boxpoints = "all", jitter = 0.3)

p6 <-plot_ly(y = ENADE2019_PRODUCAO$NT_GER, type = "box", text = ENADE2019_PRODUCAO$codCasos, boxpoints = "all", jitter = 0.3)

subplot(p1, p2, p3, p4, p5, p6)

# anular o valor discrepante
ENADE2019_PRODUCAO[140 , 6:13 ] <- NA

ENADE2019_PRODUCAO

# corrigir o valor anulado

ENADE2019_PRODUCAO[ ,6:13] <- impute(ENADE2019_PRODUCAO[ ,6:13], 'random')

# Pré-processamento
particaoENEM = createDataPartition(1:nrow(ENADE2019_PRODUCAO), p=.7) # cria a partição 70-30
treinoENEM = ENADE2019_PRODUCAO[particaoENEM$Resample1, ] # treino
testeENEM = ENADE2019_PRODUCAO[-particaoENEM$Resample1, ] # - treino = teste

ENADE_LM_v2 <- lm(NT_GER ~ QE_I02 +  QE_I04 + QE_I05 + QE_I08 + QE_I09, data = treinoENEM)

summary(ENADE_LM_v2)

#### OUTLIERS
outlierTest(ENADE_LM_v2) # identificar outliers na regressão

# identificar pontos de alavancagem
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Pontos de Alavancagem")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(ENADE_LM_v2)

# identificar observações influentes
influencePlot(ENADE_LM_v2, id.method="identify", main="Observações Influentes")
