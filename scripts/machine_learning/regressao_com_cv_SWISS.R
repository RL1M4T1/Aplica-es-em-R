# carrega as bibliotecas
pacman::p_load(car, caret, corrplot, data.table, dplyr, forcats, funModeling, mltools, randomForest, tidyverse)

# carregar a base de dados
BASE_swiss  <-  swiss  

# AED 
status(BASE_swiss) # explorar a qualidade das variáveis
freq(BASE_swiss) # explorar os fatores
plot_num(BASE_swiss) # exploração das variáveis numéricas
profiling_num(BASE_swiss) # estatísticas das variáveis numéricas


# Treino e Teste: Pré-processamento
particaoBASE_swiss = createDataPartition(1:nrow(BASE_swiss), p=.7) # cria a partição 70-30
treinoBASE_swiss = BASE_swiss[particaoBASE_swiss$Resample1, ] # treino
testeBASE_swiss = BASE_swiss[-particaoBASE_swiss$Resample1, ] # - treino = teste

# Validação Cruzada: Pré-processamento
# Controle de treinamento
# train.control <- trainControl(method = "boot", number = 100)
train.control <- trainControl(method = "cv", number = 10) # controle de treino
# Treinamento
BASE_swiss_LM <- train(Infant.Mortality ~ Fertility + Education   , data = BASE_swiss, method = "lm", trControl = train.control)
# Sumários
print(BASE_swiss_LM)
summary(BASE_swiss_LM)

# Bagging
BASE_swiss_RF = randomForest(treinoBASE_swiss[ , c(1, 4)], treinoBASE_swiss[ ,6], ntree = 100, keep.forest=T, keep.inbag = TRUE, importance=T) # floresta aleatória

plot(BASE_swiss_RF)

varImp(BASE_swiss_RF, scale = T) # importância de cada variável
varImpPlot(BASE_swiss_RF, type=2) # importância de cada variável

# Boosting
# Treinamento
BASE_swiss_ADA <- train(Infant.Mortality ~ Fertility + Education   , data = BASE_swiss, method = "glmboost")

# evolução do modelo
plot(BASE_swiss_ADA)

# Sumários
print(BASE_swiss_ADA)
summary(BASE_swiss_ADA)
