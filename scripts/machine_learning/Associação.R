install.packages('pacman')
install.packages ("e1071")
### Regras de Associação 
pacman::p_load(ade4, arules, arulesCBA, arulesViz, car, caret, chunked, data.table, data.tree, dplyr, ff, ffbase, foreign, funModeling, ggparty, ggplot2, ggtree, gplots, LaF, Metrics, party, partykit, permimp,plm, randomForest, rattle, readr, REEMtree, sqldf)

# Github
ENEM_ESCOLA_2018 <- read.csv2('https://raw.githubusercontent.com/hugoavmedeiros/etl_com_r/master/bases_tratadas/ENEM_ESCOLA_2018.csv', stringsAsFactors = T) # carregando a base já tratada para o ambiente do R

ENEM_ESCOLA_2018

ENEM_ESCOLA_2018 <- ENEM_ESCOLA_2018 %>% select(ID, tipo, media, TDI_03, MHA_03) # selecionando variáveis de interesse

ENEM_ESCOLA_2018

# Pré-processamento de variáveis
ENEM_ESCOLA_2018[ , -1] <- discretizeDF(ENEM_ESCOLA_2018[ , -1]) # transforma variáveis numéricas em fatores
# Pré-processamento de base
particaoENEM = createDataPartition(1:nrow(ENEM_ESCOLA_2018), p=.7) # cria a partição 70-30
treinoENEM = ENEM_ESCOLA_2018[particaoENEM$Resample1, ] # treino
testeENEM = ENEM_ESCOLA_2018[-particaoENEM$Resample1, ] # - treino = teste

treinoENEM <- treinoENEM[ , -1]
testeENEM <- testeENEM[ , -1]

#Regra Fighonathi

# Modelagem
regrasENEM = arulesCBA::CBA(media ~ ., treinoENEM, supp=0.01, conf=0.01) 
inspect(regrasENEM$rules)
plot(regrasENEM$rules)
predicaoRegrasENEM <- predict(regrasENEM, testeENEM)
confusionMatrix(predicaoRegrasENEM, testeENEM$media)

# Modelagem TST 1
regrasENEM = arulesCBA::CBA(media ~ ., treinoENEM, supp=0.10, conf=0.01) 
inspect(regrasENEM$rules)
plot(regrasENEM$rules)
predicaoRegrasENEM <- predict(regrasENEM, testeENEM)
confusionMatrix(predicaoRegrasENEM, testeENEM$media)


# Modelagem TST 2
regrasENEM = arulesCBA::CBA(media ~ ., treinoENEM, supp=0.06, conf=0.06) 
inspect(regrasENEM$rules)
plot(regrasENEM$rules)
predicaoRegrasENEM <- predict(regrasENEM, testeENEM)
confusionMatrix(predicaoRegrasENEM, testeENEM$media)


# Modelagem TST 3
regrasENEM = arulesCBA::CBA(media ~ ., treinoENEM, supp=0.11, conf=0.11) 
inspect(regrasENEM$rules)
plot(regrasENEM$rules)
predicaoRegrasENEM <- predict(regrasENEM, testeENEM)
confusionMatrix(predicaoRegrasENEM, testeENEM$media)


# Modelagem TST 4
regrasENEM = arulesCBA::CBA(media ~ ., treinoENEM, supp=0.20, conf=2.00) 
inspect(regrasENEM$rules)
plot(regrasENEM$rules)
predicaoRegrasENEM <- predict(regrasENEM, testeENEM)
confusionMatrix(predicaoRegrasENEM, testeENEM$media)



