# carregar as bibliotecas
pacman::p_load(cluster, ggplot2)

# pré-processamento
face <- read.csv2('https://raw.githubusercontent.com/hugoavmedeiros/cd_com_r/master/bases_tratadas/facebook_2021.csv', stringsAsFactors = T)  [ , 4 : 12]
face2 <- read.csv2('https://raw.githubusercontent.com/hugoavmedeiros/cd_com_r/master/bases_tratadas/facebook_2021.csv', stringsAsFactors = T)  

table(face2$status_type)
str(face)

# setar semente aleatória
set.seed(1)

# Agrupamento com kmeans
cls <- kmeans(x = face, centers = 4) # aprendizagem ns
face$cluster <- as.factor(cls$cluster) # passamos os clusters para a base original
head(face)

# plot nativo do R
plot(face)

# plot com função própria do pacote
clusplot(face, cls$cluster, xlab = 'Fator1', ylab = 'Fator2', main = 'Agrupamento Estudantes', lines = 0, shade = F, color = TRUE, labels = 2)

# plot com ggplot
ggplot() +
  geom_point(data = face, 
             mapping = aes(x = num_comments, 
                           y = num_shares, 
                           colour = cluster)) +
  geom_point(mapping = aes_string(x = cls$centers[, "num_comments"], 
                                  y = cls$centers[, "num_shares"]),
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = cls$centers[, "num_comments"], 
                                 y = cls$centers[, "num_shares"],
                                 label = 1:4),
            color = "black", size = 4) +
  theme_light()

## comparação
face$Species <- iris$Species
