
# vecinos mas cercanos para asemejar nuevos lotes, con lotes anteriores
# Hugo Dorado
# 25-09-2018

library(caret)
library(tidyr)
library(ggplot2)

source('SCRIPTS/nearest_neighbor_FUN.R')


suelo_hist <- read.csv('DATOS/suelo_historico.csv',row.names = 1)

suelo_nuev <- read.csv('DATOS/suelo_nuevo.csv',row.names = 1)


# Analisis de datos exploratorios

suelo_hist_BX <- gather(suelo_hist,variable,value)

suelo_hist_BX$data <- 'Historico'

suelo_nuev_BX <- gather(suelo_nuev,variable,value)

suelo_nuev_BX$data <- 'Nuevo'

ggS <-
ggplot(
rbind(suelo_hist_BX,suelo_nuev_BX),aes(data,y=value)
)+geom_boxplot()+
  facet_wrap(~variable,scales='free')+ggtitle('Todos')

ggS

ggsave('RESULTADOS/soil_info.png',ggS)

# Normalizacion

normRG <- preProcess(suelo_hist,method = 'range')

norm_suelo_hist <- predict(normRG,suelo_hist)

norm_suelo_nuev <- predict(normRG,suelo_nuev)

distMat <- distance_matrix2(base=norm_suelo_hist,nuevo = norm_suelo_nuev)


# Vecinos mas cercano

vecinos_cercanos <- extract_neigthbors(suelo_hist,suelo_nuev,distMat,k=5)

head(vecinos_cercanos)

write.csv(vecinos_cercanos,'RESULTADOS/vecinos_mas_cercanos_suelo.csv',row.names = F)



