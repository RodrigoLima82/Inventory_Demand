# Otimizando o modelo Generalized Boosted Regression Modeling (GBM)

# Carregando os Pacotes
library(gbm)

# Modificando os Hyperparametros
hyperParam <- expand.grid(
  shrinkage = c(.01, .05, .1),
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(5, 7, 10),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,
  min_RMSE = 0
)

for(i in 1:nrow(hyperParam)) {
  # Treinando o modelo
  modeloGBM_v2 <- gbm(
    formula = Demanda_uni_equil ~ .,
    distribution = "gaussian",
    data = dados_treino,
    n.trees = 200,
    interaction.depth = hyperParam$interaction.depth[i],
    shrinkage = hyperParam$shrinkage[i],
    n.minobsinnode = hyperParam$n.minobsinnode[i],
    bag.fraction = hyperParam$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL,
    verbose = FALSE
  )
  
  hyperParam$optimal_trees[i] <- which.min(modeloGBM_v2$valid.error)
  hyperParam$min_RMSE[i] <- sqrt(min(modeloGBM_v2$valid.error))
}

hyperParam %>% 
  arrange(min_RMSE) %>%
  head(10)


# Treinando o modelo final com os melhores parametros
modeloGBM_final <- gbm(
  formula = Demanda_uni_equil ~ .,
  distribution = "gaussian",
  data = dados_treino,
  n.trees = 180,
  interaction.depth = 7,
  shrinkage = 0.1,
  n.minobsinnode = 5,
  bag.fraction = 1, 
  train.fraction = 1,
  n.cores = NULL,
  verbose = FALSE
)  

# Visualizando as variaveis de influencia
par(mar = c(5, 8, 1, 1))
summary(
  modeloGBM_final, 
  cBars = 10,
  method = relative.influence,
  las = 2
)
