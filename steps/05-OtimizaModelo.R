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
    formula = log_demand ~ .,
    distribution = "gaussian",
    data = dados_treino,
    n.trees = 150,
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


# Treinando o modelo final
modeloGBM_final <- gbm(
  formula = log_demand ~ .,
  distribution = "gaussian",
  data = dados_treino,
  n.trees = 150,
  interaction.depth = 7,
  shrinkage = 0.1,
  n.minobsinnode = 7,
  bag.fraction = 0.8, 
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

# Aplicando e visualizando o modelo nos dados de teste
predGBM_final <- predict(modeloGBM_final, n.trees = modeloGBM_final$n.trees, dados_teste)
plot(dados_teste$log_demand, predGBM_final)

# Testando o modelo nos dados de teste
predGBM_final <- predict(modeloGBM_final, n.trees = modeloGBM_final$n.trees, dados_teste)

# Apresentando os resultados estatisticos do modelo
RMSE_gbm_v2 <- RMSE(dados_teste$log_demand, predGBM_final)
print(RMSE_gbm_v2)
#RMSE    : 0.7855224
#Rsquared: xx
#MAE     : xx


