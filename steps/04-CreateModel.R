### CRIANDO E AVALIANDO O MODELO

# Carregando os Pacotes
library(caret)

# Gerando dados de treino e de teste
splits <- createDataPartition(dfSample$log_demand, p=0.7, list=FALSE)

# Separando os dados
dados_treino <- dfSample[ splits,]
dados_teste <- dfSample[-splits,]

# --------------------------------------------------------------------
# Construindo um modelo Linear Model
modeloLM <- train(log_demand ~ ., data = dados_treino, method = "lm")

# Resumo do Modelo
print(modeloLM)
#RMSE    : 0.9398102
#Rsquared: 0.1180286
#MAE     : 0.7306142

# Aplicando e visualizando o modelo nos dados de teste
predLM <- predict(modeloLM, newdata = dados_teste)
plot(dados_teste$log_demand, predLM)


# --------------------------------------------------------------------
# Construindo um modelo Generalized Boosted Regression Modeling (GBM)
modeloGBM <- train(log_demand ~ .,data=dados_treino, method="gbm", verbose=FALSE)

# Resumo do Modelo
print(modeloGBM)
#RMSE    : 0.8340822
#Rsquared: 0.3196709
#MAE     : 0.6419830

# Aplicando e visualizando o modelo nos dados de teste
predGBM <- predict(modeloGBM, newdata = dados_teste)
plot(dados_teste$log_demand, predGBM)

