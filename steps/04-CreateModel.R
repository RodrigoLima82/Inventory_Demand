### CRIANDO E AVALIANDO O MODELO

# Carregando os Pacotes
library(caret)

# Gerando dados de treino e de teste
splits <- createDataPartition(dfSelected$Demanda_uni_equil, p=0.7, list=FALSE)

# Separando os dados
dados_treino <- dfSelected[ splits,]
dados_teste <- dfSelected[-splits,]

# --------------------------------------------------------------------
# Construindo um modelo Linear Model
controlLM <- trainControl(method="cv", number=5)
modeloLM <- train(Demanda_uni_equil ~ ., data = dados_treino, method = "lm", metric="RMSE", trControl=controlLM)

# Resumo do Modelo Linear Model
# O Rsquared ficou muito alto (pode ser problema de overfitting devido % de nao retorno que é alto)
print(modeloLM)

# --------------------------------------------------------------------
# Construindo um modelo Random Forest
controlRF <- trainControl(method="cv", number=4)
modeloRF <- train(Demanda_uni_equil ~ ., data = dados_treino, method = "rf", metric="RMSE", trControl=controlRF)

# Resumo do Modelo Random Forest
print(modeloRF)

# --------------------------------------------------------------------
# Construindo um modelo Generalized Boosted Regression Modeling (GBM)
controlGBM <- trainControl(method="cv", number=4)
modeloGBM <- train(Demanda_uni_equil ~ ., data=dados_treino, method="gbm", verbose=FALSE, metric="RMSE", trControl=controlGBM)

# Resumo do Modelo GBM
print(modeloGBM)