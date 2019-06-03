# Aplicando modelo aos dados de teste (arquivo TEST.CSV)

# Selecionando algumas colunas para previsao de correlacao
dfTeste2 <- dfTeste %>% select(Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID)

# Testando o modelo nos dados de teste
predicted <- predict(modeloGBM_final, n.trees = modeloGBM_final$n.trees, dfTeste2)

predicted_2 <- unscale(predicted)
View(predicted)

# Apresentando os resultados
RMSE(predicted, dfTeste)

submission <- data.frame(ID=testLargeData$id, Demanda_uni_equil=prediction)


?rescale