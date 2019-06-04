# Analisando o resultado atraves de gráficos

# --------------------------------------------------------------------
# Modelo Linear Model
predLM <- predict(modeloLM, newdata = dados_teste)
plot(dados_teste$Demanda_uni_equil, predLM)

# --------------------------------------------------------------------
# Modelo Random Forest
predRF <- predict(modeloRF, newdata = dados_teste)
plot(dados_teste$Demanda_uni_equil, predRF)

# --------------------------------------------------------------------
# Modelo Generalized Boosted Regression Modeling (GBM)
predGBM <- predict(modeloGBM, newdata = dados_teste)
plot(dados_teste$Demanda_uni_equil, predGBM)

# --------------------------------------------------------------------
# Modelo GBM Otimizado
predGBM_final <- predict(modeloGBM_final, n.trees = modeloGBM_final$n.trees, dados_teste)
plot(dados_teste$Demanda_uni_equil, predGBM_final)
