### FEATURE SELECTION (Selecao de Variaveis)

# Selecionando algumas colunas para previsao de correlacao
dfSampleSelected <- dfSample %>% select(Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID, Demanda_uni_equil)

# Visualizando dados do dataframe
View(dfSampleSelected)