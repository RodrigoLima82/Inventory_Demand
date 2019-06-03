### FEATURE SELECTION (Selecao de Variaveis)

# Buscando uma amostra do DataFrame de Treino para facilitar o analise
dfSample <- sample_n(dfTreino, 200000)

# Selecionando algumas colunas para previsao de correlacao
dfSample <- dfSample %>% select(Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID, Demanda_uni_equil)

# Visualizando dados do dataframe
View(dfSample)