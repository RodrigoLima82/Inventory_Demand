### FEATURE SELECTION (Selecao de Variaveis)

# Buscando uma amostra do DataFrame de Treino para facilitar a analise
dfSample <- sample_n(dfTreino, 100000)

# Verificar se existem valores ausentes (missing) em cada coluna
# Dados NA nao encontrados
any(is.na(dfSample))
