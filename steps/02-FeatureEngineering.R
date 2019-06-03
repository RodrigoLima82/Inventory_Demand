### FEATURE ENGINEERING

# Aplicando Engenharia de Atributos em Variaveis Numericas
# Verificar se existem valores ausentes (missing) em cada coluna
# Dados NA nao encontrados
any(is.na(dfTreino))

# Transformando a variavel target para unidade logaritma
# O objetivo é minimizar o RMSLE e RMSE
dfTreino$log_demand = log1p(dfTreino$Demanda_uni_equil) 

# Transformando variáveis numéricas em variáveis categóricas
# Funcao para converter variáveis para fator
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

# Definindo as variaveis que serao transformadas em fator
# Convertendo variaveis para fator
categorical.vars <- c('Semana', 'Canal_ID')
dfTreino <- to.factors(dfTreino, variables = categorical.vars)
str(dfTreino)

# Normalizando as variáveis numericas
# Funcao para normalizar variáveis numericas 
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}

# Definindo as variaveis que serao normalizadas
numeric.vars <- c('Venta_uni_hoy','Venta_hoy','Dev_uni_proxima','Dev_proxima','Demanda_uni_equil', 'log_demand')
dfTreino <- scale.features(dfTreino, numeric.vars)
str(dfTreino)
