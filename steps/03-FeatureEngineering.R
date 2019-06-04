### FEATURE ENGINEERING

# Aplicando Engenharia de Atributos em Variaveis Numericas

# Verificando a taxa de retorno
# Observa-se que 96% das unidades não tem retorno e menos de 2% retornaram 1 unidade
table(dfSample$Dev_uni_proxima)
prop.table(table(dfSample$Dev_uni_proxima))

# Criando novas colunas no dataset de avaliacao
dfSelected <- dfSample %>%
  group_by(Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID, Demanda_uni_equil) %>%
  summarise(Unid = sum(Venta_uni_hoy),
            Venda = sum(Venta_hoy),
            Ret_Unid = sum(Dev_uni_proxima),
            Ret_Venda = sum(Dev_proxima)) %>%
  mutate(Avg_Venda = Venda / Unid,
         Taxa_Ret = Ret_Unid / (Unid+Ret_Unid)) %>%
  filter(!is.nan(Avg_Venda))

# Removendo a variavel 'Unid' pois é praticamente a mesma informacao da variavel target
# Ela permanecendo no dataset, prejudica a criacao do modelo
dfSelected$Unid <- NULL

# Verificando os dados estatisticos da variavel 'Demanda_uni_equil'
# identificamos que 95% dos dados tem media 24 e que os maiores valores passam de 2000
# por isso serao removidos os valores de demanda acima de 30
dfSelected <- dfSelected[-which(dfSelected$Demanda_uni_equil > 30),]
describe(dfSelected$Demanda_uni_equil)
hist(dfSelected$Demanda_uni_equil, xlab="Demanda", ylab="Frequencia")
boxplot(dfSelected$Demanda_uni_equil)

# Verificando a analise exploratoria, selecionamos somente o Canal 1
# pois tem o maior volume de dados e movimentacoes
#dfSelected <- dfSelected[which(dfSelected$Canal_ID %in% 1),]

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
dfSelected <- to.factors(dfSelected, variables = categorical.vars)
str(dfSelected)

# Normalizando as variáveis numericas
# Funcao para normalizar variáveis numericas 
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}
         
# Definindo as variaveis que serao normalizadas
numeric.vars <- c('Demanda_uni_equil','Venda','Ret_Unid','Ret_Venda','Avg_Venda','Taxa_Ret')
dfSelected <- scale.features(dfSelected, numeric.vars)
str(dfSelected)

# Removendo funcoes e variaveis da memoria
rm('to.factors')
rm('scale.features')
rm('categorical.vars')
rm('numeric.vars')