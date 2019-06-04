### Prevendo Demanda de Estoque com Base em Vendas ###
## Formacao Cientista de Dados - DSA (https://www.datascienceacademy.com.br)##

### Preparacao e Carregamento dos datasets
# Os arquivos sao grandes por isso segue o caminho para baixar
# https://www.kaggle.com/c/grupo-bimbo-inventory-demand/data

# Verifica se existe o arquivo RDA no disco
# Se existir carrega o arquivo
# Carregando os Pacotes
library(data.table)

### --------------------------------------------------------------------
### Carregando o arquivo de Treino ###

trainData = "dados/train.data.Rda" 

if (file.exists(trainData)) {
  dfTreino <- readRDS(file=trainData)
  rm('trainData')
}else{
  # Carregando o Dataset "TRAIN.CSV"
  # Os dados brutos contem 11 colunas (atributos). 
  # A coluna "Demanda_uni_equil" eh o alvo.
  dtTreino  <- fread("dados/train.csv")
  dtCliente <- fread("dados/cliente_tabla.csv")
  dtProduto <- fread("dados/producto_tabla.csv")
  dtCidade  <- fread("dados/town_state.csv")
  
  # Criacao dos DataFrame
  dfTreino  <- as.data.frame(dtTreino)
  dfCliente <- as.data.frame(dtCliente)
  dfProduto <- as.data.frame(dtProduto)
  dfCidade  <- as.data.frame(dtCidade)
  
  # Remover os DataTable
  rm('dtTreino')
  rm('dtCliente')
  rm('dtProduto')
  rm('dtCidade')
  
  # Merge dos DataFrame
  dfTreino <- merge(dfTreino, dfCidade, by=c("Agencia_ID"))
  dfTreino <- merge(dfTreino, dfCliente, by=c("Cliente_ID"))
  dfTreino <- merge(dfTreino, dfProduto, by=c("Producto_ID"))
  
  # Salvando DF no disco para acelerar o trabalho
  saveRDS(dfTreino, file="dados/train.data.Rda")
  
  # Remover os DataFrame secundarios
  rm('dfCidade')
  rm('dfCliente')
  rm('dfProduto')
  
  # Os dados brutos apos o merge ficou com 15 colunas (atributos). 
  # Visualizando dados do dataframe
  #View(dfTreino)
  #str(dfTreino)
  
}

# --------------------------------------------------------------------
# Carregando o arquivo de Teste
# Utilizado caso queira prever as demandas dos produtos 
# no dataset de teste disponibilizado no kaggle

# testData = "dados/test.data.Rda" 
# 
# if (file.exists(testData)) {
#   dfTeste <- readRDS(file=testData)
# }else{
#   # Carregando o Dataset "TEST.CSV"
#   dtTeste   <- fread("dados/test.csv")
# 
#   # Criacao dos DataFrame
#   dfTeste   <- as.data.frame(dtTeste)
#   
#   # Remover os DataTable
#   rm('dtTeste')
# 
#   # Salvando DF no disco para acelerar o trabalho
#   saveRDS(dfTeste, file="test.data.Rda")
#   
# }

# --------------------------------------------------------------------
# Nome das variaveis

# dfTreino
# Semana, Agencia_ID (+Town, +State), Canal_ID, Ruta_SAK, Cliente_ID (+NombreCliente), Producto_ID (+NombreProducto), Venta_uni_hoy, Venta_hoy, Dev_uni_proxima, Dev_proxima, Demanda_uni_equil

### Data fields
# Semana — Week number (From Thursday to Wednesday)
# Agencia_ID — Sales Depot ID
# Canal_ID — Sales Channel ID
# Ruta_SAK — Route ID (Several routes = Sales Depot)
# Cliente_ID — Client ID
# NombreCliente — Client name
# Producto_ID — Product ID
# NombreProducto — Product Name
# Venta_uni_hoy — Sales unit this week (integer)
# Venta_hoy — Sales this week (unit: pesos)
# Dev_uni_proxima — Returns unit next week (integer)
# Dev_proxima — Returns next week (unit: pesos)
# Demanda_uni_equil — Adjusted Demand (integer) (This is the target you will predict)

### -------------------------------------------------------------------
### Analise exploratoria de dados ###
# Creditos ao Fabiensvs: https://www.kaggle.com/fabienvs

# Carregando os Pacotes
library(dplyr)
library(Hmisc)
library(stringr)
library(ggplot2)
library(scales)
library(treemap)

# Verificando os dados estatisticos da variavel 'Demanda_uni_equil'
# identificamos que 95% dos dados tem media 24 e que os maiores valores passam de 2000
# por isso serao removidos os valores de demanda acima de 30
describe(dfTreino$Demanda_uni_equil)
hist(dfTreino$Demanda_uni_equil, "FD", xlab="Demanda", ylab="Frequencia")

# -------------------------------------------------------------------
# Avaliando a variavel PRODUTO e seus relacionamentos
# A variavel 'NombreProducto' contem alem do nome, suas caracteristicas (qtde pacotes, peso, marca)
head(dfTreino %>% distinct(NombreProducto))

# Primeiro, vamos extrair apenas o NOME DO PRODUTO da coluna NombreProducto
# Criando coluna 'Prod_nome'
# Removendo a matrix criada (temporaria)
m <- str_extract_all(dfTreino$NombreProducto, '^(\\D*)', simplify = TRUE)
dfTreino$Prod_nome <- as.vector(m)
head(dfTreino %>% distinct(Prod_nome))
rm(m)

# -------------------------------------------------------------------
# Analisando dados de Produtos
produto <- dfTreino %>% 
  group_by(Producto_ID, Prod_nome) %>%
  summarise(Unid = sum(Venta_uni_hoy),
            Venda = sum(Venta_hoy),
            Ret_Unid = sum(Dev_uni_proxima),
            Ret_Venda = sum(Dev_proxima),
            Liquida = sum(Demanda_uni_equil)) %>%
  mutate(Avg_Venda = Venda / Unid,
         Taxa_Ret = Ret_Unid / (Unid+Ret_Unid)) %>%
  filter(!is.nan(Avg_Venda)) %>%
  arrange(desc(Unid))

# Observa-se que os produtos com mais vendas sao Paes Branco e Integrais, alem do Nito e Tortillinas
treemap(produto[1:100, ], 
        index=c("Prod_nome"), vSize="Venda", vColor="Avg_Venda", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="% Venda", title="Top 100 produtos")

# Observa-se que os produtos com mais retorno de vendas sao Nito, Gansito e Rebanada
treemap(produto[1:100, ], 
        index=c("Prod_nome"), vSize="Unid", vColor="Taxa_Ret", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="% Retorno (Unidade)", title="Top 100 produtos")

# Visualizando os dados do histograma (media de venda dos produtos)
ggplot(produto, aes(x=Avg_Venda))+
  geom_histogram(aes(y=..density..), fill="gray", color="black", alpha="0.3")+
  geom_density(fill="red", alpha="0.3")+
  scale_x_continuous(name="Media de Venda dos Produtos", lim=c(0, 50))+
  scale_y_continuous(name="Densidade", labels=percent)+
  theme_bw()

rm(produto)

# -------------------------------------------------------------------
# Analisando dados de Produtos por Canais
canal.produto <- dfTreino %>% 
  group_by(Producto_ID) %>%
  summarise(n_canal = n_distinct(Canal_ID))

# Os produtos sao entregues por varios canais
ggplot(canal.produto)+
  geom_histogram(aes(x=n_canal), fill="red", color="black", alpha="0.3", binwidth=1)+
  scale_x_continuous(name="Nro. Canal", breaks=1:10, lim=c(1, 10))+
  scale_y_continuous(name="Nro. Produtos")+
  theme_bw()

rm(canal.produto)

# -------------------------------------------------------------------
# Analisando dados de Produtos x Clientes
produto.cliente <- dfTreino %>%
  group_by(Cliente_ID) %>%
  summarise(n_produtos = n_distinct(Producto_ID))

ggplot(produto.cliente)+
  geom_histogram(aes(x=n_produtos), fill="red", color="black", alpha="0.3", binwidth=2)+
  scale_x_continuous(name="Nro de Produtos por Clientes", lim=c(0, 150))+
  scale_y_continuous(name="Nro de Clientes", labels=function(x)paste(x/1000, "k"))+
  theme_bw()

rm(produto.cliente)

# -------------------------------------------------------------------
# Analisando dados das Semanas
ggplot(dfTreino %>% sample_frac(0.005))+
  geom_histogram(aes(x=Semana), color="black", fill="red", alpha=0.5)+
  scale_x_continuous(breaks=1:10)+
  scale_y_continuous(name="Cliente / Produtos Entregues")+
  theme_bw()

# -------------------------------------------------------------------
# Analisando dados das Agencias
agencia <- dfTreino %>%
  group_by(Agencia_ID) %>%
  summarise(Unid = sum(Venta_uni_hoy),
            Venda = sum(Venta_hoy),
            Ret_Unid = sum(Dev_uni_proxima),
            Ret_Venda = sum(Dev_proxima),
            Liquida = sum(Demanda_uni_equil)) %>%
  mutate(Venda_Liquida = Venda - Ret_Venda,
         Taxa_Ret = Ret_Unid / (Unid+Ret_Unid)) %>%
  arrange(desc(Unid))

ggplot(agencia, aes(x=Unid/7))+
  geom_histogram(fill="red", color="gray", binwidth=10000)+
  scale_x_continuous(name="Unidades por Semana", labels=function(x)paste(x/1000, "k"))+
  scale_y_continuous(name="Agencias")+
  theme_bw()

# Observa-se que a agencia 1114 tem uma alta taxa de retorno das unidades
treemap(agencia[1:100, ], 
        index=c("Agencia_ID"), vSize="Unid", vColor="Taxa_Ret", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="% Retorno (Unidade)", title="Top 100 Agencias")

rm(agencia)

# -------------------------------------------------------------------
# Analisando dados de Estados
estado <- dfTreino %>%
  group_by(State, Semana) %>%
  summarise(Unid = sum(Venta_uni_hoy),
            Venda = sum(Venta_hoy),
            Ret_Unid = sum(Dev_uni_proxima),
            Ret_Venda = sum(Dev_proxima),
            Liquida = sum(Demanda_uni_equil)) %>%
  mutate(Avg_Venda = Venda / Unid,
         Taxa_Ret = Ret_Unid / (Unid+Ret_Unid)) %>%
  arrange(desc(Unid))

# Observa-se que apenas alguns estados tem um alto volume de venda semanal
ggplot(estado)+
  geom_bar(aes(x=Semana, y=Unid, fill=Taxa_Ret), stat="identity", color="black")+
  scale_y_continuous(labels=function(x)paste(x/1e6, "m"))+
  scale_fill_gradient(name="Taxa\nRetorno %", low="white", high="red")+
  facet_wrap(~State)+
  ggtitle("Estados")+
  theme_bw()

rm(estado)

# -------------------------------------------------------------------
# Analisando dados dos Canais de Venda
canal <- dfTreino %>%
  group_by(Canal_ID, Semana) %>%
  summarise(Unid = sum(Venta_uni_hoy),
            Venda = sum(Venta_hoy),
            Ret_Unid = sum(Dev_uni_proxima),
            Ret_Venda = sum(Dev_proxima),
            Liquida = sum(Demanda_uni_equil)) %>%
  mutate(Venda_Liquida = Venda - Ret_Venda,
         Avg_Venda = Venda / Unid,
         Taxa_Ret = Ret_Unid / (Unid+Ret_Unid)) %>%
  arrange(desc(Unid))

# Observa-se que o canal 1 tem muito mais movimentacao de Unidades
treemap(canal, index=c("Canal_ID"), vSize="Unid", type="index", title="Canais")

# Observa-se que o canal 5 e 8 tem maior taxa de retorno
ggplot(canal)+
  geom_bar(aes(x=Semana, y=Unid, fill=Taxa_Ret), stat="identity", color="black")+
  scale_y_continuous(labels=function(x)paste(x/1e6, "m"))+
  scale_fill_gradient(name="Taxa\nRetorno %", low="white", high="red")+
  facet_wrap(~Canal_ID, scale="free")+
  theme_bw()

rm(canal)

# -------------------------------------------------------------------
# Analisando dados das Rotas
rotas <- dfTreino %>% group_by(Ruta_SAK) %>%
  summarise(n_Agencias = n_distinct(Agencia_ID),
            n_Clientes = n_distinct(Cliente_ID),
            Unid = sum(Venta_uni_hoy),
            Ret_Unid = sum(Dev_uni_proxima)) %>%
  mutate(Return_Rate = Ret_Unid / (Ret_Unid)) %>%
  arrange(desc(Unid))

# Observa-se que mais de 80% das rotas movimentam menos de 30k de produtos por semana
ggplot(rotas, aes(x=Unid/7))+
  geom_histogram(fill="red", color="gray", binwidth=5000)+
  scale_x_continuous(name="Unidades por Semana", labels=function(x)paste(x/1000, "k"))+
  scale_y_continuous(name="Rotas")+
  theme_bw()

rm(rotas)

# -------------------------------------------------------------------
# Analisando dados de Clientes
cliente <- dfTreino %>%
  group_by(Cliente_ID, NombreCliente) %>%
  summarise(Unid = sum(Venta_uni_hoy),
            Venda = sum(Venta_hoy),
            Ret_Unid = sum(Dev_uni_proxima),
            Ret_Venda = sum(Dev_proxima),
            Liquida = sum(Demanda_uni_equil)) %>%
  mutate(Venda_Liquida = Venda - Ret_Venda,
         Avg_Venda = Venda / Unid,
         Taxa_Ret = Ret_Unid / (Unid+Ret_Unid)) %>%
  arrange(desc(Unid))

# Observa-se que o maior cliente é 'PUEBLA REMISION'
treemap(cliente[1:100, ], 
        index=c("NombreCliente"), vSize="Unid", vColor="Taxa_Ret", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="Taxa de Retorno %", title="Top 100 clientes")

rm(cliente)

### -------------------------------------------------------------------
### FEATURE SELECTION (Selecao de Variaveis)

# Buscando uma amostra do DataFrame de Treino para facilitar a analise
dfSample <- sample_n(dfTreino, 50000)

# Verificar se existem valores ausentes (missing) em cada coluna
# Dados NA nao encontrados
any(is.na(dfSample))


### -------------------------------------------------------------------
### Aplicando Engenharia de Atributos em Variaveis ###
# Verificando a taxa de retorno
# Observa-se que 96% das unidades nao tem retorno e menos de 2% retornaram 1 unidade
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

# Removendo a variavel 'Unid' pois eh praticamente a mesma informacao da variavel target
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

# Transformando variaveis numericas em variaveis categoricas
# Funcao para converter variaveis para fator
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

# Normalizando as variaveis numericas
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

### -------------------------------------------------------------------
### Criando e Avaliando o modelo ###

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
print(modeloLM)

# --------------------------------------------------------------------
# Construindo um modelo Random Forest
# controlRF <- trainControl(method="cv", number=4)
# modeloRF <- train(Demanda_uni_equil ~ ., data = dados_treino, method = "rf", metric="RMSE", trControl=controlRF)
# 
# # Resumo do Modelo Random Forest
# print(modeloRF)

# --------------------------------------------------------------------
# Construindo um modelo Generalized Boosted Regression Modeling (GBM)
controlGBM <- trainControl(method="cv", number=4)
modeloGBM <- train(Demanda_uni_equil ~ ., data=dados_treino, method="gbm", verbose=FALSE, metric="RMSE", trControl=controlGBM)

# Resumo do Modelo GBM
print(modeloGBM)

### -------------------------------------------------------------------
### Otimizando o modelo Generalized Boosted Regression Modeling (GBM) ###

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
    formula = Demanda_uni_equil ~ .,
    distribution = "gaussian",
    data = dados_treino,
    n.trees = 200,
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


# Treinando o modelo final com os melhores parametros
modeloGBM_final <- gbm(
  formula = Demanda_uni_equil ~ .,
  distribution = "gaussian",
  data = dados_treino,
  n.trees = 180,
  interaction.depth = 7,
  shrinkage = 0.1,
  n.minobsinnode = 5,
  bag.fraction = 1, 
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

### -------------------------------------------------------------------
### Analisando o resultado atraves de gráficos ###

# --------------------------------------------------------------------
# Modelo Linear Model
predLM <- predict(modeloLM, newdata = dados_teste)
plot(dados_teste$Demanda_uni_equil, predLM)

# --------------------------------------------------------------------
# Modelo Random Forest
# predRF <- predict(modeloRF, newdata = dados_teste)
# plot(dados_teste$Demanda_uni_equil, predRF)

# --------------------------------------------------------------------
# Modelo Generalized Boosted Regression Modeling (GBM)
predGBM <- predict(modeloGBM, newdata = dados_teste)
plot(dados_teste$Demanda_uni_equil, predGBM)

# --------------------------------------------------------------------
# Modelo GBM Otimizado
predGBM_final <- predict(modeloGBM_final, n.trees = modeloGBM_final$n.trees, dados_teste)
plot(dados_teste$Demanda_uni_equil, predGBM_final)

