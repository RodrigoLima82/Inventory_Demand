### PREPARACAO E CARREGAMENTO DOS DATASETS

# Verifica se existe o arquivo RDA no disco
# Se existir carrega o arquivo
# Carregando os Pacotes
library(data.table)

# --------------------------------------------------------------------
# Carregando o arquivo de Treino

trainData = "dados/train.data.Rda" 

if (file.exists(trainData)) {
  dfTreino <- readRDS(file=trainData)
}else{
  # Carregando o Dataset "TRAIN.CSV"
  # Os dados brutos contém 74.180.464 linhas e 11 colunas (atributos). 
  # A coluna "Demanda_uni_equil" é o alvo.
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
  
  # Remover os DataFrame secundários
  rm('dfCidade')
  rm('dfCliente')
  rm('dfProduto')
  
  # Os dados brutos apos o merge ficou com 74.773.833 linhas e 15 colunas (atributos). 
  # Visualizando dados do dataframe
  View(dfTreino)
  str(dfTreino)
  
}

# --------------------------------------------------------------------
# Carregando o arquivo de Teste

testData = "dados/test.data.Rda" 

if (file.exists(testData)) {
  dfTeste <- readRDS(file=testData)
}else{
  # Carregando o Dataset "TEST.CSV"
  dtTeste   <- fread("dados/test.csv")

  # Criacao dos DataFrame
  dfTeste   <- as.data.frame(dtTeste)
  
  # Remover os DataTable
  rm('dtTeste')

  # Salvando DF no disco para acelerar o trabalho
  saveRDS(dfTeste, file="test.data.Rda")
  
}

# --------------------------------------------------------------------
# Nome das variáveis

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



