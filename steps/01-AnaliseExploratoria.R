### ANALISE EXPLORATORIA DE DADOS ###
# Apoio do Fabiensvs: https://www.kaggle.com/fabienvs

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