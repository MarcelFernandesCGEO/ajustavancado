# Projeto V - Ajustamento Avançado
# Prof: TC Roberto Gomes
# Marcel Fernandes Gomes
# 21/05/2020

########## Bibliotecas ##########

# Carregar as bibliotecas necessárias
library(openxlsx)  #install.packages('openxlsx')
library(lubridate) #install.packages('lubridate')
library(ggplot2)   #install.packages('ggplot2')
library(GGally)    #install.packages('GGally')
library(readr)     #install.packages('readr')
library(forecast)  #install.packages('forecast')
library(fpp2)      #install.packages('fpp2')
library(TTR)       #install.packages('TTR')
library(dplyr)     #install.packages('dplyr')
library(plotly)    #install.packages('plotly')

########## Preparar os Dados ##########

# Ler dados do Brasil
dadosbr <- read.xlsx('covid19br28maio.xlsx', detectDates = TRUE, colNames = TRUE)

# Separar os dados do pais
dadospaisbr <- dadosbr[dadosbr$regiao == 'Brasil', ]

# Separar os dados do estado do CE
dadosce <- dadosbr[dadosbr$estado == 'CE', ]
dadosce <- dadosce[is.na(dadosce$codmun), ]
dadosce <- dadosce[!is.na(dadosce$regiao), ]
dadosceajuste <- read.xlsx('covid19ajuste.xlsx', sheet = 1, detectDates = TRUE, colNames = TRUE)
dadosce <- rbind(dadosceajuste, dadosce)

# Separar os dados do estado do RJ
dadosrj <- dadosbr[dadosbr$estado == 'RJ', ]
dadosrj <- dadosrj[is.na(dadosrj$codmun), ]
dadosrj <- dadosrj[!is.na(dadosrj$regiao), ]
dadosrjajuste <- read.xlsx('covid19ajuste.xlsx', sheet = 2, detectDates = TRUE, colNames = TRUE)
dadosrj <- rbind(dadosrjajuste, dadosrj)

# Separar os dados do estado do SP
dadossp <- dadosbr[dadosbr$estado == 'SP', ]
dadossp <- dadossp[is.na(dadossp$codmun), ]
dadossp <- dadossp[!is.na(dadossp$regiao), ]

########## Teste e Treino ##########

# Criar colunas de teste e treino
dadospaisbr$classe <- ''
dadosce$classe <- ''
dadosrj$classe <- ''
dadossp$classe <- ''
dadospaisbr[1:93, 17] <- 'Train'
dadospaisbr[89:93, 17] <- 'Test'
dadosce[1:73, 17] <- 'Train'
dadosce[69:73, 17] <- 'Test'
dadosrj[1:85, 17] <- 'Train'
dadosrj[81:85, 17] <- 'Test'
dadossp[1:93, 17] <- 'Train'
dadossp[89:93, 17] <- 'Test'

# Quebrar em tabelas de teste em treino
paisbr_train <- subset(dadospaisbr, classe == 'Train')
paisbr_test <- subset(dadospaisbr, classe == 'Test')
ce_train <- subset(dadosce, classe == 'Train')
ce_test <- subset(dadosce, classe == 'Test')
rj_train <- subset(dadosrj, classe == 'Train')
rj_test <- subset(dadosrj, classe == 'Test')
sp_train <- subset(dadossp, classe == 'Train')
sp_test <- subset(dadossp, classe == 'Test')

########## Definir Função ##########

# Função de Erro:
mape <- function(actual, pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

########## Séries Temporais ##########

# Serie Temporal:
dadospaisbr_ts <- ts(as.numeric(paisbr_train[ ,11]), start = ymd(paisbr_train[1,8]), frequency = 1)
dadosce_ts <- ts(as.numeric(ce_train[ ,11]), start = ymd(ce_train[1,8]), frequency = 1)
dadosrj_ts <- ts(as.numeric(rj_train[ ,11]), start = ymd(rj_train[1,8]), frequency = 1)
dadossp_ts <- ts(as.numeric(sp_train[ ,11]), start = ymd(sp_train[1,8]), frequency = 1)

########## Modelo Holt ##########

# Modelo Holt (SP, CE, RJ, Brasil)
holt_model_paisbr <- holt(dadospaisbr_ts, h = 5)
df_holt_paisbr <- as.data.frame(holt_model_paisbr)
write.xlsx(df_holt_paisbr, 'holt29mai02jun.xlsx')
win.graph(width = 27, height = 27)
plot(holt_model_paisbr)

holt_model_ce <- holt(dadosce_ts, h = 5)
df_holt_ce <- as.data.frame(holt_model_ce)
write.xlsx(df_holt_ce, 'holt29mai02jun.xlsx')
win.graph(width = 27, height = 27)
plot(holt_model_ce)

holt_model_rj <- holt(dadosrj_ts, h = 5)
df_holt_rj <- as.data.frame(holt_model_rj)
write.xlsx(df_holt_rj, 'holt29mai02jun.xlsx')
win.graph(width = 27, height = 27)
plot(holt_model_rj)

holt_model_sp <- holt(dadossp_ts, h = 5)
df_holt_sp <- as.data.frame(holt_model_sp)
write.xlsx(df_holt_sp, 'holt29mai02jun.xlsx')
win.graph(width = 27, height = 27)
plot(holt_model_sp)

# Adicionar o modelo Holt a tabela de teste:
paisbr_test$holt <- df_holt_paisbr$`Point Forecast`
ce_test$holt <- df_holt_ce$`Point Forecast`
rj_test$holt <- df_holt_rj$`Point Forecast`
sp_test$holt <- df_holt_sp$`Point Forecast`

# Rodar a função de verificação de erro:
mape(as.numeric(paisbr_test$casosAcumulado), paisbr_test$holt)
mape(as.numeric(ce_test$casosAcumulado), ce_test$holt)
mape(as.numeric(rj_test$casosAcumulado), rj_test$holt)
mape(as.numeric(sp_test$casosAcumulado), sp_test$holt)
