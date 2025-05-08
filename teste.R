## TESTE ##
#loop do código main
rm(list=ls())
library(dplyr)
library(stringr)
library(gdata) 
#### PREPARANDO AMBIENTE PARA CÓDIGO PYTHON  ####
library(reticulate)
# Alterar instância do Python
#use_python("C:/Users/F8039139/anaconda3/python.exe", required = T) # Monique
use_python("C:/Program Files/Python37/python.exe", required = T) # Thaina
# Verificar instância do Python
py_config()

##### IMPORTANDO BASES ####
hoje = lubridate::today()
#semana = paste0('W',lubridate::epiweek(hoje)-2) #WeekAtual-1
ano = "2023"
semana = "W10"

cels <- c("ALRGO_0002",
          "ALGUP_0001",
          "ALMCO_0017",
          "ALMCO_0122",
          "ALMCO_0151",
          "ALMCO_0220",
          "ALMCO_0150",
          'ALMCO_0240',
          "ALPND_0001",
          "ALMCO_0001")

## BASE 2 HMM
base2hmm_raw <- read.table(paste0('Inputs/',ano,'/',semana,'/Relatório Personalizado LTE - HMM_TBR.txt'), header = FALSE, sep = ";", row.names = NULL, stringsAsFactors = F, fileEncoding = "UTF-8")
names(base2hmm_raw) <- as.matrix(base2hmm_raw[1,])
base2hmm_raw <- base2hmm_raw[-1,]

teste_hmm <- base2hmm_raw %>% filter(Célula %in% cels)
check_hmm <- length(unique(teste_hmm$Célula)) == length(cels)

## BASE OCUPAÇÃO
#baseocup2_raw = readxl::read_excel(paste0("Inputs/",ano,'/', "W45", "/ALL_Relatório de Ocupação de Células 4G – Semanal.xlsx"))
baseocup_raw <- read.table(paste0("Inputs/",ano,'/', semana, "/ALL_Relatório de Ocupação de Células 4G – Semanal.txt"), header = FALSE, sep = ";", row.names = NULL, stringsAsFactors = F, fileEncoding = "UTF-8",  quote = "")
names(baseocup_raw) <- as.matrix(baseocup_raw[1,])
baseocup_raw <- baseocup_raw[-1,]
df <- baseocup_raw %>% select(`Station ID`,Célula, Classificação)

teste_ocup_w12 <- df %>% filter(`Station ID` %in% cels)
check_ocup <- length(unique(teste_ocup_w10$`Station ID`)) == length(cels)
