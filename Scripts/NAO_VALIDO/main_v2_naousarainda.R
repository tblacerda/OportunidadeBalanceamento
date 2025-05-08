
#### IMPORT DAS BASES DA SEMANA ####
## BASE 2 HMM
raw_base2hmm <- read.table(paste0('Inputs/',ano,'/',semana,'/Relatório Personalizado LTE - HMM_TBR.txt'), header = FALSE, sep = ";", row.names = NULL, stringsAsFactors = F, fileEncoding = "UTF-8")
names(raw_base2hmm) <- as.matrix(raw_base2hmm[1,])
raw_base2hmm <- raw_base2hmm[-1,]

## BASE OCUPAÇÃO
raw_baseocup <- read.table(paste0("Inputs/",ano,'/', semana, "/ALL_Relatório de Ocupação de Células 4G – Semanal.txt"), header = FALSE, sep = ";", row.names = NULL, stringsAsFactors = F, fileEncoding = "UTF-8",  quote = "")
names(raw_baseocup) <- as.matrix(raw_baseocup[1,])
raw_baseocup <- raw_baseocup[-1,]


#### UNINDO BASES ####

# Ultilizando max volume da 2HMM/semana para basekpi   
basekpi <- raw_base2hmm %>%
  #Seleção apenas das colunas necessárias neste relatório
  select(Dia, `Dia da Semana`,Célula,PRB_UTIL_MEAN_DL,USERS_RRC_CONN_MEAN_AVG,PDCCH_CCE_UTIL,THROU_USER_PDCP_DL,`VOLUME_DADOS_DL_ALLOP 4G`,CQI_MEAN) %>%
  #Tratamento da formatação das colunas de kpis
  mutate(
    PRB_UTIL_MEAN_DL = as.numeric(str_replace_all(str_remove_all(PRB_UTIL_MEAN_DL,"[.%]"),"[,]","."))/100,
    USERS_RRC_CONN_MEAN_AVG = as.numeric(str_replace_all(str_remove_all(USERS_RRC_CONN_MEAN_AVG,"[.]"),"[,]",".")),
    PDCCH_CCE_UTIL = as.numeric(str_replace_all(str_remove_all(PDCCH_CCE_UTIL,"[.%]"),"[,]","."))/100,
    THROU_USER_PDCP_DL = as.numeric(str_replace_all(str_remove_all(THROU_USER_PDCP_DL,"[.]"),"[,]",".")),
    `VOLUME_DADOS_DL_ALLOP 4G`=as.numeric(str_replace_all(str_remove_all(`VOLUME_DADOS_DL_ALLOP 4G`,"[.]"),"[,]",".")),
    CQI_MEAN = as.numeric(str_replace_all(str_remove_all(CQI_MEAN,"[.]"),"[,]","."))
    ) %>% 
  #Escolhendo apenas o maior valor de Volume de 2hmm da semana para utilizar como valor para aquela célula
  group_by(Célula) %>%
  slice(which.max(`VOLUME_DADOS_DL_ALLOP 4G`)) %>%
  select(Célula,PRB_UTIL_MEAN_DL,USERS_RRC_CONN_MEAN_AVG,PDCCH_CCE_UTIL,THROU_USER_PDCP_DL,`VOLUME_DADOS_DL_ALLOP 4G`,CQI_MEAN)

basekpi[is.na(basekpi)] <- 0

# Tratamento Base Ocupação
baseocup <- raw_baseocup %>%
  select(`Semana Ocupação`, Fornecedor, Banda, Célula, `Classificação Populacional`,`Re-classificação LT`,`Flag Mocn`,Vol_Total_DlUl_Allop_LT,Vol_Total_DlUl_Tim_LT,`Percentual da Cidade`,`Percentual da Célula`) %>%
  unique() %>%
  rename(Classificação = `Re-classificação LT`) %>%
  mutate(`Percentual da Célula` = as.numeric(str_replace_all(str_remove_all(`Percentual da Célula`,"[.%]"),"[,]","."))/100,
         `Percentual da Cidade` = as.numeric(str_replace_all(str_remove_all(`Percentual da Cidade`,"[.%]"),"[,]","."))/100)

#Tratamento de duplicatas
baseocup_duplicated <- baseocup %>% #retirando duplicatas com classificação diferente (escolhe-se a pior)
  filter(Célula %in% baseocup$Célula[duplicated(baseocup$Célula)]) %>%
  mutate(Ocupação = case_when(Classificação == "BOM" ~ 1,
                              Classificação == "ALERTA" ~ 2,
                              Classificação == "CRITICO" ~ 3,
                              TRUE ~ 4)) %>% #DESCARTADO
  group_by(Célula) %>% slice(which.max(Ocupação)) %>%
  select(-"Ocupação")
baseocup <- baseocup %>% filter((Célula %in% baseocup_duplicated$Célula) == FALSE) 
baseocup <- bind_rows(baseocup, baseocup_duplicated)

base <- merge(baseocup,basekpi, by = "Célula", all.x = T)
teste_duplicata1 <- base$Célula[duplicated(base$Célula)==TRUE]

#tratmento cgi
source("Scripts/modulo_tratamento_nextim.R", encoding = "utf-8")

base <- merge(base, cgi_ok, by = "Célula", all.x = T)  
teste_duplicata2 <- base$Célula[duplicated(base$Célula)==TRUE]


# CRITÉRIO DE OCUPAÇÃO -----------------------------------------------------------------------------------------------------

#### SEPARANDO BASES POR CLASSIFICAÇÃO ####
cellbom <- base %>% filter(Classificação == "BOM" & !is.na(DIR_IRR) & !is.na(`Endereço ID`) & !is.na(Latitude) & !is.na(Longitude)) %>% 
  filter(Regional != "ND" & Estado != "ND" & ANF != "ND" & Município != "ND" & Banda != "ND" & Classificação != "ND" & `Classificação Populacional` != "ND")
cellalerta <- base %>% filter(Classificação == "ALERTA"  & !is.na(DIR_IRR) & !is.na(`Endereço ID`) & !is.na(Latitude) & !is.na(Longitude) & `Percentual da Célula` > 0.1) %>%
  filter(Regional != "ND" & Estado != "ND" & ANF != "ND" & Município != "ND" & Banda != "ND" & Classificação != "ND" & `Classificação Populacional` != "ND")
cellcritico <- base %>% filter(Classificação == "CRITICO"  & !is.na(DIR_IRR) & !is.na(`Endereço ID`) & !is.na(Latitude) & !is.na(Longitude) & `Percentual da Célula` > 0.1) %>%
  filter(Regional != "ND" & Estado != "ND" & ANF != "ND" & Município != "ND" & Banda != "ND" & Classificação != "ND" & `Classificação Populacional` != "ND")

#### OPORTUNIDADE DE BALANCEAMENTO BOM VS CRITICA ####
source("Scripts/modulo_critico-bom.R", encoding = "utf-8")

#### OPORTUNIDADE DE BALANCEAMENTO BOM VS CRITICA - VIZINHO ####
outlist <- c()
source("Scripts/modulo_critico-bom_vizinho.R", encoding = "utf-8")

#### OPORTUNIDADE DE BALANCEAMENTO BOM VS ALERTA ####
outlist2 <- vizcritico$OPORTUNIDADE
outlist <- c(outlist, outlist2)
source("Scripts/modulo_alerta-bom.R", encoding = "utf-8")

#### COMBINANDO OUTPUTS ####
oportunidades_ocp <- rbind(alertabom,criticabom, vizcritico)


# CRITÉRIO DE ASSESSMENT ---------------------------------------------------------------------------------------------------

#### APLICANDO OS CRITÉRIOS ASSESSMENT ####
base2hmm <- merge(raw_base2hmm, cgi[,c("Célula","BW_MHz")])
#base2hmm <- filter(!is.na(BW_MHz) & BW_MHz == "Indefinido")
base2hmm <- base2hmm %>%
  mutate(Indicador1 = case_when(PRB_UTIL_MEAN_DL > 0.7 ~ "PRB"), #Limiar PRB
         Indicador2 = case_when(PDCCH_CCE_UTIL > 0.7 ~ "CCE"), #Limiar CCE
         Indicador3 = case_when( #Limiar User por BW e Vendor
           ((Fornecedor == "NOKIA" & BW_MHz == 5 & USERS_RRC_CONN_MEAN_AVG > 50) | 
              (Fornecedor == "NOKIA" & BW_MHz == 10 & USERS_RRC_CONN_MEAN_AVG > 150) |
              (Fornecedor == "NOKIA" & BW_MHz == 15 & USERS_RRC_CONN_MEAN_AVG > 220) |
              (Fornecedor == "NOKIA" & BW_MHz == 20 & USERS_RRC_CONN_MEAN_AVG > 300) |
              (Fornecedor == "NOKIA" & BW_MHz == 25 & USERS_RRC_CONN_MEAN_AVG > 400) |
              (Fornecedor == "HUAWEI" & BW_MHz == 5 & USERS_RRC_CONN_MEAN_AVG > 60) |
              (Fornecedor == "HUAWEI" & BW_MHz == 10 & USERS_RRC_CONN_MEAN_AVG > 200) |
              (Fornecedor == "HUAWEI" & BW_MHz == 15 & USERS_RRC_CONN_MEAN_AVG > 250) |
              (Fornecedor == "HUAWEI" & BW_MHz == 20 & USERS_RRC_CONN_MEAN_AVG > 400) |
              (Fornecedor == "HUAWEI" & BW_MHz == 25 & USERS_RRC_CONN_MEAN_AVG > 600) |
              (Fornecedor == "ERICSSON" & BW_MHz == 5 & USERS_RRC_CONN_MEAN_AVG > 50) |
              (Fornecedor == "ERICSSON" & BW_MHz == 10 & USERS_RRC_CONN_MEAN_AVG > 135) |
              (Fornecedor == "ERICSSON" & BW_MHz == 15 & USERS_RRC_CONN_MEAN_AVG > 220) |
              (Fornecedor == "ERICSSON" & BW_MHz == 20 & USERS_RRC_CONN_MEAN_AVG > 375) |
              (Fornecedor == "ERICSSON" & BW_MHz == 25 & USERS_RRC_CONN_MEAN_AVG > 500)) ~ "USER")) %>%
  tidyr::unite("Indicador",c("Indicador1","Indicador2","Indicador3"), sep = "/", na.rm = TRUE, remove = TRUE) %>%
  mutate(Classificação = case_when(Indicador == "" ~ "BOM",
                                   TRUE ~ "CRITICO"))

foraassessment <- base2hmm %>% filter(Classificação == "CRITICO") %>%
  group_by(Célula, Indicador) %>%
  summarise(Quantidade = n()) %>%
  filter(Quantidade != 1) %>% #retirando indicadores que sairam apenas 1 vez
  mutate(indicadores = paste(Indicador,collapse = "/")) %>%  #juntar indicadores que sairam em dias diferentes
  select(-Indicador,-Quantidade) %>%
  unique.data.frame() %>% #retirar linhas de células duplicatas
  mutate(Indicador = paste(unique(unlist(str_split(indicadores,fixed('/')), use.names = FALSE)),collapse = "/")) #retirar duplicatas no caracter de indicadores
  
baseassess <- merge(base, foraassessment[c("Célula","Indicador")], by = "Célula", all.x = T) %>%
  mutate(Classificação = if_else(is.na(Indicador),"BOM","CRITICO"))
  

#### SEPARANDO BASES POR CLASSIFICAÇÃO ####
cellcritico <- baseassess %>% filter(Classificação == "CRITICO" & !is.na(DIR_IRR) & !is.na(`Endereço ID`) & !is.na(Latitude) & !is.na(Longitude) & `Percentual da Célula` > 0.1) %>%
  filter(Célula %in% oportunidades_ocp$Célula == FALSE) %>% #retirando celulas que ja encontraram oportunidade no critério de ocupação
  select(-Indicador) %>%
  filter(Regional != "ND" & Estado != "ND" & ANF != "ND" & Município != "ND" & Banda != "ND" & Classificação != "ND" & `Classificação Populacional` != "ND")

outlist2 <- alertabom$OPORTUNIDADE %>% str_split(fixed(',')) %>% unlist(use.names = FALSE) %>% str_sub(start = 2) %>% unique()
outlist <- c(outlist, outlist2)
altaocup <- base %>% filter(Classificação != "BOM")

cellbom <- baseassess %>% filter(Classificação == "BOM" & BW_MHz != "Indefinido" & !is.na(DIR_IRR) & !is.na(`Endereço ID`) & !is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Célula %in% altaocup$Célula == FALSE) %>% #retirando celulas com alta ocupação
  filter(Célula %in% outlist == FALSE) %>% #retirando celulas que ja foram apontadas como oportunidade anteriormente
  select(-Indicador) %>%
  filter(Regional != "ND" & Estado != "ND" & ANF != "ND" & Município != "ND" & Banda != "ND" & Classificação != "ND" & `Classificação Populacional` != "ND")


#### OPORTUNIDADE DE BALANCEAMENTO Bom VS Critico ####
source("Scripts/modulo_critico-bom.R", encoding = "utf-8")

#### OPORTUNIDADE DE BALANCEAMENTO Bom VS Critico - VIZINHO ####
source("Scripts/modulo_critico-bom_vizinho.R", encoding = "utf-8")

#### COMBINANDO OUTPUTS ####
oportunidades_assess <- rbind(criticabom, vizcritico)


# MODELANDO OUTPUT ----------------------------------------------------------------------------------------------------------------------------

#### COMBINANDO TODAS AS OPORTUNIDADES ####
oportunidades <- bind_rows(oportunidades_assess,oportunidades_ocp) %>% select(-Classificação) %>% mutate(BALANCEAMENTO = 'Com Oportunidade')

#### COMBINANDO INFORMAÇÕES DE TODAS AS CÉLULAS ####
totalcell <- merge(base %>% rename(`Status Ocupação` = Classificação),
                   baseassess[c("Célula","Classificação","Indicador")] %>% rename(`Status Assessment` = Classificação), by = "Célula")
totalcell <- merge(totalcell, oportunidades[c("Célula", "OPORTUNIDADE", "TIPO", "DELTA_Tput", "BALANCEAMENTO")], by = "Célula", all.x = T)
totalcell <- totalcell %>% mutate(BALANCEAMENTO = if_else(is.na(BALANCEAMENTO), "Sem Oportunidade", BALANCEAMENTO),
                                  CRITÉRIO = case_when(((`Status Ocupação` != "BOM" & `Status Ocupação` != "DESCARTADO")  & `Status Assessment` == "BOM") ~ "Ocupação",
                                                       (`Status Ocupação` == "BOM" & `Status Assessment` != "BOM") ~ "Assessment",
                                                       ((`Status Ocupação` != "BOM" & `Status Ocupação` != "DESCARTADO") & `Status Assessment` != "BOM") ~ "Ambos",
                                                       TRUE ~ 'OK')) %>%
  rename(Semana = `Semana Ocupação`) %>%
  filter(CRITÉRIO != "OK")

#### FATOR DE CRITICIDADE DA CIDADE ####
source("Scripts/modulo_class_cidade.R", encoding = "utf-8")

#### PRIORIZAÇÃO ####
source("Scripts/modulo_priorizacao.R", encoding = "utf-8")


# ESCREVENDO OUTPUTS ---------------------------------------------------------------------------------------------------------------------------
totalcell <- totalcell %>% select(`Semana do Ano`, Semana, Regional, Estado, ANF, Município, `Classificação Populacional`, `Status Município`, `Endereço ID`, Célula, Fornecedor, Banda, BW_MHz, `Flag Mocn`, `Status Ocupação`, `Status Assessment`, Indicador, CRITÉRIO, DIR_IRR,Latitude,Longitude,OPORTUNIDADE,TIPO, DELTA_Tput, Prio, BALANCEAMENTO)
source("Scripts/modulo_escrever_output.R", encoding = "utf-8")


# MODULO EXTRA - OPORTUNIDADES MMIMO -----------------------------------------------------------------------------------------------------------
Py_semana <- r_to_py(semana)
py_run_file("Scripts/modulo_oportunidades_mmimo.py")

# MODULO TRATAMENTO -----------------------------------------------------------------------------------------------------------
source("Scripts/modulo_tratamento.R", encoding = "utf-8")

# neg_ocup <- oportunidades_ocp %>% filter(DELTA_Tput < 0)
# neg_assess <- oportunidades_assess %>% filter(DELTA_Tput < 0)