rm(list=ls())
library(dplyr)
library(XLConnect)
library(stringr)

#### PREPARANDO AMBIENTE PARA CÓDIGO PYTHON  ####
library(reticulate)
# Alterar instância do Python
#use_python("C:/Users/matheus/AppData/Local/Programs/Python/Python38/python.exe", required = T) # Matheus
use_python("C:/Program Files/Python37/python.exe", required = T) # Thaina
# Verificar instância do Python
py_config()
#Requisitos: pandas numpy xlrd geographiclib openpyxl datetime
# Para instalar através do R:
# py_install("package")
# Possível instalar bibliotecas através do CMD, caso o Python esteja adicionado ao PATH do Windows. Executar:
# pip install pandas numpy xlrd geographiclib openpyxl datetime
#------------------------------------------------------------------------------------------------------------

for (loop in 5:2) {
  ##### IMPORTANDO BASES ####
  semana = lubridate::epiweek(lubridate::today())-loop #WeekAtual-1
  
  #basekpi <- readxl::read_excel(paste0("Inputs/W", semana, "/4021-LTE- 2ºHMM Volume Dados - Semanal.xlsx"))
  base2hmm_raw <- readxl::read_excel(paste0("Inputs/W", semana, "/Relatório Personalizado LTE - HMM_TBR.xlsx"))
  baseocup_raw = readxl::read_excel(paste0("Inputs/W", semana, "/New - ALL_Relatório de Ocupação de Células 4G – Semanal.xlsx"))
  #baseocup_raw = readxl::read_excel(paste0("Inputs/W", semana, "/Relatório de Ocupação de Células 4G – Semanal.xlsx"))
  #basevol = readxl::read_excel(paste0("Inputs/W", semana, "/Relatório Personalizado LTE - Semanal - VOL DADOS.xlsx"))
  basebw = readxl::read_excel(paste0("Inputs/W", semana, "/Relatório Personalizado LTE - Diário - PRB AVAIL.xlsx"))
  spazio = readxl::read_excel("Inputs/Spazio.xlsx")
  spazio_raw <- spazio
  nextim <- read.csv("Inputs/NEXTIM_4G_DB16.csv", header = FALSE, sep = ";", row.names = NULL, stringsAsFactors = F)
  names(nextim) <- as.matrix(nextim[1,])
  LATLONG <- nextim[-1,c("CELLA","LATITUD","LONG")] %>% unique.data.frame()
  nextim <- nextim[-1,c("CELLA","DIR_IRR")] %>% unique.data.frame()
  
  #### UNINDO BASES ####
  # Ultilizando max volume da 2HMM/semana para basekpi
  basekpi <- base2hmm_raw %>% group_by(Célula) %>%
    slice(which.max(`VOLUME_DADOS_DL_ALLOP 4G`)) %>%
    select(-Dia,-`Dia da Semana`,-`Dia HMM`,-`Número HMM`,-`Regra HMM`,-`Hora HMM`,-Metrics,-Fornecedor,-Regional,-`Station ID`)
  basekpi[is.na(basekpi)] <- 0
  
  # Tratamento BaseOcupação
  baseocup <- baseocup_raw %>%
    filter(Regional != "ND" & Estado != "ND" & ANF != "ND" & Município != "ND" & Banda != "ND" & Classificação != "ND" & `Classificação Populacional` != "ND") %>%
    select(`Semana Ocupação`, Regional, Estado, ANF, Município, Fornecedor, Banda, Célula, Classificação, `Classificação Populacional`,`Re-classificação LT`,`Flag Mocn`,Vol_Total_DlUl_Allop_LT,Vol_Total_DlUl_Tim_LT,`Percentual da Cidade`,`Percentual da Célula`) %>% unique()
  baseocup_duplicated <- baseocup %>% #retirando duplicatas com classificação diferente (escolhe-se a pior)
    filter(Célula %in% baseocup$Célula[duplicated(baseocup$Célula)]) %>%
    mutate(Ocupação = case_when(Classificação == "Bom" ~ 1,
                                Classificação == "Alerta" ~ 2,
                                TRUE ~ 3)) %>%
    group_by(Célula) %>% slice(which.max(Ocupação)) %>%
    select(-"Ocupação")
  baseocup <- baseocup %>% filter(Célula %in% baseocup$Célula[duplicated(baseocup$Célula)] == FALSE)
  baseocup <- bind_rows(baseocup, baseocup_duplicated)
  base <- merge(baseocup,basekpi)
  
  
  # Tratamento BaseBw(PRB_AVAIL) - Determinar largura de banda
  basebw <- basebw %>% 
    select(Célula, PRB_AVAIL) %>%
    mutate(PRB_AVAIL = ifelse(PRB_AVAIL > 150, 0, PRB_AVAIL)) %>%
    group_by(Célula) %>% slice(which.max(PRB_AVAIL))
  basebw <- basebw %>%
    mutate(BW_MHz = case_when(PRB_AVAIL == 0 ~ "Indefinido",
                              PRB_AVAIL > 0 & PRB_AVAIL <= 37.5 ~ "5",
                              PRB_AVAIL > 37.5 & PRB_AVAIL <= 62.5 ~ "10",
                              PRB_AVAIL > 62.5 & PRB_AVAIL <= 84.5 ~ "15",
                              PRB_AVAIL > 84.5 & PRB_AVAIL <= 112.5 ~ "20",
                              PRB_AVAIL > 112.5 ~ "25"))
  base <- merge(base, basebw[,c("Célula", "BW_MHz")], all.x = T)
  
  # Tratamento Spazio - inserção do END_ID
  spazio <- spazio_raw %>% select("Site ID", "Endereço ID", "Status") %>%
    filter(`Site ID` %in% base$`BTS/NodeB/ENodeB`) %>%
    unique()
  spazio_duplicated <- spazio %>% filter(`Site ID` %in% spazio$`Site ID`[duplicated(spazio$`Site ID`)]) %>%
    left_join(spazio_raw %>% select (Regional, Estado, Município, `Endereço ID`, `Site ID`), by = c("Site ID", "Endereço ID")) %>%
    select(Regional, Estado, Município, `Endereço ID`, `Site ID`, Status)
  spazio <- spazio %>% filter(`Site ID` %in% spazio_duplicated$`Site ID` == FALSE)
  base <- merge(base, spazio[,c("Site ID", "Endereço ID")], by.x = "BTS/NodeB/ENodeB", by.y = "Site ID", all.x = T) %>%
    rename("Site" = "BTS/NodeB/ENodeB")
  
  # Tratamento e inserção de LATLONG
  LATLONG <- LATLONG %>%
    mutate(Latitude = gsub(" ", "'", LATITUD),
           Longitude = gsub(" ", "'", LONG)) %>%
    select(-LATITUD,-LONG)
  ConvertLATLONG <- OSMscale::degree(LATLONG$Latitude, LATLONG$Longitude, digits = 15) #conversao de coordenada para decimal
  LATLONG <- bind_cols(LATLONG,ConvertLATLONG) %>%
    select("CELLA", "lat", "long") %>%
    rename(Célula = CELLA,
           Latitude = lat,
           Longitude = long) %>%
    mutate(Latitude = gsub('\\s+', '', Latitude),
           Latitude = gsub(',', '.', Latitude),
           Latitude = as.numeric(Latitude),
           Longitude = gsub('\\s+', '', Longitude),
           Longitude = gsub(',', '.', Longitude),
           Longitude = as.numeric(Longitude))
  rm(ConvertLATLONG)
  base <- merge(base, LATLONG, by = "Célula", all.x = T)  
  
  # Tratamento NEXTIM
  source("Scripts/modulo_tratamento_nextim.R", encoding = "utf-8")
  base <- merge(base, nextim, by.x = "Célula", by.y = "CELLA", all.x = T) %>% mutate(DIR_IRR = as.numeric(DIR_IRR))
  teste_duplicata <- base$Célula[duplicated(base$Célula)==TRUE]
  
  
  # CRITÉRIO DE OCUPAÇÃO -----------------------------------------------------------------------------------------------------
  
  #### SEPARANDO BASES POR CLASSIFICAÇÃO ####
  cellbom <- base %>% filter(Classificação == "Bom" & !is.na(DIR_IRR) & !is.na(`Endereço ID`) & !is.na(Latitude) & !is.na(Longitude))
  cellalerta <- base %>% filter(Classificação == "Alerta"  & !is.na(DIR_IRR) & !is.na(`Endereço ID`) & !is.na(Latitude) & !is.na(Longitude) & `Percentual da Célula` > 0.1) 
  cellcritico <- base %>% filter(Classificação == "Crítico"  & !is.na(DIR_IRR) & !is.na(`Endereço ID`) & !is.na(Latitude) & !is.na(Longitude) & `Percentual da Célula` > 0.1)
  
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
  base2hmm <- merge(base2hmm_raw, basebw[,c("Célula","BW_MHz")])
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
    mutate(Classificação = case_when(Indicador == "" ~ "Bom",
                                     TRUE ~ "Crítico"))
  
  foraassessment <- base2hmm %>% filter(Classificação == "Crítico") %>%
    group_by(Célula, Indicador) %>%
    summarise(Quantidade = n()) %>%
    filter(Quantidade != 1) %>% #retirando indicadores que sairam apenas 1 vez
    mutate(indicadores = paste(Indicador,collapse = "/")) %>%  #juntar indicadores que sairam em dias diferentes
    select(-Indicador,-Quantidade) %>%
    unique.data.frame() %>% #retirar linhas de células duplicatas
    mutate(Indicador = paste(unique(unlist(str_split(indicadores,fixed('/')), use.names = FALSE)),collapse = "/")) #retirar duplicatas no caracter de indicadores
  
  baseassess <- merge(base, foraassessment[c("Célula","Indicador")], by = "Célula", all.x = T) %>%
    mutate(Classificação = if_else(is.na(Indicador),"Bom","Crítico"))
  
  
  #### SEPARANDO BASES POR CLASSIFICAÇÃO ####
  cellcritico <- baseassess %>% filter(Classificação == "Crítico" & !is.na(DIR_IRR) & !is.na(`Endereço ID`) & !is.na(Latitude) & !is.na(Longitude) & `Percentual da Célula` > 0.1) %>%
    filter(Célula %in% oportunidades_ocp$Célula == FALSE) %>% #retirando celulas que ja encontraram oportunidade no critério de ocupação
    select(-Indicador)
  
  outlist2 <- alertabom$OPORTUNIDADE %>% str_split(fixed(',')) %>% unlist(use.names = FALSE) %>% str_sub(start = 2) %>% unique()
  outlist <- c(outlist, outlist2)
  altaocup <- base %>% filter(Classificação != "Bom")
  
  cellbom <- baseassess %>% filter(Classificação == "Bom" & BW_MHz != "Indefinido" & !is.na(DIR_IRR) & !is.na(`Endereço ID`) & !is.na(Latitude) & !is.na(Longitude)) %>%
    filter(Célula %in% altaocup$Célula == FALSE) %>% #retirando celulas com alta ocupação
    filter(Célula %in% outlist == FALSE) %>% #retirando celulas que ja foram apontadas como oportunidade anteriormente
    select(-Indicador)
  
  
  #### OPORTUNIDADE DE BALANCEAMENTO Bom VS Critico ####
  source("Scripts/modulo_critico-bom.R", encoding = "utf-8")
  
  #### OPORTUNIDADE DE BALANCEAMENTO Bom VS Critico - VIZINHO ####
  source("Scripts/modulo_critico-bom_vizinho.R", encoding = "utf-8")
  
  #### COMBINANDO OUTPUTS ####
  oportunidades_assess <- rbind(criticabom, vizcritico)
  
  
  # MODELANDO OUTPUT -------------------------------------------------------------------------------------------------------------------
  
  #### COMBINANDO TODAS AS OPORTUNIDADES ####
  oportunidades <- bind_rows(oportunidades_assess,oportunidades_ocp) %>% select(-Classificação) %>% mutate(BALANCEAMENTO = 'Com Oportunidade')
  
  #### COMBINANDO INFORMAÇÕES DE TODAS AS CÉLULAS ####
  totalcell <- merge(base %>% rename(`Status Ocupação` = Classificação),
                     baseassess[c("Célula","Classificação","Indicador")] %>% rename(`Status Assessment` = Classificação), by = "Célula")
  totalcell <- merge(totalcell, oportunidades[c("Célula", "OPORTUNIDADE", "TIPO", "DELTA_Tput", "BALANCEAMENTO")], by = "Célula", all.x = T)
  totalcell <- totalcell %>% mutate(BALANCEAMENTO = if_else(is.na(BALANCEAMENTO), "Sem Oportunidade", BALANCEAMENTO),
                                    CRITÉRIO = case_when((`Status Ocupação` != "Bom" & `Status Assessment` == "Bom") ~ "Ocupação",
                                                         (`Status Ocupação` == "Bom" & `Status Assessment` != "Bom") ~ "Assessment",
                                                         (`Status Ocupação` != "Bom" & `Status Assessment` != "Bom") ~ "Ambos",
                                                         TRUE ~ 'OK')) %>%
    rename(Semana = `Semana Ocupação`) %>%
    filter(CRITÉRIO != "OK")
  
  #### FATOR DE CRITICIDADE DA CIDADE ####
  source("Scripts/modulo_class_cidade.R", encoding = "utf-8")
  
  #### PRIORIZAÇÃO ####
  source("Scripts/modulo_priorizacao.R", encoding = "utf-8")
  
  
  # ESCREVENDO INPUT DO POWERBI EM EXCEL ---------------------------------------------------------------------------------------
  totalcell <- totalcell %>% select(`Semana do Ano`, Semana, Regional, Estado, ANF, Município, `Classificação Populacional`, `Status Município`, `Endereço ID`, Célula, Fornecedor, Banda, BW_MHz, `Flag Mocn`, `Status Ocupação`, `Status Assessment`, Indicador, CRITÉRIO, DIR_IRR,Latitude,Longitude,OPORTUNIDADE,TIPO, DELTA_Tput, Prio, BALANCEAMENTO)
  
  #### PARA O NOVO ESCOPO - assessment + ocupação ####
  evolucao <- readxl::read_excel("Outputs/Novo_Escopo/PBI_Balanceamento_TBR.xlsx", sheet = 2)
  if (unique(totalcell$Semana) %in% unique(evolucao$Semana) == FALSE){
    #armazenamento semanal
    writexl::write_xlsx(totalcell,paste0("Outputs/Novo_Escopo/Historico semanal/PBI_Balanceamento_TBR_W", semana, ".xlsx"))
    #atualização base PBI
    evolucao <- bind_rows(evolucao, totalcell)
    writexl::write_xlsx(x = list("Semana" = totalcell,
                                 "Evolução" = evolucao),
                        "Outputs/Novo_Escopo/PBI_Balanceamento_TBR.xlsx")
    
    print(paste0("Novo Escopo: PBI_Balanceamento_TBR está atualizada com a W", semana, "!"))
  } else {
    print("Novo Escopo: PBI_Balanceamento_TBR não foi atualizada, pois ja existe dados desta week escritos!") 
  }
  
}
