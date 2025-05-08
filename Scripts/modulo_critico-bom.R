########################################################
# MODULO: OPORTUNIDADE DE BALANCEAMENTO BOM VS CRITICA #
########################################################

#### CÉLULAS BOAS ELEGÍVEIS ####
#Limiar PRB
bom_ok <- cellbom %>% filter(PRB_UTIL_MEAN_DL <= 0.55 & !is.na(BW_MHz))
#Limiar CQI
bom_ok <- bom_ok %>% mutate(Limiar = case_when(((Banda == 700 | Banda == 850) & CQI_MEAN >= 8) ~ "ok",
                                               ((Banda != 700 & Banda != 850) & CQI_MEAN >= 9) ~ "ok")
) %>% filter(Limiar == "ok") %>% select(-Limiar)
#Limiar User
nokia <- bom_ok %>% filter(Fornecedor == "NOKIA") %>%
  mutate(Limiar = (case_when((BW_MHz == 5 & USERS_RRC_CONN_MEAN_AVG <= 0.6*50) ~ "ok",
                             (BW_MHz == 10 & USERS_RRC_CONN_MEAN_AVG <= 0.85*150) ~ "ok",
                             (BW_MHz == 15 & USERS_RRC_CONN_MEAN_AVG <= 0.85*220) ~ "ok",
                             (BW_MHz == 20 & USERS_RRC_CONN_MEAN_AVG <= 0.85*300) ~ "ok",
                             (BW_MHz == 25 & USERS_RRC_CONN_MEAN_AVG <= 0.85*400) ~ "ok"))
  ) %>% filter(Limiar == "ok") %>% select(-Limiar)

huawei <- bom_ok %>% filter(Fornecedor == "HUAWEI") %>%
  mutate(Limiar = (case_when((BW_MHz == 5 & USERS_RRC_CONN_MEAN_AVG <= 0.6*60) ~ "ok",
                             (BW_MHz == 10 & USERS_RRC_CONN_MEAN_AVG <= 0.85*200) ~ "ok",
                             (BW_MHz == 15 & USERS_RRC_CONN_MEAN_AVG <= 0.85*250) ~ "ok",
                             (BW_MHz == 20 & USERS_RRC_CONN_MEAN_AVG <= 0.85*400) ~ "ok",
                             (BW_MHz == 25 & USERS_RRC_CONN_MEAN_AVG <= 0.85*600) ~ "ok"))
  ) %>% filter(Limiar == "ok") %>% select(-Limiar)

ericsson <- bom_ok %>% filter(Fornecedor == "ERICSSON") %>%
  mutate(Limiar = (case_when((BW_MHz == 5 & USERS_RRC_CONN_MEAN_AVG <= 0.6*50) ~ "ok",
                             (BW_MHz == 10 & USERS_RRC_CONN_MEAN_AVG <= 0.85*135) ~ "ok",
                             (BW_MHz == 15 & USERS_RRC_CONN_MEAN_AVG <= 0.85*220) ~ "ok",
                             (BW_MHz == 20 & USERS_RRC_CONN_MEAN_AVG <= 0.85*375) ~ "ok",
                             (BW_MHz == 25 & USERS_RRC_CONN_MEAN_AVG <= 0.85*500) ~ "ok"))
  ) %>% filter(Limiar == "ok") %>% select(-Limiar)
bom_ok <- rbind(nokia, huawei, ericsson)
rm(nokia, huawei, ericsson)

#### CHECK DE OPORTUNIDADE BOM-CRITICO ####
#Check de end ID critico com correspondente bom
listbom_ok <- bom_ok$`Endereço ID`
listbom_ok <- unique(listbom_ok)

oportcritica <- cellcritico %>% filter(`Endereço ID` %in% listbom_ok)
oportcritica <- subset(oportcritica, !is.na(oportcritica$THROU_USER_PDCP_DL)) # Tiago

listazimute <- unique(oportcritica$DIR_IRR)

criticabom <- data.frame()

unique(oportcritica$Classificação)

#Realizando check entre oportunidade e critica por azimute
for (azimute in 1:length(listazimute)) {
  setor <- oportcritica %>% filter(DIR_IRR == listazimute[azimute])
  listsetor <- setor$`Endereço ID`
  listsetor <- unique(listsetor)
  for (i in 1:length(listsetor)){
    endID = listsetor[i]
    listoport <- bom_ok %>% filter(`Endereço ID` == endID & DIR_IRR == listazimute[azimute])
    listoport <- listoport$Célula
    bom <- c()
    MaiorDelta <- c()
    if (length(listoport) != 0 ) {
      DELTA <- oportcritica %>% filter(`Endereço ID` == endID & DIR_IRR == listazimute[azimute]) %>% slice(which.min(THROU_USER_PDCP_DL))
  
      for (j in 1:length(listoport)) {
        candidato = listoport[j]
        teste <- bom_ok %>% filter(Célula == candidato)
        teste <- rbind(DELTA, teste)
        teste <- teste %>% select(Classificação, THROU_USER_PDCP_DL) %>%
          tidyr::spread(key = Classificação, value = THROU_USER_PDCP_DL) %>%
          mutate(delta = BOM - CRITICO)
        bom[length(bom)+1]=candidato
        MaiorDelta[length(MaiorDelta)+1] <- teste$delta
      }
      
      
      oport <- oportcritica %>% filter(`Endereço ID` == endID & DIR_IRR == listazimute[azimute]) %>% mutate(OPORTUNIDADE = paste("",bom, collapse = ","), DELTA_Tput = max(MaiorDelta))
      criticabom <- rbind(criticabom,oport)
    }
  }
}

#### DF DE OPORTUNIDADES DE BALANCEAMENTO CRITICO-BOM ####
rm(teste, DELTA, oport,setor, listsetor,listoport, listbom_ok, oportcritica, endID, i, azimute, listazimute, bom, candidato, j, MaiorDelta)
criticabom <- criticabom %>% filter(OPORTUNIDADE != " ") %>% mutate (TIPO = "Próprio") 

# Excluir casos onde o Delta de Tput é negativo
criticabom <- criticabom %>% filter(DELTA_Tput > 0)

