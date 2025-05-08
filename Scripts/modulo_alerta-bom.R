########################################################
# MODULO: OPORTUNIDADE DE BALANCEAMENTO BOM VS ALERTA #
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
  mutate(Limiar = (case_when((BW_MHz == 5 & USERS_RRC_CONN_MEAN_AVG <= 0.7*50) ~ "ok",
                             (BW_MHz == 10 & USERS_RRC_CONN_MEAN_AVG <= 0.95*150) ~ "ok",
                             (BW_MHz == 15 & USERS_RRC_CONN_MEAN_AVG <= 0.95*220) ~ "ok",
                             (BW_MHz == 20 & USERS_RRC_CONN_MEAN_AVG <= 0.95*300) ~ "ok",
                             (BW_MHz == 25 & USERS_RRC_CONN_MEAN_AVG <= 0.95*400) ~ "ok"))
  ) %>% filter(Limiar == "ok") %>% select(-Limiar)

huawei <- bom_ok %>% filter(Fornecedor == "HUAWEI") %>%
  mutate(Limiar = (case_when((BW_MHz == 5 & USERS_RRC_CONN_MEAN_AVG <= 0.7*60) ~ "ok",
                             (BW_MHz == 10 & USERS_RRC_CONN_MEAN_AVG <= 0.95*200) ~ "ok",
                             (BW_MHz == 15 & USERS_RRC_CONN_MEAN_AVG <= 0.95*250) ~ "ok",
                             (BW_MHz == 20 & USERS_RRC_CONN_MEAN_AVG <= 0.95*400) ~ "ok",
                             (BW_MHz == 25 & USERS_RRC_CONN_MEAN_AVG <= 0.95*600) ~ "ok"))
  ) %>% filter(Limiar == "ok") %>% select(-Limiar)

ericsson <- bom_ok %>% filter(Fornecedor == "ERICSSON") %>%
  mutate(Limiar = (case_when((BW_MHz == 5 & USERS_RRC_CONN_MEAN_AVG <= 0.7*50) ~ "ok",
                             (BW_MHz == 10 & USERS_RRC_CONN_MEAN_AVG <= 0.95*135) ~ "ok",
                             (BW_MHz == 15 & USERS_RRC_CONN_MEAN_AVG <= 0.95*220) ~ "ok",
                             (BW_MHz == 20 & USERS_RRC_CONN_MEAN_AVG <= 0.95*375) ~ "ok",
                             (BW_MHz == 25 & USERS_RRC_CONN_MEAN_AVG <= 0.95*500) ~ "ok"))
  ) %>% filter(Limiar == "ok") %>% select(-Limiar)
bom_ok <- rbind(nokia, huawei, ericsson)
rm(nokia, huawei, ericsson)
bom_ok <- bom_ok %>% filter(Célula %in% outlist == FALSE)


#### CHECK DE OPORTUNIDADE BOM-CRITICO ####
#Check de end ID critico com correspondente bom
listbom_ok <- bom_ok$`Endereço ID`
listbom_ok <- unique(listbom_ok)
oportalerta <- cellalerta %>% filter(`Endereço ID` %in% listbom_ok)

listazimute <- unique(oportalerta$DIR_IRR)

alertabom <- data.frame()

# Count NA values in each column
na_counts <- sapply(bom_ok, function(col) sum(is.na(col)))
print(na_counts)

# TIAGO em 27/11
#Realizando check entre oportunidade e alerta por azimute
for (azimute in 1:length(listazimute)) {
  setor <- oportalerta %>% filter(DIR_IRR == listazimute[azimute])
  listsetor <- setor$`Endereço ID`
  listsetor <- unique(listsetor)
  for (i in 1:length(listsetor)){
    endID = listsetor[i]
    listoport <- bom_ok %>% filter(`Endereço ID` == endID & DIR_IRR == listazimute[azimute])
    listoport <- listoport$Célula
    bom <- c()
    MaiorDelta <- c()
    if (length(listoport) != 0 ) {
      DELTA <- oportalerta %>% filter(`Endereço ID` == endID & DIR_IRR == listazimute[azimute]) %>% slice(which.min(THROU_USER_PDCP_DL))
      for (j in 1:length(listoport)) {
        candidato = listoport[j]
        teste <- bom_ok %>% filter(Célula == candidato)
        teste <- rbind(DELTA, teste)
        teste <- teste %>% select(Classificação, THROU_USER_PDCP_DL) %>%
          tidyr::spread(key = Classificação, value = THROU_USER_PDCP_DL) %>%
          mutate(perc = (1-(ALERTA/BOM)),
                 delta = BOM - ALERTA)
          tryCatch({
          if (teste$perc > 0.3) {
            bom[length(bom) + 1] <- candidato
            MaiorDelta[length(MaiorDelta) + 1] <- teste$delta
          }
        }, error = function(e) {
          cat("An error occurred:", e$message, "\n")
        })
      }
      oport <- oportalerta %>% filter(`Endereço ID` == endID & DIR_IRR == listazimute[azimute]) %>% mutate(OPORTUNIDADE = paste("",bom, collapse = ","), DELTA_Tput = max(MaiorDelta))
      alertabom <- rbind(alertabom,oport)
    }
  }
}

# TIAGO
# Initialize alertabom if not already done
# 
# for (azimute in 1:length(listazimute)) {
#   setor <- oportalerta %>% filter(DIR_IRR == listazimute[azimute])
#   listsetor <- setor$`Endereço ID`
#   listsetor <- unique(listsetor)
#   
#   for (i in 1:length(listsetor)) {
#     endID <- listsetor[i]
#     listoport <- bom_ok %>% filter(`Endereço ID` == endID & DIR_IRR == listazimute[azimute])
#     listoport <- listoport$Célula
#     bom <- c()
#     MaiorDelta <- c()
#     
#     if (length(listoport) != 0) {
#       DELTA <- oportalerta %>% 
#         filter(`Endereço ID` == endID & DIR_IRR == listazimute[azimute]) %>% 
#         slice(which.min(THROU_USER_PDCP_DL))
#       
#       for (j in 1:length(listoport)) {
#         candidato <- listoport[j]
#         teste <- bom_ok %>% filter(Célula == candidato)
#         teste <- rbind(DELTA, teste)
#         teste <- teste %>%
#           tidyr::pivot_wider(names_from = Classificação, values_from = THROU_USER_PDCP_DL) %>%
#           mutate(perc = (1 - (ALERTA / BOM)),
#                  delta = BOM - ALERTA)
#         
#         # Ensure logical evaluation handles multiple rows
#         if (all(!is.na(teste$perc)) && any(teste$perc > 0.3)) {
#           bom <- c(bom, candidato)
#           MaiorDelta <- c(MaiorDelta, teste$delta)
#         }
#       }
#       
#       # Ensure DELTA_Tput is safely computed
#       DELTA_Tput <- ifelse(length(MaiorDelta) > 0, max(MaiorDelta), NA)
#       
#       oport <- oportalerta %>% 
#         filter(`Endereço ID` == endID & DIR_IRR == listazimute[azimute]) %>% 
#         mutate(OPORTUNIDADE = paste(bom, collapse = ","),
#                DELTA_Tput = DELTA_Tput)
#       
#       alertabom <- rbind(alertabom, oport)
#     }
#   }
# }





#### DF DE OPORTUNIDADES DE BALANCEAMENTO ALERTA-BOM ####
rm(teste, DELTA, oport,setor, listsetor,listoport, listbom_ok, oportalerta, endID, i, azimute, listazimute, bom, candidato, j, MaiorDelta)
alertabom <- alertabom %>% filter(OPORTUNIDADE != " ") %>% mutate (TIPO = "Próprio")

# Excluir casos onde o Delta de Tput é negativo
alertabom <- alertabom %>% filter(DELTA_Tput > 0)
