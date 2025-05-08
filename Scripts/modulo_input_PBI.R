##########################################
#### MODULO: EXPORT DE INPUT PARA PBI ####
##########################################

#### CONSTRUINDO BASE SEMANAL ####
baseweek <- oportunidades %>% mutate(BALANCEAMENTO = 'Com Oportunidade') #guardar informação da variável
oportunidades <- bind_rows(cellalerta, cellcritico) #para utilizar mesmo nome da variável inserida no módulo

source("Scripts/modulo_class_cidade.R", encoding = "utf-8") #classificar cidades

oportunidades <- oportunidades %>% mutate(TIPO = NA,
                                          Prio = NA,
                                          OPORTUNIDADE = NA,
                                          DELTA_Tput = NA,
                                          BALANCEAMENTO = 'Sem Oportunidade') %>%
  select(`Semana do Ano`, Regional, Estado, ANF, Município, `Status Município`, `Classificação Populacional`, `Endereço ID`, Célula, Fornecedor, Banda, BW_MHz, Classificação, OPORTUNIDADE, TIPO, Prio, BALANCEAMENTO, DELTA_Tput)
oportunidades <- oportunidades %>% filter(Célula %in% baseweek$Célula == FALSE)
totalcell <- bind_rows(oportunidades,baseweek)

spazio <- spazio %>% select("Endereço ID", "Latitude", "Longitude")
spazio <- unique(spazio)
totalcell <- merge(totalcell, spazio[,c("Endereço ID", "Latitude", "Longitude")], by.x = "Endereço ID", by.y = "Endereço ID")
totalcell <- totalcell %>% mutate(Latitude = gsub('\\s+', '', Latitude),
                                  Latitude = gsub(',', '.', Latitude),
                                  Latitude = as.numeric(Latitude),
                                  Longitude = gsub('\\s+', '', Longitude),
                                  Longitude = gsub(',', '.', Longitude),
                                  Longitude = as.numeric(Longitude))

# maximo <- base %>% select("Endereço ID","Setor", "AVG_THROU_PDCP_USER_DL") %>% group_by(`Endereço ID`, Setor) %>% slice(which.max(AVG_THROU_PDCP_USER_DL)) %>% rename("TputMax" = "AVG_THROU_PDCP_USER_DL")
# minimo <- base %>% select("Endereço ID","Setor", "AVG_THROU_PDCP_USER_DL") %>% group_by(`Endereço ID`, Setor) %>% slice(which.min(AVG_THROU_PDCP_USER_DL)) %>% rename("TputMin" = "AVG_THROU_PDCP_USER_DL")
# delta <- merge(maximo,minimo) %>% mutate(DeltaTput = TputMax - TputMin)
# delta <- delta %>% select(-"Setor") %>% group_by(`Endereço ID`) %>% slice(which.max(DeltaTput))
#totalcell <- merge(totalcell, delta[,c("Endereço ID", "DeltaTput")])

ano <- str_sub(lubridate::epiyear(lubridate::today()),3,4)
totalcell <- totalcell %>%
  mutate(Semana = paste0(ano,"W",semana))


#### ESCREVENDO BASE SEMANAL/EVOLUÇÃO NO EXCEL ####
evolucao <- readxl::read_excel("Outputs/PBI_Balanceamento_TBR.xlsx", sheet = 2)
check_data <- evolucao$Semana %>% unique()

if (paste0(ano,"W",semana) %in% check_data == FALSE){
  #armazenamento semanal
  writexl::write_xlsx(totalcell,paste0("Outputs/Historico semanal/PBI_Balanceamento_TBR_W", semana, ".xlsx"))
  
  #atualização base PBI
  evolucao <- bind_rows(evolucao, totalcell)
  writexl::write_xlsx(x = list("Semana" = totalcell,
                               "Evolução" = evolucao),
                      "Outputs/PBI_Balanceamento_TBR.xlsx")
  
  print("PBI_Balanceamento_TBR está atualizada!")
  
} else {
  print("PBI_Balanceamento_TBR não foi atualizada, pois ja existe dados desta week escritos!") 
}
