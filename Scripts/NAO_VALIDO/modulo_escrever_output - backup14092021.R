#######################################
# MODULO: ESCREVER INPUT PBI EM EXCEL #
#######################################

#### PARA O NOVO ESCOPO - assessment + ocupação ####
writexl::write_xlsx(totalcell,paste0("Outputs/Novo_Escopo/Historico semanal/PBI_Balanceamento_TBR_", semana, ".xlsx"))

# evolucao <- readxl::read_excel("Outputs/Novo_Escopo/PBI_Balanceamento_TBR.xlsx", sheet = 2)
# if (unique(totalcell$Semana) %in% unique(evolucao$Semana) == FALSE){
#   #armazenamento semanal
#   writexl::write_xlsx(totalcell,paste0("Outputs/Novo_Escopo/Historico semanal/PBI_Balanceamento_TBR_", semana, ".xlsx"))
#   #atualização base PBI
#   evolucao <- bind_rows(evolucao, totalcell)
#   DIASDOHISTORICO <- unique(evolucao$Semana) %>% sort()
#   if (length(DIASDOHISTORICO) > 26){
#     GAP = length(DIASDOHISTORICO) - 26
#     evolucao <- evolucao %>% filter(Semana %in% DIASDOHISTORICO[-(1:GAP)])
#   }
#   writexl::write_xlsx(x = list("Semana" = totalcell,
#                                "Evolução" = evolucao),
#                       "Outputs/Novo_Escopo/PBI_Balanceamento_TBR.xlsx")
#   
#   print(paste0("Novo Escopo: PBI_Balanceamento_TBR está atualizada com a ", semana, "!"))
# } else {
#   print("Novo Escopo: PBI_Balanceamento_TBR não foi atualizada, pois ja existe dados desta week escritos!") 
# }


#### PARA O ESCOPO ATUAL - apenas ocupação ####
totalcell_ocup <- totalcell %>% #retirando colunas que serão populadas apenas pela base de oportunidades de ocupação
  select(-OPORTUNIDADE,-BALANCEAMENTO,-DELTA_Tput,-TIPO,-Prio)
total_ocp <- merge(oportunidades_ocp[,c("Célula","OPORTUNIDADE","DELTA_Tput","TIPO")],totalcell[,c("Célula","Prio")], all.x = T) #Adicionando a prioridade ao df de oportunidades de ocupação
totalcell_ocup <- merge(totalcell_ocup,total_ocp, all.x = T) %>% 
  mutate(BALANCEAMENTO = if_else(is.na(OPORTUNIDADE),"Sem Oportunidade","Com Oportunidade"), #Reinserindo a coluna de balanceamento de acordo com apenas a ocupação
         Semana = paste0(str_sub(Semana,3,4),semana)) %>% #Colocando semana no formato do PBI
  rename(Classificação = `Status Ocupação`) %>%
  select(`Endereço ID`,`Semana do Ano`,	Regional,	Estado,	ANF,	Município,	`Status Município`, `Classificação Populacional`, Célula,	Fornecedor,	Banda,	BW_MHz,	Classificação,	OPORTUNIDADE,	TIPO,	Prio,	BALANCEAMENTO,	DELTA_Tput,	Latitude,	Longitude, Semana) %>%
  filter(Classificação != "BOM" & Classificação != "DESCARTADO") %>%
  mutate(Classificação = if_else(Classificação == "ALERTA", "Alerta", "Crítico"))

evolucao <- readxl::read_excel("Outputs/PBI_Balanceamento_TBR.xlsx", sheet = 2)
if (unique(totalcell$Semana) %in% unique(evolucao$Semana) == FALSE){
  #armazenamento semanal
  writexl::write_xlsx(totalcell_ocup,paste0("Outputs/Historico semanal/PBI_Balanceamento_TBR_", semana, ".xlsx"))
  #atualização base PBI
  evolucao <- bind_rows(evolucao, totalcell_ocup)
  DIASDOHISTORICO <- unique(evolucao$Semana) %>% sort()
  if (length(DIASDOHISTORICO) > 26){
    GAP = length(DIASDOHISTORICO) - 26
    evolucao <- evolucao %>% filter(Semana %in% DIASDOHISTORICO[-(1:GAP)])
  }
  writexl::write_xlsx(x = list("Semana" = totalcell_ocup,
                               "Evolução" = evolucao),
                      "Outputs/PBI_Balanceamento_TBR.xlsx")
  
  print(paste0("Escopo Atual: PBI_Balanceamento_TBR está atualizada com a ", semana, "!"))
} else {
  print("Escopo Atual: PBI_Balanceamento_TBR não foi atualizada, pois ja existe dados desta week escritos!") 
}


#### ESCREVER NO BD ####
DBConnection <- RSQLite::dbConnect(drv = RSQLite::SQLite(), "Outputs/BANCO DE DADOS/BD_Oport-Balanceamento.db")  
BD_Verifica <- RSQLite::dbGetQuery(conn = DBConnection, paste0("SELECT Semana FROM controle"))

if (((unique(totalcell$Semana)) %in% BD_Verifica$Semana) == FALSE){
  RSQLite::dbWriteTable(DBConnection, "ocupacao", totalcell_ocup, append = TRUE)
  RSQLite::dbWriteTable(DBConnection, "assessment", totalcell, append = TRUE)
  df_controle <- totalcell %>% select(Semana) %>% unique()
  RSQLite::dbWriteTable(DBConnection, "controle", df_controle, append = TRUE)
  print(paste0("Dados de ", unique(totalcell$Semana), " foram escritos no BD"))
} else{
  print(paste0("Dados de ", unique(totalcell$Semana), " já escrito"))
}

RSQLite::dbDisconnect(DBConnection)