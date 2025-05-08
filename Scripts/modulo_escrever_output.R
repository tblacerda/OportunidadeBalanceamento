#######################################
# MODULO: ESCREVER INPUT PBI EM EXCEL #
#######################################

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

# fechar conexao com bd
RSQLite::dbDisconnect(DBConnection)

#### ESCREVER EXCEL #####

#### PARA O NOVO ESCOPO - assessment + ocupação
# quantidade de semanas no pbi
qntd_semanas_pbi = 4

writexl::write_xlsx(totalcell,paste0("Outputs/Novo_Escopo/Historico semanal/PBI_Balanceamento_TBR_", semana, ".xlsx"))

# range_pbi = unique(RSQLite::dbGetQuery(conn = DBConnection, "SELECT Semana FROM assessment"))$Semana %>% sort()
# tamanho_vetor = length(range_pbi)
# range_pbi = range_pbi[-(1:(tamanho_vetor-qntd_semanas_pbi))]
# str_range_pbi = paste0("(",str_c(range_pbi, collapse = ", "),")") #colocando no formato para o sql "entender"
# 
# evolucao <- RSQLite::dbGetQuery(conn = DBConnection, paste0("SELECT * FROM assessment WHERE Semana IN ", str_range_pbi))
# 
# # fechar conexao com bd
# RSQLite::dbDisconnect(DBConnection)

# # escrever excel pbi
# writexl::write_xlsx(x = list("Semana" = totalcell,
#                              "Evolução" = evolucao),
#                     "Outputs/Novo_Escopo/PBI_Balanceamento_TBR.xlsx")


#### PARA O ESCOPO ATUAL - apenas ocupação
# quantidade de semanas no pbi
qntd_semanas_pbi = 15
## TIAGO
vetor_semanas <- c()
for (i in 0:(qntd_semanas_pbi - 1)) {
  week_num <- as.numeric(str_sub(semana, 2)) - i
  if (week_num <= 0) {
    week_num <- 52 + week_num
    ano_num <- as.numeric(str_sub(ano, 3, 4)) - 1
  } else {
    ano_num <- as.numeric(str_sub(ano, 3, 4))
  }
  vetor_semanas <- c(vetor_semanas, paste0(ano_num, "W", week_num))
}
vetor_semanas

writexl::write_xlsx(totalcell_ocup,paste0("Outputs/Historico semanal/PBI_Balanceamento_TBR_", semana, ".xlsx"))


# abrir conexao com bd
DBConnection <- RSQLite::dbConnect(drv = RSQLite::SQLite(), "Outputs/BANCO DE DADOS/BD_Oport-Balanceamento.db")  

# TIAGO
# range_pbi = unique(RSQLite::dbGetQuery(conn = DBConnection, "SELECT Semana FROM ocupacao"))$Semana %>% sort()
# tamanho_vetor = length(range_pbi)
# range_pbi = range_pbi[-(1:(tamanho_vetor-qntd_semanas_pbi))]
# str_range_pbi = paste0("('",str_c(range_pbi, collapse = "', '"),"')") #colocando no formato para o sql "entender"

str_range_pbi = paste0("('",str_c(vetor_semanas, collapse = "', '"),"')") #colocando no formato para o sql "entender"
# evolucao <- RSQLite::dbGetQuery(conn = DBConnection, paste0("SELECT * FROM ocupacao WHERE Semana IN ", str_range_pbi))
evolucao <- RSQLite::dbGetQuery(conn = DBConnection, paste0("SELECT * FROM ocupacao WHERE Semana IN ", str_range_pbi))
# evolucao
# str_range_pbi
# fechar conexao com bd
RSQLite::dbDisconnect(DBConnection)

# escrever excel pbi
writexl::write_xlsx(x = list("Semana" = totalcell_ocup,
                             "Evolução" = evolucao),
                    "Outputs/PBI_Balanceamento_TBR.xlsx")




