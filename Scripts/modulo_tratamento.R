##### MODULO DE TRATAMENTO #####

### GUARDAR ATUALIZAÇÕES DE TRATAMENTO NO BD
# LENDO EXCEL COM TRATAMENTOS
tratados <- readxl::read_xlsx("Outputs/TRATAMENTO/Tratamento_de_Oportunidades_de_Balanceamento_TBR.xlsx", col_types = c("text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","numeric","text","text","text","text","text","date","text"))
tratados <- tratados %>% 
  filter(!is.na(AÇAO) & !is.na(DATA_DA_AÇAO) & !is.na(RESPONSAVEL)) %>% # filtra apenas os que foram tratados e corretamente preenchidos
  mutate(DATA_DA_AÇAO = as.character.Date(DATA_DA_AÇAO)) %>% # colocar data em string
  select(-HISTORICO_AÇAO_CELL,-HISTORICO_AÇAO_SITE)

# ESCREVER NO BD
DBConnection <- RSQLite::dbConnect(drv = RSQLite::SQLite(), "Outputs/BANCO DE DADOS/BD_Oport-Balanceamento_Tratamento.db")  

if (dim(tratados)[1] > 0){
  BD_Verifica <- RSQLite::dbGetQuery(conn = DBConnection, paste0("SELECT Semana FROM controle"))
  if (((unique(tratados$Semana)) %in% BD_Verifica$semana) == FALSE){
    RSQLite::dbWriteTable(DBConnection, "historico_tratamento", tratados, append = TRUE)
    df_controle <- tratados %>% select(Semana) %>% unique()
    RSQLite::dbWriteTable(DBConnection, "controle", df_controle, append = TRUE)
    print(paste0("Dados de ", unique(tratados$Semana), " foram escritos no BD"))
  } else{
    print(paste0("Dados de ", unique(tratados$Semana), " já escrito"))
  }
} else{
  print(paste0("Não houveram novas celulas tratadas nesta semana, desta forma não houve escrita no banco de diagnóstico."))
}

### MAPINIPULAR OUTPUT DE TRATAMENTO
#totalcell_ocup = readxl::read_excel("Outputs/Historico semanal/PBI_Balanceamento_TBR_W25.xlsx") #foi para teste
# CRIANDO BASE PARA TRATAMENTO
tratamento = totalcell_ocup %>%
  filter(BALANCEAMENTO == "Com Oportunidade") %>%
  mutate("AÇAO"=NA,
         "DATA_DA_AÇAO"=NA,
         "RESPONSAVEL"=NA)

# LER DB
BD_historico <- RSQLite::dbGetQuery(conn = DBConnection, paste0("SELECT `Endereço ID`,Célula,AÇAO,DATA_DA_AÇAO,RESPONSAVEL FROM historico_tratamento"))
BD_historico <- BD_historico %>%
  filter(`Endereço ID` %in% unique(tratamento$`Endereço ID`)) %>%
  # historico de ação por celula
  arrange(`Endereço ID`,Célula,DATA_DA_AÇAO) %>%
  mutate(CONCAT_ACAO = paste0("[",DATA_DA_AÇAO,"] ",AÇAO, " - ", RESPONSAVEL)) %>%
  group_by(`Célula`) %>%
  mutate(HISTORICO_AÇAO_CELL = paste0(CONCAT_ACAO, collapse = " ")) %>%
  select(`Endereço ID`,Célula,HISTORICO_AÇAO_CELL) %>%
  unique() %>%
  # historico de ação por site (end id)
  mutate(CONCAT_ACAO_CELL = paste0(Célula," = ",HISTORICO_AÇAO_CELL)) %>%
  group_by(`Endereço ID`) %>%
  mutate(HISTORICO_AÇAO_SITE = paste0(CONCAT_ACAO_CELL, collapse = " || ")) %>%
  ungroup() %>%
  select(Célula,HISTORICO_AÇAO_CELL,HISTORICO_AÇAO_SITE) %>%
  unique()
# desconectar do BD
RSQLite::dbDisconnect(DBConnection)
# merge com a base para tratamento
tratamento <- merge(tratamento, BD_historico, by = "Célula", all.x = T)

# FLAG mMIMO (apenas TNE por enquanto)
mMIMO = readxl::read_excel(paste0("Outputs/Historico semanal/PBI_Balanceamento_TNE_mMIMO_", semana, ".xlsx")) %>%
  select(Célula) %>%
  mutate(FLAG_mMIMO = "Oportunidade com mMIMO")
# merge com a base para tratamento
tratamento <- merge(tratamento, mMIMO, by = "Célula", all.x = T)


# inserindo informação nas colunas de historico para evitar preenchimento indevido
tratamento <- tratamento %>%
  mutate(HISTORICO_AÇAO_CELL = if_else(is.na(HISTORICO_AÇAO_CELL),"SEM REGISTROS",HISTORICO_AÇAO_CELL),
         HISTORICO_AÇAO_SITE = if_else(is.na(HISTORICO_AÇAO_SITE),"SEM REGISTROS",HISTORICO_AÇAO_SITE))
# organizando output
tratamento = tratamento %>%
  select("Semana do Ano","Semana","Regional","Estado","ANF","Município","Status Município",
         "Classificação Populacional","Endereço ID","Célula","Fornecedor","Banda","BW_MHz",
         "Classificação","OPORTUNIDADE","TIPO","Prio","DELTA_Tput","BALANCEAMENTO","FLAG_mMIMO",
         "HISTORICO_AÇAO_SITE","HISTORICO_AÇAO_CELL","AÇAO","DATA_DA_AÇAO","RESPONSAVEL") %>%
  arrange(Regional,Estado,ANF,Município,Prio)

## ESCREVENDO OUTPUT
writexl::write_xlsx(tratamento,"Outputs/TRATAMENTO/Tratamento_de_Oportunidades_de_Balanceamento_TBR.xlsx")
