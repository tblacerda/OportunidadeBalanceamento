######################################################################
#### MODULO: TRATAMENTO DE INCONSISTENCIAS-NA'S-DUPLICATAS azi ####
######################################################################

cgi <- cgi_raw %>%
  filter(TECNO == "4G") %>%
  select(`STATION ID`, CELULA, LATITUDE, LONGITUDE, AZIMUTE, LARGURA)%>%
  rename(END_ID = `STATION ID`,
         Célula = CELULA,
         DIR_IRR = AZIMUTE,
         Latitude = LATITUDE,
         Longitude = LONGITUDE,
         BW_MHz = LARGURA)

#### Tratar BW - cgi ####
bw <- cgi %>% 
  select(Célula,BW_MHz) %>%
  unique() %>%
  group_by(Célula) %>%
  slice(which.max(BW_MHz))


#### Tratar azimutes - cgi ####
azi <- cgi %>%
  select(END_ID,Célula,DIR_IRR) %>%
  unique()

# inserir informação de end id e calcular setor
azi <- azi %>% mutate(
  Cell = str_sub(Célula, str_length(Célula)),
  Setor = case_when(Cell %in% c("A", "E", "I", "M", "Q", "1", "X") ~ "1",
                    Cell %in% c("B", "F", "J", "N", "R", "2", "Y") ~ "2",
                    Cell %in% c("C", "G", "K", "O", "S", "3", "Z") ~ "3",
                    Cell %in% c("D", "H", "L", "P", "T", "4") ~ "4"),
  DIR_IRR = as.numeric(DIR_IRR))

#celulas ok
azi_ok <-  azi %>% filter (DIR_IRR >= 0 & !Célula %in% azi$Célula[duplicated(azi$Célula)==TRUE])
#celulas nok
azi_nok <- azi %>% filter(!Célula %in% unique(azi_ok$Célula))
azi_NA <- azi_nok %>% filter(is.na(DIR_IRR)) %>% #filtra cell nok por azimute vazio
  select(-DIR_IRR)

# Create the function of mode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# obter a moda dos azimutes para setores c valores divergentes
azi_ok_setor <- azi_ok %>% select(END_ID,Setor,DIR_IRR) %>% unique() %>%
  group_by(END_ID,Setor) %>%
  summarise(DIR_IRR = getmode(DIR_IRR))

# agregar valor às ccelulas c azimutes vazios
azi_NA_solved <- merge(azi_NA,azi_ok_setor[c("END_ID", "Setor", "DIR_IRR")], by = c("END_ID", "Setor")) %>%
  unique()

# lista de celulas c azimutes ok
azi_final <- bind_rows(azi_ok,azi_NA_solved)
azi_duplicated <- azi_final %>% filter(Célula %in% azi_final$Célula[duplicated(azi_final$Célula)==TRUE]) #selecionar duplicatas azimute para manter registro
azi_final <- azi_final %>% filter(!Célula %in% unique(azi_duplicated$Célula)) %>% #remover duplicatas p segir c o script
  select(Célula,DIR_IRR)

#### Tratamento lat-long ####
latlong <- cgi %>% select(Célula,Latitude,Longitude) %>% unique()
latlong_duplicated <- latlong %>% filter(Célula %in% latlong$Célula[duplicated(latlong$Célula)==TRUE]) #selecionar duplicatas azimute para manter registro
latlong <- latlong %>% filter(!Célula %in% unique(latlong_duplicated$Célula)) #remover duplicatas p segir c o script


#### Juntando bases ####
cgi_ok <- cgi %>% select(Célula) %>% unique()
cgi_ok <- cgi_ok %>% 
  merge(azi_final, all.x = TRUE) %>%
  merge(latlong, all.x = TRUE) %>%
  merge(bw, all.x = TRUE)


######################################################################################

#azi_dup <- azi_nok %>% filter(!is.na(DIR_IRR)) #filtra cell nok por duplicata
#length(unique(azi_ok$Célula))
#length(azi$Célula[is.na(azi$DIR_IRR)==TRUE])

# #### CLASSIFICANDO SETOR ####
# azi <- cgi %>% mutate(
#   Cell = str_sub(Célula, str_length(Célula)),
#   Setor = case_when(Cell %in% c("A", "E", "I", "M", "Q", "1", "X") ~ "1",
#                     Cell %in% c("B", "F", "J", "N", "R", "2", "Y") ~ "2",
#                     Cell %in% c("C", "G", "K", "O", "S", "3", "Z") ~ "3",
#                     Cell %in% c("D", "H", "L", "P", "T", "4") ~ "4"),
#   DIR_IRR = as.numeric(DIR_IRR)) %>%
#   left_join(base %>% select("Endereço ID", "Célula"), by = c("Célula" = "Célula"))
# 
# 
# #### SEPARANDO CÉLULAS OK #### (um unico valor de azimute diferente de NA)
# azi_ok <-  azi %>% filter (DIR_IRR >= 0 & !Célula %in% azi$Célula[duplicated(azi$Célula)==TRUE])
# #azi_ok$Célula[duplicated(azi_ok$Célula)==TRUE] # verificando duplicatas
# #azi_ok$Célula[is.na(azi_ok$Célula)==TRUE] # verificando NA
# ## separando valores duplicados e mantendo apenas a linha que contem um valor preenchido, caso exista (caso hajam 2 valores, a linha é descartada)
# azi_duplicated <- azi %>% filter(Célula %in% azi$Célula[duplicated(azi$Célula)==TRUE]) # filtrando células duplicadas
# azi_duplicated <- azi_duplicated %>% group_by(Célula) %>% filter(!is.na(DIR_IRR)) %>% distinct() # mantendo, dentre as duplicadas, a que possui valor de azimute preenchido
# azi_duplicated_solved <- azi_duplicated %>% filter(!Célula %in% azi_duplicated$Célula[duplicated(azi_duplicated$Célula)==TRUE]) # removendo casos em que há dois valores preenchidos
# azi_duplicated_unsolved <- azi_duplicated %>% filter(Célula %in% azi_duplicated$Célula[duplicated(azi_duplicated$Célula)==TRUE]) # removendo casos em que há dois valores preenchidos
# 
# azi_duplicated_unsolved <- azi_duplicated_unsolved %>%
#   group_by(Célula) %>%
#   slice(which.max(BW_MHz))
# azi_duplicated_2solved <- azi_duplicated_unsolved%>% filter(!Célula %in% azi_duplicated_solved$Célula[duplicated(azi_duplicated_solved$Célula)==TRUE]) # removendo casos em que há dois valores preenchidos
# 
# #azi_duplicated_solved$Célula[duplicated(azi_duplicated_solved$Célula)==TRUE] # verificando duplicatas
# #azi_duplicated_solved$Célula[is.na(azi_duplicated_solved$Célula)==TRUE] # verificando NA
# 
# azi_ok <- azi_ok %>%
#   bind_rows(azi_duplicated_solved,azi_duplicated_2solved)
# 
# 
# #### Separando células com azimute NA ####
# azi_NA <- azi %>% filter (is.na(DIR_IRR) & !Célula %in% azi_ok$Célula & str_detect(`Endereço ID`,"[A-Z]")) %>% select(-DIR_IRR)
# azi_NA_solved <- merge(azi_NA,azi_ok[c("Endereço ID", "Setor", "DIR_IRR")], by = c("Endereço ID", "Setor")) %>%
#   unique()
# 
# azi_ok <- azi_ok %>%
#   bind_rows(azi_NA_solved)
# 
# azi_ok <- azi_ok %>% unique()
# 
# # azi_NA <- azi_NA %>%
# #   left_join(azi_ok %>% select("Endereço ID", "Setor", "DIR_IRR"), by = c("Endereço ID", "Setor")) %>%
# #   distinct() 
# # azi_NA_solved <- azi_NA %>%
# #   filter(!Célula %in% azi_NA$Célula[duplicated(azi_NA$Célula)==TRUE] & !is.na(DIR_IRR))
# # 
# # azi_NA_unsolved <- azi_NA %>%
# #   filter(Célula %in% azi_NA$Célula[duplicated(azi_NA$Célula)==TRUE] | is.na(DIR_IRR)) %>%
# #   left_join(base, by = c("Endereço ID" = "Endereço ID", "Célula" = "Célula")) %>%
# #   select(Regional, Estado, ANF, Município, `Endereço ID`, Site, Célula, DIR_IRR) #%>%
# # 
# # azi_duplicated_unsolved <- azi_duplicated_unsolved %>%
# #   left_join(base, by = c("Endereço ID" = "Endereço ID", "Célula" = "Célula")) %>%
# #   select(Regional, Estado, ANF, Município, `Endereço ID`, Site, Célula, DIR_IRR) #%>%
# # 
# # 
# # #### ESCREVENDO OUTPUT DE INCONSISTENCIAS ####
# # writexl::write_xlsx(list(#"Spazio" = spazio_duplicated,
# #                          "azi_Duplicatas" = azi_duplicated_unsolved,
# #                          "azi_NAs" = azi_NA_unsolved), paste0("Outputs/Inconsistências/Inconsistências_Bases_Balanceamento_", semana, ".xlsx"))
# # 
# 
# #### UNINDO azi TRATADA ####
# # azi_ok <- azi_ok %>%
# #   bind_rows(azi_NA_solved)
# #azi_ok$Célula[duplicated(azi_ok$Célula)==TRUE]
# #azi_ok$Célula[is.na(azi_ok$DIR_IRR)==TRUE]

# teste_azi <- azi_final %>% select(Célula, DIR_IRR) %>% unique()
# teste_bw <- bw %>% select(Célula, BW_MHz) %>% unique()
# teste_cel <- cgi %>% select(Célula) %>% unique()
