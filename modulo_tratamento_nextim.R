######################################################################
#### MODULO: TRATAMENTO DE INCONSISTENCIAS-NA'S-DUPLICATAS azi ####
######################################################################

# CORRIGIR INCONSISTENCIA DE: EXEMPLO - 7NLBOGV20E
cgi <- raw_cgi %>%
  filter(TECNO == "4G") %>%
  select(REGIONAL, UF, ANF, CIDADE, IBGE,`STATION ID`, TECNO, SITE, CELULA, LATITUDE, LONGITUDE, AZIMUTE, FREQ, LARGURA)%>%
  rename(`Endereço ID` = `STATION ID`,
         Célula = CELULA,
         DIR_IRR = AZIMUTE,
         Latitude = LATITUDE,
         Longitude = LONGITUDE,
         BW_MHz = LARGURA,
         Regional = REGIONAL,
         Estado = UF,
         Município = CIDADE)

TA ERRADO AQUI! TEM QUE ARRUMAR AS COLUNAS


#### INCONSISTENCIA DE LOCAL CADASTRADO ####
local_cadastro <- cgi %>% select(Célula, REGIONAL, UF, ANF, CIDADE, IBGE,`Endereço ID`,Latitude,Longitude) %>% unique()
local_cadastro <- local_cadastro %>% filter(!is.na(Latitude) & !is.na(Longitude) & !is.na(`Endereço ID`) & str_detect(`Endereço ID`,"[A-Z]")) #Retirando valores nulos de LatLong e EndID, bem como end_ids inconsistentes
local_cadastro_dup <- local_cadastro %>% filter(Célula %in% local_cadastro$Célula[duplicated(local_cadastro$Célula)==TRUE]) #selecionar duplicatas azimute para manter registro
local_cadastro <- local_cadastro %>% filter(!Célula %in% unique(local_cadastro_dup$Célula)) #remover duplicatas p segir c o script


#### LARGURA DE BANDA BW ####
bw <- cgi %>%
  filter(Célula %in% unique(local_cadastro$Célula))%>%
  select(Célula,BW_MHz) %>%
  unique() %>%
  group_by(Célula) %>%
  slice(which.max(BW_MHz)) #Já elimina BW == "ND"


#### FREQ ####
freq <- cgi %>% filter(Célula %in% unique(local_cadastro$Célula))%>%
  select(Célula,FREQ) %>%
  unique() %>%
  filter(!is.na(FREQ) & FREQ != "ND") #Retirando valores nulos de freq
freq_dup <- freq %>% filter(Célula %in% freq$Célula[duplicated(freq$Célula)==TRUE]) #selecionar duplicatas freq para manter registro
freq <- freq %>% filter(!Célula %in% unique(freq_dup$Célula)) #remover duplicatas p segir c o script


#### AZIMUTE ####
azi <- cgi %>% filter(Célula %in% unique(local_cadastro$Célula))%>%
  select(`Endereço ID`,Célula,DIR_IRR) %>%
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
azi_ok <-  azi %>% filter(DIR_IRR >= 0 & !Célula %in% azi$Célula[duplicated(azi$Célula)==TRUE])
#celulas nok
azi_nok <- azi %>% filter(!Célula %in% unique(azi_ok$Célula))
azi_NA <- azi_nok %>% filter(is.na(DIR_IRR)) %>% #filtra cell nok por azimute vazio
  select(-DIR_IRR)
#celulas dup para manter registro
azi_dup <- azi %>% filter(Célula %in% azi$Célula[duplicated(azi$Célula)==TRUE]) 

# obter a moda dos azimutes para setores c valores divergentes
## Create the function of mode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
azi_setor <- azi_ok %>% select(`Endereço ID`,Setor,DIR_IRR) %>% unique() %>%
  group_by(`Endereço ID`,Setor) %>%
  summarise(DIR_IRR = getmode(DIR_IRR))

# agregar valor às celulas c azimutes vazios
azi_NA_solved <- merge(azi_NA,azi_setor[c("Endereço ID", "Setor", "DIR_IRR")], by = c("Endereço ID", "Setor")) %>%
  unique()

# lista de celulas c azimutes ok
azi_final <- bind_rows(azi_ok,azi_NA_solved)
azi_final <- azi_final %>% filter(!Célula %in% unique(azi_dup$Célula)) %>%
  select(Célula,DIR_IRR)


#### Juntando bases ####
cgi_ok <- local_cadastro %>%
  merge(azi_final,  by = "Célula", all.x = T) %>%
  merge(freq,  by = "Célula", all.x = T) %>%
  merge(bw,  by = "Célula", all.x = T)

# teste_cgi <- cgi_ok %>% filter(Célula %in% duplicated(cgi_ok$Célula)) %>%
#   select(Célula) %>% unique()

# # escrever excel pbi
writexl::write_xlsx(x = list("Azimute" = azi_dup,
                             "Cadastro" = local_cadastro_dup,
                             "FREQ" = freq_dup),
                    "Outputs/Registros_duplicados_CGI.xlsx")
  
rm(freq,freq_dup,bw,azi,azi_ok,azi_final,azi_nok,azi_NA_solved,azi_setor,azi_NA,azi_dup,local_cadlocal_cadastro_dup)
