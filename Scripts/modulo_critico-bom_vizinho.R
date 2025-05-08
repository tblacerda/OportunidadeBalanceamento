###################################################################################
# MODULO: OPORTUNIDADE DE BALANCEAMENTO BOM VS CRITICA - busca por sites VIZINHOS #
###################################################################################

# Tratando inputs - inserir coord e azimutes
vizcritico <- cellcritico %>% filter(Célula %in% criticabom$Célula == FALSE)
vizcritico <- vizcritico %>%
  mutate(Distancia = case_when(`Classificação Populacional` == ">=500k hab" | `Classificação Populacional` == "Capital" ~ 900,
                               `Classificação Populacional` == "<100k hab" ~ 1800,
                               TRUE ~ 1500)) %>%
  rename(Azimute = DIR_IRR,
         Celula = Célula,
         END_ID = `Endereço ID`) %>% select(Celula,END_ID,Azimute,Latitude,Longitude,Distancia)

# DF com celulas boas (elegíveis - ja listadas)
outlist2 <- criticabom$OPORTUNIDADE %>% str_split(fixed(',')) %>% unlist(use.names = FALSE) %>% str_sub(start = 2) %>% unique()
outlist <- c(outlist,outlist2)
bom_ok <- bom_ok %>% filter(Célula %in% outlist == FALSE)
bom_ok <- bom_ok %>%
  mutate(Distancia = case_when(`Classificação Populacional` == ">=500k hab" | `Classificação Populacional` == "Capital" ~ 900,
                               `Classificação Populacional` == "<100k hab" ~ 1800,
                               TRUE ~ 1500)) %>%
  rename(Azimute = DIR_IRR,
         Celula = Célula,
         END_ID = `Endereço ID`) %>% select(Celula,END_ID,Azimute,Latitude,Longitude,Distancia)

# Execução do código em python
Py_vizcritico <- r_to_py(vizcritico)
Py_bom_ok <- r_to_py(bom_ok)
start_time <- Sys.time()
py_run_file("Scripts/modulo_oportunidade_vizinhos.py")
print(paste0("Tempo de execuçao da função python = ",Sys.time() - start_time))
vizcritico <- readxl::read_excel("vizcritico.xlsx")
if (file.exists("vizcritico.xlsx")) file.remove("vizcritico.xlsx")
rm(Py_vizcritico, Py_bom_ok)

# Adequação do df de saída para padrão
vizcritico <- vizcritico %>%
  filter(!is.na(OPORTUNIDADE)) %>%
  mutate(TIPO = "Vizinho") %>%
  rename(Célula = Celula)
vizcritico <- merge(vizcritico[,c("Célula","OPORTUNIDADE","TIPO")], base, by.x = "Célula", by.y = "Célula")

# Cálculando Delta Tput para vizinhos
vizcritico = merge(vizcritico, cellbom[,c("Célula", "THROU_USER_PDCP_DL")], by.x = "OPORTUNIDADE", by.y = "Célula")
vizcritico <- vizcritico %>%
  mutate(DELTA_Tput = `THROU_USER_PDCP_DL.y` - `THROU_USER_PDCP_DL.x`) %>%
  rename("THROU_USER_PDCP_DL" = "THROU_USER_PDCP_DL.x") %>%
  select(-"THROU_USER_PDCP_DL.y")

# Excluir casos onde o Delta de Tput é negativo
vizcritico <- vizcritico %>% filter(DELTA_Tput > 0)
vizcritico
