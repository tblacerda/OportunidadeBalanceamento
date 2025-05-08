############################################################
# MODULO: FATOR DE CRITICIDADE DA CIDADE: STATUS MUNICÍPIO #
############################################################

# CALCULO DE PORCENTAGEM TRAF_TIM CIDADE #
#Em Cidades com Vol Traf Tim < 15%, Celulas com Traf TIM <10% seriam expurgadas

# CALCULANDO AGREGADO #
totalcidade <- baseocup_raw %>% select(Município,Estado,Célula,`Re-classificação LT`)
criticocidade <- totalcidade %>% filter(`Re-classificação LT` == "CRITICO") %>% group_by(Município, Estado) %>% summarise(CriticoCell = n())
totalcidade <- totalcidade %>% group_by(Município, Estado) %>% summarise(TotalCell = n())

cidade <- full_join(totalcidade, criticocidade, by = c("Município","Estado"))
rm(totalcidade, criticocidade)
cidade <- cidade[!is.na(cidade$Município) & !is.na(cidade$Estado), ]
cidade[is.na(cidade)] <- 0

# DETERMINAÇÃO DA CLASSIFICAÇÃO #
cidade <- cidade %>% mutate(`Valor Município` = CriticoCell/TotalCell) %>%
  mutate(`Classificação Município` = case_when(`Valor Município` >= 0.2 ~ "Business Critical",
                                               `Valor Município` < 0.2 & `Valor Município` >= 0.1 ~ "Crítico",
                                               `Valor Município` < 0.1 & `Valor Município` >= 0.05 ~ "Alerta",
                                               TRUE ~ "Bom"))
         
cidade <- cidade %>% select("Estado","Município","Classificação Município")

# DETERMINAÇÃO DO STATUS DO MUNICÍPIO #
totalcell <- left_join(totalcell,cidade, by= c("Estado", "Município"))

totalcell <- totalcell %>%
  mutate(`Status Município` =
           case_when((`Classificação Populacional` != "<100k hab" & `Classificação Populacional` != ">=100k e <200k hab") & `Classificação Município` != "Bom" ~ "FORA DA META",
                     (`Classificação Populacional` != "<100k hab" & `Classificação Populacional` != ">=100k e <200k hab") & `Classificação Município` == "Bom" ~ "DENTRO DA META",
                     
                     `Classificação Populacional` == ">=100k e <200k hab" & (`Classificação Município` == "Business Critical"|`Classificação Município` == "Crítico") ~ "FORA DA META",
                     `Classificação Populacional` == ">=100k e <200k hab" & (`Classificação Município` == "Alerta") ~ "RISCO",
                     `Classificação Populacional` == ">=100k e <200k hab" & (`Classificação Município` == "Bom") ~ "DENTRO DA META",
                     
                     `Classificação Populacional` == "<100k hab" & (`Classificação Município` == "Business Critical") ~ "FORA DA META",
                     `Classificação Populacional` == "<100k hab" & (`Classificação Município` == "Crítico") ~ "RISCO",
                     TRUE ~ "DENTRO DA META")) %>%
  select(-"Classificação Município")

