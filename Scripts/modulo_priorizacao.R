#######################
# MODULO: PRIORIZAÇÃO #
#######################

priorizacao <- totalcell %>% filter(BALANCEAMENTO == "Com Oportunidade") %>%
  mutate(Prio = case_when(
    (`Status Município` == "FORA DA META" & `Status Ocupação` == "CRITICO" & (`Classificação Populacional` != "<100k hab" & `Classificação Populacional` != ">=100k e <200k hab") ) ~ "0",
    (`Status Município` == "FORA DA META" & `Status Ocupação` == "CRITICO" & `Classificação Populacional` == ">=100k e <200k hab") ~ "01",
    (`Status Município` == "FORA DA META" & `Status Ocupação` == "CRITICO" & `Classificação Populacional` == "<100k hab") ~ "02",
    (`Status Município` == "FORA DA META" & `Status Ocupação` == "ALERTA" & (`Classificação Populacional` != "<100k hab" & `Classificação Populacional` != ">=100k e <200k hab") ) ~ "03",
    (`Status Município` == "FORA DA META" & `Status Ocupação` == "ALERTA" & `Classificação Populacional` == ">=100k e <200k hab") ~ "04",
    (`Status Município` == "FORA DA META" & `Status Ocupação` == "ALERTA" & `Classificação Populacional` == "<100k hab") ~ "05",
    
    (`Status Município` == "RISCO" & `Status Ocupação` == "CRITICO" & `Classificação Populacional` == ">=100k e <200k hab") ~ "06",
    (`Status Município` == "RISCO" & `Status Ocupação` == "CRITICO" & `Classificação Populacional` == "<100k hab") ~ "06",
    (`Status Município` == "RISCO" & `Status Ocupação` == "ALERTA" & `Classificação Populacional` == ">=100k e <200k hab") ~ "07",
    (`Status Município` == "RISCO" & `Status Ocupação` == "ALERTA" & `Classificação Populacional` == "<100k hab") ~ "07",
    
    (`Status Município` == "DENTRO DA META" & `Status Ocupação` == "CRITICO" & (`Classificação Populacional` != "<100k hab" & `Classificação Populacional` != ">=100k e <200k hab") ) ~ "08",
    (`Status Município` == "DENTRO DA META" & `Status Ocupação` == "CRITICO" & `Classificação Populacional` == ">=100k e <200k hab") ~ "09",
    (`Status Município` == "DENTRO DA META" & `Status Ocupação` == "CRITICO" & `Classificação Populacional` == "<100k hab") ~ "10",
    (`Status Município` == "DENTRO DA META" & `Status Ocupação` == "ALERTA" & (`Classificação Populacional` != "<100k hab" & `Classificação Populacional` != ">=100k e <200k hab") ) ~ "11",
    (`Status Município` == "DENTRO DA META" & `Status Ocupação` == "ALERTA" & `Classificação Populacional` == ">=100k e <200k hab") ~ "12",
    (`Status Município` == "DENTRO DA META" & `Status Ocupação` == "ALERTA" & `Classificação Populacional` == "<100k hab") ~ "13",
    (CRITÉRIO == "Assessment") ~ "14"
    )
  )

totalcell <- merge(totalcell, priorizacao[c("Célula", "Prio")], by = "Célula", all.x = T)
