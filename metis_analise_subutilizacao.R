library(dplyr)
library(XLConnect)
library(stringr)

jp_raw <- readxl::read_excel("jampa_2600_12072022.xlsx")
jp <- jp_raw %>% group_by(Cell) %>% slice(which.max(`TIM_PRB_UTIL_MEAN_DL (%)`)) %>%
  filter(`TIM_PRB_UTIL_MEAN_DL (%)` < 10)

writexl::write_xlsx(jp, "JoaoPessoa_cell2600_subutilizadas.xlsx")
