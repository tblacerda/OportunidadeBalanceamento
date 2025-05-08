arrayDatas <- c("202315")
arrayDatas2 <- c("23W15")

## Define conexão com o banco de dados
DBConnection <- RSQLite::dbConnect(drv = RSQLite::SQLite(), "Outputs/BANCO DE DADOS/BD_Oport-Balanceamento.db")  


## Executando query de exclusão
for (i in 1:length(arrayDatas)) {
  dbGetQuery(DBConnection, paste0("DELETE FROM controle WHERE Semana = '", arrayDatas[i],"'"))
  dbGetQuery(DBConnection, paste0("DELETE FROM assessment WHERE Semana = '", arrayDatas[i],"'"))
  dbGetQuery(DBConnection, paste0("DELETE FROM ocupacao WHERE Semana = '", arrayDatas2[i],"'"))
}
