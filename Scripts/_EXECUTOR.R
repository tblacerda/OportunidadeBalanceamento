system("type R")
R.home()
file.path(R.home("bin"), "R")

#loop do código main
rm(list=ls())
library(dplyr)
library(stringr)
library(gdata) 
#update.packages(ask = FALSE, repos = "https://cran.r-project.org") 
#install.packages("dplyr", repos = "https://cloud.r-project.org")
#install.packages("dplyr", repos = "https://cran-r.c3sl.ufpr.br/")
#install.packages("dplyr")
#https://cran.r-project.org/web/packages/<PACKAGE NAME>/index.html
#install.packages("C:/Users/F8058552/Downloads/readxl_1.4.3.zip", pkgType = "binary", repos = NULL)

#### PREPARANDO AMBIENTE PARA CÓDIGO PYTHON  ####
library(reticulate)
# Alterar instância do Python
#use_python("C:/Users/F8039139/anaconda3/python.exe", required = T) # Monique
#use_python("C:/Program Files/Python37/python.exe", required = T) # Thaina
use_python("C:/Users/F8058552/AppData/Local/anaconda3/envs/clusters", required = T)
# Verificar instância do Python
py_config()

##### IMPORTANDO BASES ####
hoje = lubridate::today()
hoje
ano = "2024"
semanas = c("W48")
semana = "W48"
cgi_raw <- readxl::read_xlsx(dir('C:/Users/F8058552/OneDrive - TIM/CGI MicroStrategy/', full.names=T, pattern="^CGI MicroStrategy - Tim Brasil"))

for (semana in semanas){
  print(semana)
  source("Scripts/main.R", encoding = "utf-8")
} 
