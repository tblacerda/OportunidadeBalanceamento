#pip install --trusted-host pypi.python.org --trusted-host pypi.org --trusted-host files.pythonhosted.org math --user
import pandas as pd
import numpy as np
from geographiclib.geodesic import Geodesic
import math
geod = Geodesic.WGS84
from datetime import date
import time

def find_best_match(df_solucoes,                 # Dataframe com o universo de sites para serem verificados como solução ao setor critico. 
                    CHAVE,                       # Nome da coluna df_solucoes com a chave que será retornada. Pode ser o nome do site, END_ID, OC, etc
                    ID_solucoes,                 # Nome da coluna df_solucoes com o ENDID da chave
                    coluna_lat_solucoes,         # Nome da coluna do DF com as Latitudes dos sites a serem testados
                    coluna_lon_solucoes,         # Nome da coluna do DF com as Longitudes dos sites a serem testados
                    NOME_COLUNA_AZIMUTE,         # nome da coluna do df_solucoes com o azimute   
                    ID_critico,                  # Nome da coluna do DF com o ENDID dos sites a serem testados
                    Lat_critico,                 # Latitude do site critico
                    Lon_critico,                 # Longitude do site critico
                    AZIMUTE_SETOR_CRITICO,       # Azimute do setor critico
                    MAX_DIST,                    # Maxima distância para buscar #### NOVO####

                   ):
    """
    Encontrar o melhor site para resolver o problema do site critico
    Dado LAT, LONG, AZIMUTE E MAX_DIST, encontra o site em plano mais proximo e mais centralizado no azimute informado.
    so considera candidatos com distancia inferior a MAX_DIST
    df_solucoes - DataFrame com as solucoes
    Chave: Qual a coluna com a Chave para interar na Planilha. Pode ser ORDEM_Complexa ou outra.
    """
    # Definições: A: Site_A ou Setor_A refere-se ao site/setor crítico
    #             B: Site_B ou Setor_B refere-se ao site/setor solucao
    ###
    ## Constante
    ABERTURA_HORIZONTAL_ANT = 33 # + OU - 33 = TOTAL 66 GRAUS

    if MAX_DIST < 1000:
        erro = 0.01 #Distancia calculada de 2211m
    elif MAX_DIST >=1000 or MAX_DIST <2000:
        erro = 0.02
    elif MAX_DIS >= 2000 or MAX_DIST <5000:
        erro = 0.05
    else:
        erro =0.09


    menor_dist = 9999999
    azimute_solucao = 0
    site = ''

    df_solucoes.drop_duplicates(inplace=True)
    
    #09/06/2020 - INICIO
    df_solucoes = df_solucoes.loc[abs(abs(df_solucoes[coluna_lat_solucoes]) - abs(Lat_critico)) < erro]
    df_solucoes = df_solucoes.loc[abs(abs(df_solucoes[coluna_lon_solucoes]) - abs(Lon_critico)) < erro]
    #FIM
    for index, row in df_solucoes.iterrows():
        #print("--- %.2f %% Concluido ---" % ((index / total_cel_criticas)*100))
        # O calculo de distancia e azimute completo é computacionalmente dispendioso, por isso realizo um crivo antes para evitar realizar calculos em sites que são muito distantes entre si.
        # Como testado na celula acima, uma diferenca de 0.1 em latitude ou longitude implica em uma distancia superior a 10km. Por isso, ja testo logo. Se o site a ser verificado tem um deltaLat ou
        # deltaLong superior a 0.1, nem calculo. Passa para o proximo site.
        if abs(abs(Lat_critico) - abs(row[coluna_lat_solucoes])) > erro or abs(abs(Lon_critico) - abs(row[coluna_lon_solucoes])) > erro or (ID_critico == row[ID_solucoes]):
            pass
        else:
            calculo =  geod.Inverse(Lat_critico,Lon_critico,row[coluna_lat_solucoes],row[coluna_lon_solucoes])
            distancia = calculo['s12']
            azimute_calculado = calculo['azi1']
            if azimute_calculado < 0:
                azimute_calculado += 360 # Corrige a diferença de azimutes. a API fornece em + e - 180. preciso entre 0 e 360
            #check1: VErificar se o site esta no azimute do setor Critico + ou - 32°
            if abs( abs(AZIMUTE_SETOR_CRITICO) - abs(azimute_calculado)) <= ABERTURA_HORIZONTAL_ANT:
                if distancia < menor_dist:
                    site_solucao = row[ID_solucoes]
                    menor_dist = distancia
                    azimute_solucao = azimute_calculado
            if menor_dist == 0:
                #print('distancia =0')
                return site_solucao
    if menor_dist <= MAX_DIST:
        #print(index)
        ### Cria um DF apenas com as células do END_ID(ID_solucoes é o END_ID) que foram apontados site solução.
        df_setores_site_solucao = df_solucoes[df_solucoes[ID_solucoes]==site_solucao]
        df_setores_site_solucao.drop_duplicates(inplace=True)
        #Lat_critico ### Não muda
        #Lon_critico ### Não muda
        menor_diferenca_azimute = 359
        setor_solucao = ''
        for index2, row2 in df_setores_site_solucao.iterrows():  
                #novo_calculo = geod.Inverse(Lat_critico,Lon_critico,row[coluna_lat_solucoes],row[coluna_lon_solucoes])
                #novo_azimute_calculado = novo_calculo['azi1']
                #### Já sei qual o azimute do setor_critico: AZIMUTE_SETOR_CRITICO
                #### Preciso interar os azimutes do site_solucao para descobrir qual o mais adequado. 
                #### O mais adequado é o que estiver mais proximo possivel de (180 - azimute_solucao)                
                ####### ATENCAO #### Espero receber um df_solucoes com uma coluna AZIMUTE ### <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            if azimute_solucao <= 180:
                calculo_azimute = abs((180 + azimute_solucao) - row2[NOME_COLUNA_AZIMUTE])
                #print(ID_critico, row2[CHAVE],calculo_azimute,azimute_solucao,row2['AZIMUTE'])
            if azimute_solucao > 180:
                calculo_azimute = abs((azimute_solucao - 180) - row2[NOME_COLUNA_AZIMUTE])
                #print(ID_critico, row2[CHAVE],calculo_azimute,azimute_solucao,row2['AZIMUTE'])

            if calculo_azimute < menor_diferenca_azimute: 
                menor_diferenca_azimute = calculo_azimute
                setor_solucao = row2[CHAVE]

        if menor_diferenca_azimute <= 2 * ABERTURA_HORIZONTAL_ANT:
            return setor_solucao
        else: return np.nan
    
    elif menor_dist == 0:
         #print(site,0,0)
        return site_solucao # [site , 0, 0]
    else:
        #print('sem solucao')
        return np.nan


#today = date.today()
#start_time = time.time()
r.Py_vizcritico['OPORTUNIDADE'] = r.Py_vizcritico.apply(lambda x: find_best_match(r.Py_bom_ok, 'Celula', 'END_ID', 'Latitude', 'Longitude', 'Azimute', x['END_ID'], x['Latitude'], x['Longitude'], x['Azimute'], x['Distancia']),axis=1)
#print("--- %s segundos ---" % (time.time() - start_time))
r.Py_vizcritico.to_excel("vizcritico.xlsx", sheet_name='vizcritico', index=False)

