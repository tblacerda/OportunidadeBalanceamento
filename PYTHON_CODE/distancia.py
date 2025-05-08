import pandas as pd
import numpy as np
#from geopy.distance import geodesic 
from geographiclib.geodesic import Geodesic
import math
geod = Geodesic.WGS84

from datetime import date
import time


df_cel_Criticas = pd.read_excel('./DADOS_REAIS_DEZ2019.xlsx', header=0, sheet_name=0)
df_aba1 = pd.read_excel('./DADOS_REAIS_DEZ2019.xlsx', header=0, sheet_name=1)
df_spazio = pd.read_excel('./dados/Spazio_2020_01_13.xlsx', header = 0)

df_spazio = df_spazio[['Endereço ID','Latitude','Longitude']]
df_spazio.rename(columns={"Latitude": "LAT", "Longitude": "LONG"}, inplace = True)

def converter(valor):

    resultado = valor.strip().replace(',','.')

    return resultado

df_spazio['LAT'] = df_spazio['LAT'].astype(str)
df_spazio['LAT'] = df_spazio.apply(lambda x: converter(x['LAT']), axis =1 )
df_spazio['LAT'] = df_spazio['LAT'].astype(float)

df_spazio['LONG'] = df_spazio['LONG'].astype(str)
df_spazio['LONG'] = df_spazio.apply(lambda x: converter(x['LONG']), axis =1 )
df_spazio['LONG'] = df_spazio['LONG'].astype(float)


df_aba1 = df_aba1.merge(df_spazio, left_on = 'ENDEREÇO ID', right_on='Endereço ID', how='left')
df_cel_Criticas = df_cel_Criticas.merge(df_spazio, left_on = 'END ID', right_on='Endereço ID', how='left')

def find_best_match(LAT, LONG, AZIMUTE_SETOR_CRITICO, MAX_DIST):
    """
    Encontrar o melhor site para resolver o problema do site critico
    Dado LAT, LONG, AZIMUTE E MAX_DIST, encontra o site em plano mais proximo e mais centralizado no azimute informado.
    so considera candidatos com distancia inferior a MAX_DIST
    """
    menor_dist = 9999999
    azimute_solucao = 0
    site = ''
    for index, row in df_aba1.iterrows():

        if abs(abs(LAT) - abs(row['LAT'])) > 0.1 or abs(abs(LONG) - abs(row['LONG'])) > 0.1:
            pass
        else:
            calculo =  geod.Inverse(LAT,LONG,row['LAT'],row['LONG'])
            distancia = calculo['s12']
            azimute_calculado = calculo['azi1']

            if azimute_calculado < 0:
                azimute_calculado += 360 # Corrige a diferença de azimutes. a API fornece em + e - 180. preciso entre 0 e 360
        
            #check1: VErificar se o site esta no azimute do setor Critico + ou - 35°
            if abs(abs(AZIMUTE_SETOR_CRITICO) - abs(azimute_calculado)) <= 32:
                if distancia < menor_dist:
                    site = row['Elemento_ID(Site_ID)']
                    menor_dist = distancia
                    azimute_solucao = azimute_calculado
 #           if menor_dist == 0:
 #               return site, 0 , 0
    
    if menor_dist <= MAX_DIST:
        return site , menor_dist, azimute_solucao

    elif menor_dist == 0:
        return site , 0, 0

    else:
        return np.nan    


today = date.today()
start_time = time.time()

df_cel_Criticas['Solução Encontrada'] = df_cel_Criticas.apply(lambda x: find_best_match(x['LAT'], x['LONG'], x['azimute'], x['DISTÂNCIA']),axis=1)

df_cel_Criticas.to_excel('saida.xlsx')

print("--- %s segundos ---" % (time.time() - start_time))
