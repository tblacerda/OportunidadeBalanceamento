import pandas as pd
import numpy as np
import re
import openpyxl

print('Carregando arquivo...')

arquivo = "Outputs/Historico semanal/PBI_Balanceamento_TBR_"+r.Py_semana+".xlsx"
df = pd.read_excel(arquivo)

print('Filtrando linhas apenas com oportunidades...')
df = df[df['Regional'] == 'TNE']
df = df[df['BALANCEAMENTO'] == 'Com Oportunidade']


def separa(oportunidade):
    return oportunidade.split(',')


def mMIMO(celula):  # Tiago
    '''
    Entrada: Nome da célula
    Saida: True -> é uma célula de uma mMIMO.
    False: não é
    '''
    def NovaNomenclatura(celula):
        temp = False
        if celula.find('-') != -1:
            temp = True
        return (bool(re.search('[0-9][ABCDQRIJKLUV123456]$', celula)) and temp)
    def VelhaNomenclatura(celula):
        return not(celula.startswith(('4G','S4'))) and (bool(re.search('[0-9][0-9][0-9]?[I]?[0-9][A-Z]?[ABCD1234QRSTXYZI]$',celula)))
    return (NovaNomenclatura(celula) or VelhaNomenclatura(celula))
        
df['SEPARADO'] = df.apply(lambda x: separa(x['OPORTUNIDADE']),axis=1)


df_explode = df.explode('SEPARADO')

df_explode['MMIMO'] = df_explode.apply(lambda x: mMIMO(x['SEPARADO']),axis=1)

print('Filtrando linhas apenas com oportunidades...')
df_explode = df_explode[df_explode['MMIMO'] == True]

# cria df_count apenas com o nome da célula e quantas vezes a célula aparece
df_count = df_explode.groupby(['Célula'])['Célula'].count().reset_index(name='counts')

# remove todas as células com count > 1
df_count = df_count[df_count['counts'] > 1]

# remove a coluna de contagem
df_count = df_count.drop('counts', axis=1)

# junta os dois df pela célula
df_explode = pd.merge(df_explode, df_count, on="Célula")

df_explode = df_explode.drop('SEPARADO', axis=1)
df_explode = df_explode.drop('MMIMO', axis=1)
df_explode = df_explode.drop_duplicates()

df_explode.to_excel("Outputs/Historico semanal/PBI_Balanceamento_TNE_mMIMO_"+r.Py_semana+".xlsx", index=False)
