import pandas as pd
import numpy as np

# 1 Escriba un programa en Pandas que permita agregar datos a 
# un serie existente. Use los  siguientes datos de prueba

list = input("Digite los datos que desea agregar a la lista: ")
ser = pd.Series(list)
print(ser)

# 2 Escriba un programa en Pandas que permita verificar si dos series son iguales

lista1=[1,2,4,4,5]
lista2=[1,2,3,4,5]
serie1=pd.Series(lista1)
serie2=pd.Series(lista2)


x=serie1.equals(serie2)
if x==True:
    print('Las series son iguales')
else:
    print('No son iguales')
    
# 3 Escriba un programa en Pandas que seleccione las filas de un dataframe dado basado en los 
# valores en algunas columnas.


# 4 Escriba un programa en Pandas que devuelva el indice de un elemento dado por el usuario, de 
# una serie. Si la serie es [1,2,3] y el usuario desea saber la posición de 3, la cual es 2. Use una 
# serie de al menos tamaño 10.


lista1=[1,2,3,4,5,6,7,8,9,10]
serie1=pd.Series(lista1)
x=int(input('Digite el numero al que desea averiguar posicion: '))
x=7
posiciones = np.where(serie1 == x)
print(f' Posiciones del numero son: {posiciones[0]}')


# 5 Escriba un programa en Pandas que convierta un índice de una dataframe dado en una columna.

import pandas as pd
datos=[[1,2,3],
       ["a","b","c"],
       ["Felipe","Juan","María" ]
       ]
df1=pd.DataFrame(datos)
print(df1)

df2=df1.reset_index(level=None, drop=False, inplace=False, col_level=0, col_fill='')
print(df2)


# 6 Escriba un programa en Pandas que seleccione una fila específica de una serie o dataframe 
# mediante un índice entero.
datos_csv = pd.read_csv("C:/Users/cesar/OneDrive/Escritorio/Universidad/ESTADÍSTICA COMPUTACIONAL II/Datasets/diamonds.csv")
x= 0
datos_csv[x:x+1]


# 7 Escriba un programa en Pandas que seleccione una fila específica de una serie o dataframe 
# mediante un índice entero.

# 8 Escriba un programa en Pandas que permita agregar filas a un dataframe existente y muestre los 
# datos combinados.
import pandas as pd
 
data = {'nombre': ['samuel', 'andres', 'daniel', 'moises'],
    'matematicas': [68, 74, 77, 78],
    'fisica': [84, 56, 73, 69],
    'calculo': [78, 88, 82, 87]}
 
     
# crear dataframe
df_data= pd.DataFrame(data)
print('data frame original\n------------------')
print(df_data)
 
df_fila = {'nombre':'paulos', 'matematicas':87, 'fisica':92, 'calculo':97}
 
# Añadiendo una fila al dataframe
df_data= df_data.append(df_fila, ignore_index=True)
 
print('\n\nNueva Fila Añadida al DataFrame\n--------------------------')
print(df_data)



# 9 Escriba un programa en Pandas que combine dos objetos de tipo dataframe de forma que 
# rellene los valores nulos con valores no nulos del otro dataframe.



# 10 Escriba un programa en Pandas para determinar los detalles de consumo de alcohol para el año 
# 1986, donde la región sea “Western Pacific” y donde el país sea Vietnam. Use los siguientes 
# datos:

data = {'year': [1986, 1986, 1985,1986,1987],
    'who region': ['western pacific','americas','africa', 'americas','americas'],
    'country': ['viet nam', 'uruguay', 'cte divoire','colombia','saint kitts an nevis'],
    'types': ['wine','other','wine', 'beer','beer'],
    'display value':[0.00,0.50,1.62,4.27,1.98]
    }
    
df_data= pd.DataFrame(data)
print('data frame\n------------------')
print(df_data)


df_data.query('(year == 1986) & ( country == "viet nam")')

# 11 Con los mismos datos del punto anterior. Escriba un programa en Pandas para determinar los 
# detalles de consumo de alcohol para el año 1986 o 1989, donde la región sea América.



# 12 Escriba un programa en Pandas que agrupe el dataset dado por mes y año sobre la variable 
# “order date” y encuentre el monto total de compra.

data = {'ord_no': [70001, 70009, 70002,70004,70007,70005,70008,70010,70003,70012,70011,70013],
    'purch_amt': [150.5,270.65,65.26,110.5, 948.5,2400.6, 5760,1983.43,2480.4,250.45,75.29,3045.6],
    'ord_date': ["2012-10-05", "2012-09-10", "2012-10-05","2012-08-17","2012-09-10","2012-07-27","2012-09-10","2012-10-10","2012-10-10","2012-06-27","2012-08-17","2012-04-25"],
    'customer_id': [3005,3001,3002,3009,3005,3007,3002,3004,3009,3008,3003,3002],
    'salesman_id':[5002,5005,5001,5003,5002,5001,5001,5006,5003,5002,5007,5001]
    }
    df_data= pd.DataFrame(data)
print('data frame\n------------------')
print(df_data)

df_data['ord_date'] = pd.to_datetime(data['ord_date']) 
  
print(type(df_data.ord_date[0]))
df_data.sort_values(by='ord_date') 

Total = df_data['purch_amt'].sum()
print ("Column purch_amt sum:",Total)

# 13 Escriba un programa en Pandas que determine el promedio, el mínimo y el máximo de la altura
# (height), agrupado por clase.


# 14 Escriba un programa en Pandas para contar la cantidad de valores nulos en cada columna del 
# siguiente dataset.

data = {'ord_no': [70001,None , 70002,70004,None,70005,None,70010,70003,70012,None,70013],
    'purch_amt': [150.5,None,65.26,110.5, 948.5,None, 5760,1983.43,None,250.45,75.29,3045.6],
    'ord_date': ["2012-10-05", "2012-09-10", None,"2012-08-17","2012-09-10","2012-07-27","2012-09-10","2012-10-10","2012-10-10","2012-06-27","2012-08-17","2012-04-25"],
    'customer_id': [3005,3001,3002,3009,3005,3007,3002,3004,3009,3008,3003,3002],
    'salesman_id':[5002,5005,5001,None,5002,5001,5001,None,5003,5002,5007,None]
    }
    df_data= pd.DataFrame(data)
print('data frame\n------------------')
print(df_data)



Total = (pd.isna(df_data['ord_no'])*1).sum()
print ("Nulos en ord_no:",Total)

Total = (pd.isna(df_data['purch_amt'])*1).sum()
print ("Nulos en purch_amt:",Total)

Total = (pd.isna(df_data['ord_date'])*1).sum()
print ("Nulos en ord_date:",Total)

Total = (pd.isna(df_data['customer_id'])*1).sum()
print ("Nulos en customer_id:",Total)

Total = (pd.isna(df_data['salesman_id'])*1).sum()
print ("Nulos en salesman_id:",Total)

# 15 Escriba un programa en Pandas para contar la cantidad de valores nulos en el dataframe dado:


# 16 Escriba un programa en Pandas que cree una tabla Pivot que determine los sobrevivientes 
# totales para una de las clases de cada grupo. (Usar dataset Titanic).
datos_csv = pd.read_csv("C:/Users/cesar/OneDrive/Escritorio/Universidad/ESTADÍSTICA COMPUTACIONAL II/Datasets/titanic.csv")


print(list(datos_csv))

tabla = pd.pivot_table(datos_csv,index='pclass',values='survived')
print(tabla)











