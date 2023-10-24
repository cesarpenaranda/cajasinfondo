import numpy as np
import scipy.stats
import pandas as pd

# 1. De acuerdo con lo visto en clase sobre simulaciones en Python, escriba un programa en Python 
# que simule el lanzamiento de tres dados y que devuelva la cantidad de juegos ganados de acuerdo 
# con la siguiente situación: Que la suma de los tres dados sea par.

dado, probabilidad, num_dados = [1,2,3,4,5,6], [1/6, 1/6, 1/6, 1/6, 1/6, 1/6], 3
rep, ganes = 10000,0 
par = [2,4,6,8,10,12,14,16,18]
resultados = []
for i in range(rep):
    outcomes=np.random.choice(dado, size=num_dados, p=probabilidad)
    result = outcomes[0] + outcomes[1] 
    result2 = result + outcomes[2]
    resultados.append(result2)
    if result2 in par:
        ganes=ganes+1
print("En {} juegos, ud ha ganado {} veces. La decision de seguir jugando o retirse es suya". format(rep,ganes))


# 2. Con el programa escrito en el punto anterior, guarde el resultado de la suma y grafique un 
# histograma. Interpréte el resultado.

import matplotlib.pyplot as plt
intervalos = range(min(resultados), max(resultados)+2)
plt.hist(x=resultados,bins=intervalos, color = "yellow", ec="black")
plt.xticks(intervalos)


# 3. Siguiendo el ejemplo visto en clase con la baraja de cartas. Construya una baraja de 52 cartas que 
# contenga los números del 2 al 10, la J, Q , K y A para corazones, tréboles, diamantes y espadas. 
# Simule una mano de Póker y obtenga la probabilidad de:
# a. Royal Flush (A, K, Q, J y 10 de espadas).
# b. Full House (3 de un tipo y 2 de otro).
# c. Three of a Kind (2 cartas cualquiera y 3 del mismo tipo).

import random as rd

palos = ["Trebol","Diamantes","Corazones", "Picas"]
numeros = ["a",2,3,4,5,6,7,8,9,10]
rep = 8
baraja=[]
for t in numeros:
    for p in palos:
        carta = "{} de {}".format(t,p)
        baraja.append(carta)
        
for i in range(rep):
    rd.shuffle(baraja)
    jugador = []    
    for i in range(5):
        mano = baraja.pop(0)
        jugador.append(mano)
    print(jugador)    
    royal = ["a de picas","K de picas", "Q de picas","J de picas", "10 de picas"]
    ganes = 0
    if jugador[0] and jugador[1] and jugador[2] and jugador[3] and jugador[4] in royal:
        ganes = ganes + 1
print(ganes/rep)

# 4. A usted se le provee una base de datos de jaúles cuyas variables son: altura total promedio, en m 
# (HT), diámetro a la altura de pecho, en cm (DAP), altura promedio de los 10 árboles más altos, en 
# m (HD), número de árboles por hectárea (DEN), edad de la plantación, en años (EDAD) y volúmen 
# total en pie, m3 (VP). Cada observación corresponde a una parcela. 
# Con estos datos determine lo siguiente:

import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression
import math
import statistics
import numpy as np
import scipy.stats
import pandas as pd

Datos_Tarea = pd.read_excel("Datos Tarea.xlsx")
View(Datos_Tarea)
#a. Cuales variables son las más determinantes en la determinación del volúmen total en pie

'Las variables mas determinantes son HT,HD ya que observando la matriz de '
'correlaciones , estas son las que presentan mas relacion con la variable VP '

#b. Obtenga el promedio del volúmen total en pie.

print(Datos_Tarea['VP'].mean())

#c. Obtenga la correlación del volúmen total en pie con el resto de variables.

Datos_Tarea = Datos_Tarea.select_dtypes(include=['float64', 'int'])
corr_matrix = Datos_Tarea.corr(method='pearson')
corr_matrix
tt=corr_matrix['VP']
tt
#d. Interpréte el coeficiente de determinación.

x=Datos_Tarea[['HT','DAP','HD','DEN','EDAD']]
x
y=Datos_Tarea['VP']
y=y.values.reshape(-1,1)
y
model = LinearRegression()
model = LinearRegression().fit(x, y)

r_sq = model.score(x, y)
print(f"coefficient of determination: {r_sq}")

#Respuesta
'El modelo explica el 84% de la variabilidad del modelo'


