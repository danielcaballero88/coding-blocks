"""
Forma de conectar dos conjuntos de elementos
"""


import numpy as np 

"""
Conectividad de a indica para cada elemento de a (dado por el indice)
cuales elementos de b le corresponden
"""
a_con = [ 
         [1,2,3],
         [4,5],
         [6,7,8,9]
    ]

"""
Conectividad de b, es la conectividad traspuesta de a 
En este caso una lista es dificil de armar, conviene un
diccionario
"""
a_conT = {}
for el, con in enumerate(a_con):
    for seg in con:
        if seg in a_conT:
            a_conT[seg].append(el)
        else:
            a_conT[seg] = [el]


"""
En todo caso se puede armar una lista sabiendo que los
elementos de b son en orden creciente y no falta ninguno
"""
b_con_lol = [[]]*10
# TODO: para seguir necesito obtener maximo valor de a_con