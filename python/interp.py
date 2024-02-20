import numpy as np 
from matplotlib import pyplot as plt

# Armo alguna funcion discreta
x = np.linspace(0., 5.) 
y = x**2 

# Interpolo algun valor
x1 = 2.4
y1 = np.interp(x1, x, y)

# Grafico para ver que este bien
fig, ax = plt.subplots() 
ax.plot(x, y) 
ax.plot(x1, y1, lw=0, marker='s')
plt.show()

# Lo mismo con un conjunto de valores 
x2 = np.array([0.5, 1.5, 3.5]) 
y2 = np.interp(x2, x, y)
fig, ax = plt.subplots() 
ax.plot(x, y) 
ax.plot(x2, y2, lw=0, marker='s')
plt.show()

# Ahora me fijo al reves, teniendo un y me fijo el x 
y3 = 8. 
x3 = np.interp(y3, y, x)
fig, ax = plt.subplots() 
ax.plot(x, y) 
ax.plot(x3, y3, lw=0, marker='s')
plt.show()

# Y me fijo que pasa si una funcion me da varios valores 
PI = np.pi
x = np.linspace(0., 4.*PI)
y = np.sin(x)
y4 = 0.5
x4 = np.interp(y4, y, x)
fig, ax = plt.subplots() 
ax.plot(x, y) 
ax.plot(x4, y4, lw=0, marker='s')
plt.show()
# Da cualquier cosa! Por algun motivo toma el ultimo valor de x
# Reviso recorriendo al reves
x4 = np.interp(y4, y[::-1], x[::-1])
fig, ax = plt.subplots() 
ax.plot(x, y) 
ax.plot(x4, y4, lw=0, marker='s')
plt.show()
# Mal! Hace lo mismo pero ahora toma el primer valor de x (porque esta al reves nada mas)
