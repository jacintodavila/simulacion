#use un software de computadora para
#obtener un campo de dirección para la ecuación diferencial dada.
#dy/dx = x + y; a) y(0) = -1, b) y(3) = 0

import numpy as np
import matplotlib.pyplot as plt

# Definimos el tamaño de la malla
x = np.linspace(-4, 4, 20)
y = np.linspace(-4, 4, 20)

# Creamos la malla de coordenadas (X, Y)
X, Y = np.meshgrid(x, y)

# Definimos la ecuación diferencial dy/dx = x + y
dy = X + Y
dx = np.ones_like(dy)  # Para que las líneas de pendiente tengan orientación horizontal

# Normalizamos los vectores para que todos tengan la misma longitud visual
magnitude = np.sqrt(dx**2 + dy**2)
dx_norm = dx / magnitude
dy_norm = dy / magnitude

# Dibujamos el campo de dirección
plt.quiver(X, Y, dx_norm, dy_norm, color='blue', angles='xy')

# Dibujamos las condiciones iniciales
plt.plot(0, -1, 'ro', label='y(0) = -1')
plt.plot(3, 0, 'go', label='y(3) = 0')

# Configuración del gráfico
plt.title("Campo de dirección para dy/dx = x + y")
plt.xlabel("x")
plt.ylabel("y")
plt.grid(True)
plt.legend()
plt.xlim(-4, 4)
plt.ylim(-4, 4)
plt.gca().set_aspect('equal')  # Para que las pendientes se vean correctamente
plt.show()
