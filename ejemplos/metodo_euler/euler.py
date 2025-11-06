# Banco de Pruebas para la visualización del Método de Euler

import numpy as np
import matplotlib.pyplot as plt

# --- BANCO DE ECUACIONES DIFERENCIALES ---
# Cada elemento de la lista es un diccionario que define un problema a resolver.
# Para probar uno nuevo, añade un diccionario a esta lista.
ecuaciones = [
    { # Índice 0
        "nombre": "Crecimiento Exponencial (Sec. 8.3)",
        "edo": lambda x, y: y,
        "solucion": lambda x: np.exp(x),
        "label": "y = e^x",
        "y0": 1,
        "x_final_recomendado": 4,
        "ylim_top_recomendado": 60
    },
    { # Índice 1
        "nombre": "Ecuación Lineal Simple (Sec. 8.2)",
        "edo": lambda x, y: x + y,
        "solucion": lambda x: 2 * np.exp(x) - x - 1,
        "label": "y = 2e^x - x - 1",
        "y0": 1,
        "x_final_recomendado": 4,
        "ylim_top_recomendado": 90
    },
    { # Índice 2
        "nombre": "Ecuación Separable con Asíntota (Sec. 8.1)",
        "edo": lambda x, y: y**2,
        "solucion": lambda x: 1 / (1 - x),
        "label": "y = 1 / (1 - x)",
        "y0": 1,
        "x_final_recomendado": 0.9, # La solución explota en x=1, usa x_final = 0.9 para visualizar
        "ylim_top_recomendado": 15
    },
    { # Índice 3
        "nombre": "Ecuación de un Círculo (Sec. 8.1)",
        "edo": lambda x, y: -x / y if y != 0 else np.inf, # Evitar división por cero
        "solucion": lambda x: np.sqrt(4**2 - x**2),
        "label": "y = sqrt(16 - x^2)",
        "y0": 4, # Empezar en (0, 4)
        "x_final_recomendado": 3.8, # La pendiente se vuelve infinita en x=4 usa x_final = 3.8 para visualizar
        "ylim_top_recomendado": 5
    },
    { # Índice 4
        "nombre": "Crecimiento Logístico (Sec. 8.3)",
        "edo": lambda x, y: y * (2 - y), # Capacidad de carga K=2
        "solucion": lambda x: 2 / (1 + 19 * np.exp(-2 * x)),
        "label": "y = 2 / (1 + 19e^(-2x))",
        "y0": 0.1, # Empezar con una población pequeña
        "x_final_recomendado": 5,
        "ylim_top_recomendado": 3
    },
    { # Índice 5
        "nombre": "Oscilador Forzado (Sec. 8.2)",
        "edo": lambda x, y: np.sin(x) - y,
        "solucion": lambda x: 0.5 * (np.sin(x) - np.cos(x) + 3 * np.exp(-x)),
        "label": "y = 0.5(sin(x)-cos(x)) + 1.5e^(-x)",
        "y0": 1,
        "x_final_recomendado": 15,
        "ylim_top_recomendado": 2
    }
]

# --- SELECCIÓN DEL MODELO A SIMULAR ---
# Elige un número del 0 al 5 para seleccionar la ecuación de la lista de arriba.
INDICE_DE_ECUACION_A_PROBAR = 3

# Seleccionamos el diccionario de la ecuación activa
ecuacion_activa = ecuaciones[INDICE_DE_ECUACION_A_PROBAR]

# --- IMPLEMENTACIÓN DEL MÉTODO DE EULER ---
def metodo_euler(f, x0, y0, h, x_final):
    x_puntos = [x0]
    y_puntos = [y0]
    x_actual = x0
    y_actual = y0
    while x_actual < x_final:
        y_nuevo = y_actual + h * f(x_actual, y_actual)
        x_actual += h
        y_actual = y_nuevo
        x_puntos.append(x_actual)
        y_puntos.append(y_actual)
    return x_puntos, y_puntos

# --- PARÁMETROS DE LA SIMULACIÓN ---
x0 = 0
y0 = 1
x_final = 5 # Ajusta según la ecuación seleccionada
h1 = 0.5
h2 = 0.1

# --- EJECUCIÓN DEL MÉTODO ---
x_aprox1, y_aprox1 = metodo_euler(ecuacion_activa["edo"], x0, y0, h1, x_final)
x_aprox2, y_aprox2 = metodo_euler(ecuacion_activa["edo"], x0, y0, h2, x_final)
x_real = np.linspace(x0, x_final, 200)
y_real = ecuacion_activa["solucion"](x_real)

# --- VISUALIZACIÓN ---
plt.figure(figsize=(10, 6))
plt.plot(x_real, y_real, 'k-', label=f'Solución Real ({ecuacion_activa["label"]})', linewidth=2)
plt.plot(x_aprox1, y_aprox1, 'ro--', label=f'Aprox. Euler (h = {h1})', marker='o')
plt.plot(x_aprox2, y_aprox2, 'g-.', label=f'Aprox. Euler (h = {h2})', marker='x')
plt.title(f"Método de Euler para: {ecuacion_activa['nombre']}")
plt.xlabel("x")
plt.ylabel("y")
plt.grid(True)
plt.legend()
plt.ylim(bottom=-10, top=15)
plt.show()
