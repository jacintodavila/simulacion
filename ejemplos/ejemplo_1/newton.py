# En una habitación de una casa donde la temperatura
# era constante de 70 F se encontró un cuerpo sin vida. Al
# medir la temperatura del cuerpo al momento de su descubrimiento,
# la lectura fue de 85 F. Una segunda medición,
# una hora después, mostró que la temperatura del cuerpo
# era de 80 F. Use el hecho de que si t = 0 corresponde al
# instante de fallecimiento, entonces la temperatura del
# cuerpo en ese instante era de 98.6 F. Determine cuántas
# horas transcurrieron entre el fallecimiento y el descubrimiento

import numpy as np
import matplotlib.pyplot as plt
import sympy as sp

# Variables simbólicas
k, t1 = sp.symbols('k t1', real=True, positive=True)
T_s = 70
T0 = 98.6

# Ecuación general de temperatura
T = lambda t: T_s + (T0 - T_s) * sp.exp(-k * t)

# Sistema de ecuaciones:
eq1 = sp.Eq(T(t1), 85)
eq2 = sp.Eq(T(t1 + 1), 80)

# Resolver sistema para k y t1
sol = sp.solve([eq1, eq2], (k, t1), dict=True)[0]
k_val = float(sol[k])
t1_val = float(sol[t1])

print(f"Constante k ≈ {k_val:.4f}")
print(f"Tiempo desde la muerte hasta el hallazgo ≈ {t1_val:.2f} horas")

# Simulación: valores numéricos de T(t)
t_vals = np.linspace(0, 5, 100)
T_vals = T_s + (T0 - T_s) * np.exp(-k_val * t_vals)

# Graficar
plt.figure(figsize=(10, 6))
plt.plot(t_vals, T_vals, label="Temperatura del cuerpo")
plt.axhline(70, color='gray', linestyle='--', label='Temp. ambiente (70°F)')
plt.axvline(t1_val, color='red', linestyle='--', label='Descubrimiento (~{:.2f} h)'.format(t1_val))
plt.axvline(t1_val + 1, color='orange', linestyle='--', label='1h después')
plt.scatter([t1_val, t1_val + 1], [85, 80], color='black', zorder=5)
plt.title("Simulación del enfriamiento de un cuerpo según la ley de Newton")
plt.xlabel("Tiempo desde la muerte (horas)")
plt.ylabel("Temperatura del cuerpo (°F)")
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()
