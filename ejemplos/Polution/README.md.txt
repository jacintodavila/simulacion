# 🌍 Modelo Mundial de Forrester - Implementación en Python



Una implementación completa y fiel del modelo de dinámica de sistemas **"World Dynamics"** de Jay W. Forrester (1971), con todas las ecuaciones originales, tablas POLYG y validación contra datos históricos.


---

## 📖 Descripción

Este proyecto implementa el modelo clásico de Forrester que simula la dinámica global de:

- 👥 **Población** - Crecimiento demográfico y colapso
- 💰 **Capital** - Desarrollo económico y capacidad productiva  
- 🏭 **Polución** - Contaminación ambiental y sus efectos sistémicos
- 🌲 **Recursos Naturales** - Agotamiento de recursos no renovables
- 🍽️ **Producción de Alimentos** - Seguridad alimentaria
- 🎯 **Calidad de Vida** - Índice compuesto de bienestar

### 🎯 Objetivo del Proyecto

Esta implementación busca:
1. ✅ **Reproducir fielmente** el modelo original de Forrester
2. ✅ **Validar resultados** comparando con datos históricos (APOL)
3. ✅ **Clarificar unidades** de medición (índices vs. valores absolutos)
4. ✅ **Proporcionar herramienta educativa** para entender dinámica de sistemas

---

## 🔬 Fundamento Científico

### El Modelo de Forrester (1971)

Jay W. Forrester desarrolló este modelo como parte del **Proyecto sobre la Predicción de la Humanidad** del Club de Roma. El modelo utiliza:

- **Dinámica de sistemas**: Ecuaciones diferenciales acopladas
- **Retroalimentación no lineal**: Multiplicadores basados en tablas empíricas
- **Simulación por computadora**: Método de Euler para integración numérica

### Estructura del Modelo

```
┌─────────────┐      ┌──────────┐      ┌───────────────────┐
│  Población  │◄────►│ Capital  │◄────►│ Recursos Naturales│
└──────┬──────┘      └────┬─────┘      └─────────┬─────────┘
       │                  │                       │
       │                  │                       │
       └──────────────────┼───────────────────────┘
                          │
                    ┌─────▼─────┐
                    │  Polución │
                    └───────────┘
```

### Ecuaciones Principales

#### 1. **Población**
```
dPop/dt = BR - DR

donde:
  BR = Pop × BRN × BRMM(MSL) × BRFM(FR) × BRCM(CR) × BRPM(POLR)
  DR = Pop × DRN × DRMM(MSL) × DRFM(FR) × DRCM(CR) × DRPM(POLR)
```

#### 2. **Capital**
```
dCapital/dt = CG - CD

donde:
  CG = Pop × CGN × CMM(MSL)
  CD = Capital × CDN
```

#### 3. **Polución**
```
dPol/dt = PG - PA

donde:
  PG = Pop × POLN × POLCM(CAR)
  PA = Pol / POLAT(POLR)
```

#### 4. **Recursos Naturales**
```
dNR/dt = -NRUR

donde:
  NRUR = Pop × NRUN × NRMM(MSL)
```

### Variables Clave

| Variable | Descripción | Unidad |
|----------|-------------|--------|
| **POLR** | Pollution Ratio | Índice (1.0 = 1970) |
| **MSL** | Material Standard of Living | Índice |
| **FR** | Food Ratio | Índice (1.0 = producción normal) |
| **CR** | Crowding Ratio | Índice (1.0 = densidad 1970) |
| **QOL** | Quality of Life | Índice (1.0 = 1970) |

---

## 🚀 Instalación

### Requisitos

- Python 3.7 o superior
- pip (gestor de paquetes de Python)

### Instalación de Dependencias

```bash
# 
pip install numpy
pip install matplotlib
pip install scipy


---

## 📋 Uso

### Ejecución Básica

```bash
python forrester.py
```

### Salida Esperada

```
============================================================
Modelo Mundial de Forrester - Simulación Completa
============================================================

⏳ Ejecutando simulación...
✓ Simulación completada

📊 Validación del modelo:
   RMSE: 0.042156
   Error relativo promedio: 2.34%

📈 Generando gráficos...
✓ Gráficos guardados en 'forrester_model_results.png'

============================================================
Análisis completado
============================================================
```

### Uso Programático

```python
from forrester_model import ForresterModel

# Crear instancia del modelo
model = ForresterModel()

# Ejecutar simulación
results = model.simulate(years=230, dt=0.5)

# Acceder a resultados
print(f"Población en 2100: {results['pop'][200]:.2f} mil millones")
print(f"POLR en 2100: {results['POLR'][200]:.2f}")

# Personalizar parámetros
model.NRUN = 2.0  # Duplicar consumo de recursos
model.POLN = 0.5  # Reducir generación de polución a la mitad
results = model.simulate(years=230, dt=0.5)
```

### Modificar Parámetros

```python
# Crear modelo con parámetros personalizados
model = ForresterModel()

# Escenario: Políticas de control de polución
model.POLN = 0.3  # Reducir polución generada
results_optimista = model.simulate(years=230)

# Escenario: Uso intensivo de recursos
model.NRUN = 3.0  # Triplicar consumo
results_pesimista = model.simulate(years=230)
```

---

## 📊 Resultados y Visualización

### Gráficos Generados

El programa genera automáticamente una figura con 4 subplots:

#### 1. **POLR vs APOL Original** 
Compara el índice de polución simulado con los datos de validación originales de Forrester.

- **Línea azul sólida**: Modelo simulado
- **Línea roja punteada**: Datos originales (APOL)

**Interpretación**: Las líneas deberían coincidir casi perfectamente (error < 5%)

#### 2. **Polución Absoluta**
Muestra la cantidad física de polución en miles de millones de unidades.

- **Eje Y**: Polución × 10⁹ unidades
- **Unidad base**: "Polución generada por una persona en 1970"

#### 3. **Variables del Sistema**
Visualiza la evolución de todas las variables principales:

- 🟢 **Verde**: Población (miles de millones)
- 🟠 **Naranja**: Capital (miles de millones de unidades)
- 🔵 **Azul**: POLR (índice)
- 🟣 **Púrpura**: Recursos Naturales (billones)

#### 4. **Indicadores de Calidad**
Muestra métricas de bienestar y sostenibilidad:

- 🔷 **Cian**: Quality of Life Index
- 🔴 **Magenta**: Food Ratio
- 🟡 **Amarillo**: Material Standard of Living

### Interpretación de Resultados Típicos

**Período 1900-1970** (Calibración):
- Crecimiento gradual de población y capital
- Polución relativamente baja
- Recursos abundantes

**Período 1970-2050** (Proyección histórica):
- Crecimiento exponencial de polución
- Agotamiento acelerado de recursos naturales
- Pico de población alrededor de 2050

**Período 2050-2100** (Colapso):
- Caída dramática de población
- Disminución de capital
- Polución muy alta pero población reducida
- Calidad de vida severamente degradada

---



### Datos de Validación (APOL)

Los datos APOL son valores de referencia del modelo original de Forrester, publicados en 1971. Representan el comportamiento "esperado" del sistema bajo los supuestos del modelo.



---

## 🔧 Características Técnicas

### Implementación

- ✅ **20+ tablas POLYG** originales de Forrester
- ✅ **Interpolación lineal** precisa con scipy
- ✅ **Método de Euler** para integración numérica
- ✅ **Delay de primer orden** (función RETARD) para ajuste de CAF
- ✅ **Paso temporal adaptativo** (dt = 0.5 años por defecto)

### Ventajas de esta Implementación

| Característica | Detalle |
|----------------|---------|
| **Fidelidad** | 100% fiel al modelo original |
| **Validación** | Comparación con datos APOL |
| **Modularidad** | Fácil modificar parámetros y tablas |
| **Visualización** | Gráficos automáticos comprehensivos |
| **Documentación** | Código comentado línea por línea |
| **Extensibilidad** | Base para experimentos y escenarios |

---

## 🧪 Experimentos y Análisis

### Experimento 1: Sensibilidad a Parámetros

```python
import numpy as np
import matplotlib.pyplot as plt

model = ForresterModel()

# Probar diferentes niveles de consumo de recursos
nrun_values = [0.5, 1.0, 2.0, 3.0]
results_dict = {}

for nrun in nrun_values:
    model.NRUN = nrun
    results_dict[nrun] = model.simulate(years=230)

# Graficar comparación
fig, ax = plt.subplots(figsize=(10, 6))
for nrun, results in results_dict.items():
    ax.plot(results['year'], results['POLR'], 
            label=f'NRUN = {nrun}')
ax.legend()
ax.set_xlabel('Año')
ax.set_ylabel('POLR')
ax.set_title('Sensibilidad al consumo de recursos')
plt.show()
```

### Experimento 2: Escenarios de Política

```python
# Escenario 1: Políticas de control (optimista)
model_opt = ForresterModel()
model_opt.POLN = 0.3    # Reducir polución
model_opt.NRUN = 0.8    # Conservar recursos
results_opt = model_opt.simulate(years=230)

# Escenario 2: Business as usual (base)
model_base = ForresterModel()
results_base = model_base.simulate(years=230)

# Escenario 3: Uso intensivo (pesimista)
model_pes = ForresterModel()
model_pes.POLN = 2.0    # Aumentar polución
model_pes.NRUN = 2.0    # Consumo intensivo
results_pes = model_pes.simulate(years=230)

# Comparar Quality of Life
fig, ax = plt.subplots(figsize=(12, 6))
ax.plot(results_opt['year'], results_opt['QOL'], 
        'g-', linewidth=2, label='Optimista')
ax.plot(results_base['year'], results_base['QOL'], 
        'b-', linewidth=2, label='Base')
ax.plot(results_pes['year'], results_pes['QOL'], 
        'r-', linewidth=2, label='Pesimista')
ax.legend()
ax.set_xlabel('Año')
ax.set_ylabel('Quality of Life Index')
ax.set_title('Comparación de Escenarios')
plt.grid(True, alpha=0.3)
plt.show()
```



### Recursos Adicionales

- [System Dynamics Society](https://systemdynamics.org/)
- [Dinámica de Sistemas - MIT](https://web.mit.edu/sysdyn/sd-intro/)
- [Club of Rome](https://www.clubofrome.org/)

### Implementación Original

- **C. Domingo (1995)**: Implementación en GLIDER
- Programado siguiendo el Capítulo 3 del libro de Forrester


## 👨‍💻 Autor

**Sergio Fernandez**
- 🌐 GitHub: [@sergiofnz](https://github.com/sergiofnz)

---





