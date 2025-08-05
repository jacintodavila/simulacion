Banco de Pruebas del Método de Euler 
================================================================================

Este programa en Python implementa y visualiza el Método de Euler, una técnica
numérica fundamental para aproximar soluciones de Ecuaciones Diferenciales
Ordinarias (EDOs). Su propósito principal es visualizar una comparación entre
las soluciones numéricas aproximadas con las soluciones analíticas reales.

Este script está directamente relacionado con la Sección 8.5: "Método de Euler"
del libro de texto del curso, "Cálculo: Trascendentes Tempranas" de Zill.

MODELOS INCLUIDOS
------------------
El programa contiene un "banco de pruebas" con varias EDOs importantes del
Capítulo 8, cada una con su solución analítica para comparación:

0.  Crecimiento Exponencial (dy/dx = y): El caso más fundamental de una
    tasa de cambio proporcional a la cantidad actual.

1.  Ecuación Lineal Simple (dy/dx = x + y): Introduce la dependencia de la
    pendiente tanto en 'x' como en 'y'.

2.  Ecuación Separable con Asíntota (dy/dx = y²): Muestra cómo el método
    maneja soluciones que "explotan" o tienden a infinito en un punto.

3.  Ecuación de un Círculo (dy/dx = -x/y): Interesante para visualizar cómo
    el método se aproxima a una curva cerrada y cómo maneja pendientes que se
    vuelven verticales.

4.  Crecimiento Logístico (dy/dx = y(2-y)): Un modelo crucial en ecología
    que muestra cómo una población se estabiliza al alcanzar una capacidad de
    carga (la famosa curva en "S").

5.  Oscilador Forzado (dy/dx + y = sin(x)): Un modelo clásico de la física
    que muestra un comportamiento transitorio que decae y una solución
    oscilatoria estable.

REQUISITOS
----------
- Python 3 instalado.
- Las siguientes librerías de Python:
    numpy
    matplotlib

Puedes instalar las librerías necesarias con este comando en la terminal o consola:

    pip install numpy matplotlib

CÓMO EJECUTAR EL PROGRAMA
--------------------------
1.  Guarda el código Python en un archivo con el nombre: `euler.py`
2.  Abre una terminal (o Símbolo del sistema en Windows).
3.  Navega a la carpeta donde guardaste el archivo.
4.  Ejecuta el programa con el comando:

    python euler.py

Al ejecutarse, se abrirá una ventana mostrando el gráfico de la simulación.

CÓMO EXPERIMENTAR
-----------------
Este script está diseñado para ser un laboratorio virtual. Para probar los
diferentes modelos:

1.  Abre el archivo `euler.py` en un editor de texto.
2.  Busca la sección "SELECCIÓN DEL MODELO A SIMULAR".
3.  Cambia el número de la variable `INDICE_DE_ECUACION_A_PROBAR` por el
    índice del modelo que deseas visualizar (los índices van del 0 al 5,
    como se describe en la lista de "MODELOS INCLUIDOS").
4.  Guarda el archivo y ejecútalo de nuevo.

También puedes experimentar cambiando los parámetros de la simulación, como
la condición inicial `y0` (x0 = 0 fijo para esta implementación) o los tamaños de paso `h1` y `h2` para ver cómo
afectan la precisión y el resultado.

¿QUÉ MUESTRA EL GRÁFICO?
-------------------------
- Línea Negra (Sólida): Representa la "verdad absoluta", la solución
  analítica exacta de la ecuación diferencial.

- Línea Roja (Punteada con círculos): Es la aproximación del Método de
  Euler usando un tamaño de paso grande (h = 0.5). Es una aproximación
  "tosca".

- Línea Verde (Discontinua con cruces): Es la aproximación usando un
  tamaño de paso más pequeño (h = 0.1). Es una aproximación "más fina".

La lección visual clave es que la línea verde sigue mucho más de cerca a la
línea negra, demostrando el principio fundamental de la simulación numérica:
Un tamaño de paso (h) más pequeño generalmente conduce a una mayor precisión,
a costa de un mayor número de cálculos.

DETALLES INTERNOS DEL CÓDIGO
-----------------------------
- `ecuaciones` (lista de diccionarios): Contiene toda la información de
  cada modelo (su nombre, la función de la EDO, su solución analítica y
  parámetros recomendados para la visualización).

- `metodo_euler()` (función): Es el corazón del script. Implementa el
  bucle iterativo `y_n+1 = y_n + h * F(x_n, y_n)` para calcular los puntos
  de la solución aproximada.

- Sección de Visualización: Utiliza `matplotlib.pyplot` para dibujar las
  tres curvas (la real y las dos aproximaciones) en el mismo gráfico,
  facilitando la comparación visual del error.
