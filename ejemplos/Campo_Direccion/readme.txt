Visualización del Campo de Dirección para la Ecuación Diferencial dy/dx = x + y
================================================================================

Este programa genera un campo de dirección (campo vectorial) para visualizar 
gráficamente la solución de la ecuación diferencial:

    dy/dx = x + y

También se incluyen dos condiciones iniciales:
    a) y(0) = -1
    b) y(3) = 0

REQUISITOS
----------
- Python 3 instalado
- Las siguientes librerías de Python:

    numpy
    matplotlib

Puedes instalar las librerías necesarias con este comando en la terminal o consola:

    pip install numpy matplotlib

CÓMO EJECUTAR EL PROGRAMA
0. Configurar env, digamos ejemplo, y ejecutar
   
   source ./ejemplo/bin/activate
--------------------------
1. Guarda el código en un archivo con nombre: campo_direccion.py
2. Abre la terminal (o símbolo del sistema en Windows).
3. Navega a la carpeta donde guardaste el archivo.
4. Ejecuta el programa con:

    python campo.py

Al ejecutarse, se abrirá una ventana con el gráfico del campo de dirección.

¿QUÉ MUESTRA EL GRÁFICO?
-------------------------
- Flechas azules: representan la dirección de la pendiente dy/dx = x + y.
- Punto rojo: representa la condición inicial y(0) = -1.
- Punto verde: representa la condición inicial y(3) = 0.

DETALLES INTERNOS DEL CÓDIGO
-----------------------------
- Se usa una malla de puntos (x, y) para calcular las pendientes locales.
- Cada flecha muestra la dirección de crecimiento de la función solución.
- Las pendientes están normalizadas para visualización uniforme.
- Se usa 'quiver' de matplotlib para dibujar el campo de dirección.
- Como desnormalizar las flechas? Usar: 
    magnitude = dx**2 + dy**2
  en lugar de:
    magnitude = np.sqrt(dx**2 + dy**2)
