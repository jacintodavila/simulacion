================================================================================
SISTEMA DE OBJETIVOS EN PROLOG
DESCRIPCIÓN
Este programa Prolog define un sistema de objetivos permanentes para una
simulación de colonización. Incluye 6 objetivos diferentes, cada uno con un
identificador único, prioridad y un conjunto de condiciones necesarias para
su cumplimiento.
Los objetivos representan las acciones que pueden realizar los colonos en
la simulación:

Búsqueda de sitio: Encontrar un lugar donde establecerse
Establecimiento: Ocupar una celda
Agricultura: Limpiar tierras y sembrar cultivos
Explotación forestal: Talar y vender madera
Mudanza: Cambiar de ubicación cuando se agotan recursos
Expansión: Ampliar las granjas existentes

REQUISITOS

SWI-Prolog (versión 8.0 o superior)
Descarga: https://www.swi-prolog.org/

Instalación por sistema operativo:

Ubuntu/Linux: sudo apt-get install swi-prolog
macOS: brew install swi-prolog
Windows: Descarga el instalador desde https://www.swi-prolog.org/

INSTALACIÓN

Guarda el código en un archivo llamado objetivos.pl
Asegúrate de que SWI-Prolog esté instalado en tu sistema

USO
OPCIÓN 1: Interfaz interactiva SWI-Prolog

Abre SWI-Prolog:
$ swipl
Carga el archivo:
?- consult('objetivos.pl').
Realiza tus consultas (ver sección de Consultas)

OPCIÓN 2: Desde línea de comandos
Abre el archivo directamente:
$ swipl -l objetivos.pl
Luego haz tus consultas en el intérprete interactivo.
OPCIÓN 3: Online (sin instalación)

Accede a: https://swish.swi-prolog.org/
Copia y pega el código en el editor
Escribe tus consultas en la sección "Query"

CONSULTAS DISPONIBLES
CONSULTAS BÁSICAS
Obtener un objetivo por ID (0-5):
?- goal(Name, 0, Conditions).
Ver todos los objetivos:
?- goal(Name, ID, Conditions).
Presiona ; para ver el siguiente, o . para terminar.
CONSULTAS INTERMEDIAS
Buscar objetivos con una condición específica:
?- goal(Name, ID, Cond), member('Celda no vigilada', Cond).
Ver un objetivo específico por nombre:
?- goal(establecerse, ID, Cond).
Contar el total de objetivos:
?- findall(X, goal(X, _, _), L), length(L, N), write(N).
CONSULTAS AVANZADAS
Listar todos los objetivos con formato legible:
?- goal(Name, ID, Cond),
format('Objetivo ~w (~w): wn', [Name, ID, Cond]), fail.
Ver todas las condiciones de un objetivo específico:
?- goal(mudarse, _, Conditions),
member(C, Conditions), write(C), nl, fail.
Verificar si una condición existe:
?- has_condition(2, 'Celda no vigilada').
Ver los objetivos con prioridad mayor a 2:
?- goal(Name, ID, _), ID > 2, write(Name), nl, fail.
OBJETIVOS DISPONIBLES
ID | Nombre              | Condiciones
---+---------------------+-------------------------------------------
0 | Buscar Sitio        | No establecido
1 | Establecerse        | Celda desocupada
|                     | Celda no vigilada
|                     | Celda apta para establecerse
2 | Limpiar y Sembrar   | Celda desocupada
|                     | Celda no vigilada
|                     | Celda apta para sembrar
3 | Talar y Vender      | Celda desocupada
|                     | Celda no vigilada
|                     | Celda con madera comercial
4 | Mudarse             | Se agotaron los recursos
5 | Expandirse          | Celda desocupada
|                     | Celda no vigilada
|                     | Celda apta para expandir
PREDICADOS DISPONIBLES
goal(Name, ID, Conditions)
Recupera un objetivo específico con su nombre, ID y lista de condiciones.
Ejemplo:
?- goal(establecerse, 1, C).
C = ['Celda desocupada', 'Celda no vigilada', 'Celda apta para establecerse'].
get_goal(GoalId, Name, Priority, Conditions)
Obtiene un objetivo completo incluyendo su prioridad.
Ejemplo:
?- get_goal(1, Name, Priority, Conditions).
all_goals(Goals)
Obtiene una lista con todos los objetivos definidos.
Ejemplo:
?- all_goals(G).
has_condition(GoalId, Condition)
Verifica si un objetivo tiene una condición específica.
Ejemplo:
?- has_condition(3, 'Celda con madera comercial').
CONSEJOS ÚTILES

Usa fail al final de una consulta para forzar el backtracking
y ver todos los resultados
Presiona ; después de un resultado para ver alternativas
Usa halt. para salir del intérprete de Prolog
Los nombres de objetivos deben estar en minúsculas con guiones bajos
Las condiciones deben estar entre comillas simples
Para borrar el buffer, usa: clear.
Para ver el historial de comandos, usa: history.

TROUBLESHOOTING
PROBLEMA: "Archivo no encontrado"
SOLUCIÓN:

Asegúrate de que objetivos.pl esté en el directorio actual
Usa la ruta completa: consult('/ruta/completa/objetivos.pl')

PROBLEMA: "Undefined procedure"
SOLUCIÓN:

Verifica que el archivo se haya cargado correctamente
Recarga el archivo con: consult('objetivos.pl')

PROBLEMA: Consultas que no devuelven resultados
SOLUCIÓN:

Verifica la sintaxis de la consulta
Asegúrate de que los valores que buscas coincidan exactamente
(mayúsculas/minúsculas)