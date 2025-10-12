% Traducción de init() de Java a Prolog
% Los objetivos permanentes se definen como hechos

% Objetivo 0: Buscar Sitio (finding a place)
goal(buscar_sitio, 0, ['No establecido']).

% Objetivo 1: Establecerse (settling down)
goal(establecerse, 1, ['Celda desocupada', 'Celda no vigilada', 'Celda apta para establecerse']).

% Objetivo 2: Limpiar y Sembrar (cleaning the land and seeding agriculture of subsistence)
goal(limpiar_sembrar, 2, ['Celda desocupada', 'Celda no vigilada', 'Celda apta para sembrar']).

% Objetivo 3: Talar y Vender (deforesting and selling wood to illegal traders)
goal(talar_vender, 3, ['Celda desocupada', 'Celda no vigilada', 'Celda con madera comercial']).

% Objetivo 4: Mudarse (moving)
goal(mudarse, 4, ['Se agotaron los recursos']).

% Objetivo 5: Expandirse (expanding settler's farms)
goal(expandirse, 5, ['Celda desocupada', 'Celda no vigilada', 'Celda apta para expandir']).

% Predicado para recuperar un objetivo por su identificador
get_goal(GoalId, Name, Priority, Conditions) :-
    goal(Name, GoalId, Conditions),
    get_priority(GoalId, Priority).

% Tabla de prioridades
get_priority(0, 0).
get_priority(1, 1).
get_priority(2, 2).
get_priority(3, 3).
get_priority(4, 4).
get_priority(5, 5).

% Predicado alternativo: obtener todos los objetivos
all_goals(Goals) :-
    findall(goal(Name, Id, Conditions), goal(Name, Id, Conditions), Goals).

% Predicado para verificar si existe una condición para un objetivo
has_condition(GoalId, Condition) :-
    goal(_, GoalId, Conditions),
    member(Condition, Conditions).