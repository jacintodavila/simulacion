TITLE
    Enfriamiento Newton
    Una barra metalica se saca de un horno cuya temperatura es 150 grados Celsius 
    y se coloca en un tanque de agua cuya temperatura se mantiene a 30 grados Celsius constantes. 
    Despues de 0.25 horas en el tanque, la temperatura de la barra es 90 grados Celsius. 
    
NETWORK
    Cooling (C)
    {
        // Ecuacion diferencial
        t' = -k * (t - T_amb);
    }

    // Grafica y resultados
    Results (A)
    {
        IT(0.05);                
        GRAPH("Enfriamiento de la Barra", TIME, "t (horas)", t, "Temperatura (°C)");
    }

INIT
    TSIM = 1.2;               // simular un poco mas de una hora
    ACT(Results, 0);
    ACT(Cooling, 0);

    // Parametros Editables con valores por defecto obtenidos en la solucion analitica
    PARAM(k, 2.7726, "Constante de Enfriamiento k (1/min)");
    PARAM(T_amb, 30.0, "Temperatura Ambiente (°C)");
    PARAM(T0, 150.0, "Temperatura Inicial (°C)");

    // Condicion Inicial
    t = T0;
    DT(Cooling, 0.001);      // paso de integracion

DECL
    REAL k, T_amb, T0;

END