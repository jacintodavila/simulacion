TITLE
    Simular la memoria de una persona con factor de olvido.
    Para mas detalles vease la solucion analitica presentada.

NETWORK
    Learning (C) 
    {
        // Ecuacion diferencial
        a' = k1*(m - a) - (k2*a);
    }

    // Grafica y resultados
    Results (A)
    {
        IT(0.05);                
        GRAPH("Material Memorizado", TIME, "Time (horas)", a, "Unidades");
    }

INIT
    TSIM = 8;          // Simulacion de 8 horas
    ACT(Results, 0);
    ACT(Learning, 0);
  
    // Parametros Editables con valores por defecto obtenidos en la solucion analitica
    PARAM(k1, 0.4, "Tasa de aprendizaje");
    PARAM(k2, 0.1, "Tasa de olvido");
    PARAM(m, 100, "Material total a memorizar");

    // Condicion Inicial
    a = a0;
    DT(Learning, 0.0167); // Seria un paso de integracion equivalente a un minuto

DECL
    REAL k1, k2, m, a0;

END