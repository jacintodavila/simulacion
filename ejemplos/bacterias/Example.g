TITLE
Crecimiento exponencial de bacterias.

Problema: La población de bacterias crece proporcionalmente al número presente.
Después de 3h hay 400 bacterias, después de 10h hay 2000 bacterias.
¿Cuál era el número inicial?

NETWORK

  BacteriaGrowth (C) {  /*Ecuación de crecimiento exponencial*/
    n'= k*n;  /* dN/dt = k*N */
  }
  
  Results (A) { /*GRÁFICA N==f(T) */
    IT(1);
    GRAPH("Crecimiento de Bacterias",TIME,"Tiempo (h)",n,"Bacterias");
  }

INIT
  tsim=15;  /* Simular 15 horas */
  ACT(Results,0); 
  ACT(BacteriaGrowth,0);
  
  /* Calculamos k usando los datos conocidos:
     En t=3h, N=400
     En t=10h, N=2000
     
     N(t) = N0 * exp(k*t)
     400 = N0 * exp(k*3)
     2000 = N0 * exp(k*10)
     
     Dividiendo: 2000/400 = exp(k*7)
     5 = exp(k*7)
     k = ln(5)/7 ≈ 0.2297
  */
  k = 0.2297;  /* Constante de crecimiento */
  
  DT(BacteriaGrowth,0.01);

  /* Para encontrar N0:
     400 = N0 * exp(0.2297*3)
     N0 = 400 / exp(0.2297*3) ≈ 200
  */
  n0 = 200;  /* Número inicial de bacterias */
  n = n0;
  
  /* Parámetros modificables */
  PARAM(n0,200,"Bacterias iniciales");
  PARAM(k,0.2297,"Constante de crecimiento");

DECL
  REAL k, n0;
END