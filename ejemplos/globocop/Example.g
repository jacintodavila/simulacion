TITLE
Carrera Armamentista
Imperfect Collective Security
Classic Richardson Model 1960

Epstein, Joshua (1997). Nonlinear dynamics, mathematical biology and social science. Lectures Notes Volume IV. Santa Fe Institute. Studies in Sciences of Complexity. Lecture 3. 

    x'= a2*y - a1*x + gx; 
    y'= b1*x - b2*y + gy; 
    
Linear GloboCOP    

NETWORK

  RM (C) { 
   
    /* x'= - a1*x + a2*(y-x) +a3*(z-x) + gx; 
    y'= b1*(x-y) - b2*y + b3*(z-y) + gy; 
    z'= c1*(x-z) + c2*(y-z) - c3*z + gz; */
  
   /* x'= a2*y - a1*x + gx; 
    y'= b1*x - b2*y + gy;
    z'= c2*y - c3*z + gz; */
    
    /* Linear GloboCop */ 
    x'= - a1*x + a2*(y-(x+C)) +a3*(z-(x+C)) + gx; 
    y'= b1*(x-(y+C)) - b2*y + b3*(z-(y+C)) + gy; 
    z'= c1*(x-(z+C)) + c2*(y-(z+C)) - c3*z + gz;
    
  }
  Results (A) { 
    IT(1);
    GRAPH("Carrera Armamentista",TIME,"t",x,"Pais x",y,"Pais y", z,"Pais z");
  }

INIT
  tsim=300;
  ACT(Results,0); 
  ACT(RM,0);
  a1=0.1; a2=1.5; a3=1.5; b1=1; b2=0.1; b3=1; c1=0.5; c2=0.5; c3=0.1; 
  DT(RM,0.125);

  /*To modify initial conditions and graphic type*/
  PARAM(C, 1000, "GloboCOP"); 
  PARAM(gx,1000,"Rabia en x");
  PARAM(gy,1000,"Rabia en y");
  PARAM(gz,1000,"Rabia en z");
  
  x=gx;
  y=gy;
  z=gz; 

DECL
  REAL  a1,a2,a3,b1,b2,b3, c1,c2,c3, gx,gy, gz, C;
END

