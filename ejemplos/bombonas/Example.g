TITLE
Bombonas

J. Dávila 2025

NETWORK

  RF (C) {  
    p'= c1*p/TIME;
  }
  Results (A) { 
    IT(1);
    vr =c3*p*c4;
    tv = tv + vr;
    GRAPH("Bombonas Defectuosas",TIME,"mes",p,"Defectuosos", tv, "Víctimas");
  }

INIT
  tsim=120;
  ACT(Results,0); 
  ACT(RF,0);
  c2=6000000;
  DT(RF,0.125);

  /*To modify initial conditions and graphic type*/
  PARAM(p0,100,"Initial failures");
  PARAM(c0,0.1,"Tasa de crecimiento");
  PARAM(a,0.05,"fraccion que causa tragedias");
  PARAM(b,2.0,"victimas por tragedia");
  p=p0;
  c1=c0; 
  c3=a;
  c4=b;

DECL
  REAL  c1,c2,c3,c4,p0,c0,tv,vr,a,b; 
END
1
