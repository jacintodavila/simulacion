TITLE
Forrester World Model.

Forrester, Jay W. WORLD DYNAMICS. Wright-Allen Press, Inc.,
1971.
See also:
Meadows Donella H. THE LIMITS TO GROWTH. Universe Books, 1974.
Meadows Donella H.Meadows Dennis L. Rander, Jorgen.
BEYOND THE LIMITS. Chelsea Green Publishing Company, 1992.

THE NUMBERS OF EQUATIONS CORRESPONDS TO PARAGRAPH NUMBERS OF
CHAPTER 3 OF FORRESTER'S BOOK. PROGRAMMED IN GLIDER.
C.DOMINGO. 1995

   This model is a classical example of GLOBAL AGGREGATED
MODEL.  Though the model is build up with variables,
expressions and Functions that have numerical values it is
different of the models in Engineering and Econometrics.
The relationships are based of course in facts, observed
relationships, and some measurements, but not in
statistical adjustments to empirical data. The method do
not claim to predict the future, but to extract
conclusions from a set of plausible relationships in a
way more complete and consistent than that attainable by
discursive reasoning applied to that set.

   If someone has different opinions about the variables
and Relations can make his or her own model. To allow
this, Forrester gives in his book all the data and
equations of the model.

   The behavior that results from the simulation run may
be Considered as a possible behavior of the system,
but more strictly it is the consequence of the set of
hypothesis included in the assumed data and
relationships. Systematic experiments and sensitivity
analysis can teach a lot about the characteristics and
behavior of the system.

   The model starts its calculations in 1900. The parameters
were adjusted to produce the correct values in 1970, the year
in which the model was made and the extrapolation begins. The
values in this year are called "normal" values. Values of
some state variables (population: pop, capital: cap, 
pollution: pol) were
divided by their values in 1970 to obtain index
(CRowding ratio, CApital Ratio, POLlution Ratio).

   The significance and influence of the state variables on
other variables are better appreciated in term of these index.

NETWORK
  World (C){ /*POPULATION. BIRTH AND DEATH RATES */
    BR=pop*BRN*BRMM(MSL)*BRFM(FR)*BRCM(CR)*BRPM(POLR);	/*2*/
    DR=pop*DRN*DRMM(MSL)*DRFM(FR)*DRCM(CR)*DRPM(POLR);	/*10*/
    pop'=BR-DR;						/*1*/
    CR=pop/(LAND*PDN);					/*CROWDING RATIO*/ /*15*/

    /*CAPITAL*/
    CG=pop*CGN*CMM(MSL);				/*CAP.GENERATION*/ /*25*/
    CD=capital*CDN;						/*CAP.DISCARD*/ /*27*/
    capital'=CG-CD;						/*24*/
    CAR=capital/pop;					/*CAP.RATIO: CAP PER PERSON*/ /*23*/
    ECR=CAR*(1-caf)*NREM(NRFR)/(1-CAFN);		/*EFFECTIVE*/ /*5*/
    MSL=ECR/ECRN;					/*EFFECTIVE RELATIVE TO 1970*/ /*4*/
    							/*IS AN INDEX OF MATERIAL STANDARD OF LIVING*/

    /*FOOD. CAPITAL FRACTION IN AGRICULTURE*/
    RETARD(1,caf,CAFT,CFFR(FR)*CQR(CAQR));		/*RAT.ADJ.*/  /*35*/
    CRA=CAR*caf/CAFN;					/*CAP.IN AGR.RELATIVE TO 1970*/ /*22*/
    FR=FC*FPC(CRA)/FN*FPM(POLR)*FCM(CR);		/*FOOD PROD.REL. */ /*19*/
    /*NATURAL RESOURCES (NOT RENEWABLE) */
    NRUR=pop*NRUN*NRMM(MSL);				/*NAT.RES.USED PER PERSON*/ /*9*/
    nr'=-NRUR;						/*NAT.RESOURCES LEFT*/ /*8*/
    NRFR=nr/NRI;					/*FRACTION OF NAT.RESOURCES LEFT*/ /*7*/

    /*POLLUTION*/
    POLR=pol/POLS;					/*POLLUTION RATIO. RELATIVE TO 1970*/ /*29*/
    PG=pop*POLN*POLCM(CAR);				/*POLLUTION GENERATION*/ /*31*/
    PA=pol/POLAT(POLR);					/*POLLUTION ABSORTION*/ /*33*/
    pol'=PG-PA;						/*30*/
    /*QUALITY OF LIFE INDEX*/
    QOL=QLS*QLM(MSL)*QLC(CR)*QLF(FR)*QLP(POLR);		/*37*/
    CAQR=QLM(MSL)/QLF(FR);				/*RATIO OF QUAL.MATERIAL/FOOD*/

    /*SCALING FOR GRAPH*/
    POPG=pop/1.0E+9;
    CAPG=capital/1.0E+9;
    NRG=nr/1.0E+12;
    APO = APOL(TIME); 
    /*GRAPH*/
    GRAPH("", TIME,"TIME",POPG,"POPG",CAPG,"CAPG",NRG,"NRG",POLR,"POLG",APO,"APOL",QOL,"QOL");
  }
INIT
  TSIM=230;      /*SIMULATION TIME*/
  ACT(World,0);   /*FIRST ACTIVATION OF WORLD*/

  /*PARAMETERS AND STANDARD (NORMAL) VALUES*/
  BRN=0.04;      /*BIRTH RATE COEFICIENT, NORMAL              */
  DRN=0.028;     /*DEATH RATE COEFICIENT, NORMAL              */
  CGN=0.05;      /*CAPITAL GROWTH RATE COEFICIENT, NORMAL     */
  CDN=0.025;     /*CAPITAL DESTRUCTION RATE COEFICIENT        */
  FR=1;          /*FOOD RATIO, NORMAL IN 1970                 */
  POLR=1;        /*POLUTION RATIO, NORMAL 1 IN 1970           */
  ECRN=1;        /*EFFECTIVE CAPITAL RATIO NORMAL             */
  LAND=135E+6;   /*TOTAL LAND Km2                             */
  PDN=26.5;      /*POPULATION DENSITY NORMAL (1970)           */
  CAFT=15.0;     /*DELAY IN ADJUSTMENT FRACTION CAPITAL IN
                 AGRICULTURE                                */
  CAFN=0.3;
  FC=1;
  FN=1;
  FR=0.3;       /*CAPITAL FRACTION IN AGRICULTURE, INITIAL    */
  POLS=3.6E+9;  /*POLLUTION IN 1970                           */
  
  /*INTERMEDIATE VARIABLES INITIAL VALUES                      */
  POLR=0.2E+9/POLS; /*INITIAL POLUTION RATIO                  */
  QLS=1.0;      /*QUALITY OF LIFE STANDARD (1970)             */
  MSL=0.25;     /*MATERIAL STANDARD OF LIVING INITIAL (1900)  */
  CAQR=2;       /*QUAL. OF LIFE RATIO INITIAL                 */
  ECR=0.25;     /*EFFECTIVE CAPITAL RATIO INITIAL             */
  NRFR=1;       /*NAT. RESOURCE FRACTION REMAINING; 1 IN 1900 */
  
  DT(World,0.5);       /*INTEGRATION INTERVAL */

  /*INTERACTIVE CHANGE OF PARAMETERS*/
  PARAM(NRUN, 1, "CONSUMPTION OF NAT.RESOURCES NORMAL SINCE 1970");
  PARAM(POLN, 1, "POLLUTION PRODUCTION/PERSON NORMAL SINCE 1970");

  /*INITIAL VALUES TIME=0  T.INITIAL 1900 */
  pop=1.65E+9;  /*POPULATION INITIAL (1900)*/
  capital=0.4E+9;   /*CAPITAL INITIAL (1900)*/
  caf=0.2;      /*CAPITAL FRACT.IN AGRICULTURE INITIAL (1900)*/
  CAR=capital/pop;  /*CAPITAL PER PERSON*/
  CRA=1;        /*CAPITAL INVESTMENT IN AGRICULTURE PER PERSON*/
  pol=0.2E+9;   /*POLUTION INITIAL (1900)*/
  CR=pop/(LAND*PDN);/*INDEX OF CROWDING VALUE==1 IN 1970 */
  NRI=900E+9;   /*NATURAL RESOURCES INITIAL: 250 YEARS AT
                  CONSUMPTION RATE OF 1970 (1 UNIT PER PERSON)*/
  nr=NRI;       /*NATURAL RESOURCES, PUT AT ITS INITIAL VALUE*/

//  INTI(BRMM,BRFM,BRCM,BRPM,DRMM,DRFM,DRCM,DRPM,CFFR);
 
DECL
  /*STATE VARIABLES.
   ACTUAL VALUE, NEW VALUE: 1. SCALED FOR GRAPH: G*/
//  CONT pop,    /* POPULATION */
//       capital,    /* CAPITAL. UNIT: CAP./PERSON IN 1970 */
//       nr,     /* NATURAL RESOURCES. UNIT: CONSUMPTION BY A PERSON IN 1970 */
//       pol;   /* POLUTION UNIT: POL.PRODUCED BY A PERSON IN 1970 */
//  RET caf[3]; /* CAPITAL FRACTION IN AGRICULTURE */

  /*INTERMEDIATE VARIABLES*/
  REAL POPG, /*pop FOR GRAPH*/
       CAPG, /*capital FOR GRAPH*/
       NRG,  /*nr  FOR GRAPH*/
       BR,   /* BIRTH RATE. NUMBER OF PEOPLE BORN IN A UNIT TIME (YEAR)*/
       DR,   /* DEATH RATE. NUMBER OF PEOPLE DEAD IN A UNIT TIME (YEAR)*/
       CG,   /* CAPITAL GENERATION PER UNIT TIME */
       CD,   /* CAPITAL DEPLETION PER UNIT TIME */
       CAR,  /* CAPITAL RATIO */
       NRFR, /*NATURAL RESOURCES FRACTION REMAINING */
       NRUR, /*NATURAL RESOURCES USE RATE */
       NRI,  /*NATURAL RESOURCES, INITIAL */
       MSL,  /* MATERIAL STANDARD OF LIVING. INDEX OF CAP. CORRECTED BY nr.*/
       FR,   /* FOOD COEFICIENT. INDEX OF FOOD PRODUCTION BY PERSON */
       CRA,  /* CAPITAL.IN AGRICULTURE UNIT: CAP IN AGR/PERSON IN 1970 */
       CR,   /* CROWDING RATIO.  INDEX OF DENSITY OF POP. 1 IN 1970 */
       ECR,  /* EFECTIVE CAPITAL. CORRECTED BY NR DEPLETION (==MSL IN VALUE )*/
       PG,   /* POLLUTION GENERATION RATE IN A UNIT TIME (YEAR) */
       PA,   /* POLLUTION ABSORTION RATE IN A UNIT TIME (YEAR) */
       POLR, /* POLUTION RATIO.   INDEX OF POLLUTION. 1 IN 1970 */
       APO,  /* ACTUAL POLLUTION */
       QOL,  /* QUALITY OF LIFE INDEX */
       CAQR, /* RATIO OF FOOD TO MAT.LIFE QUALITY OF LIFE*/

  /*PARAMETERS*/
       BRN,    /* BIRTH RATE NORMAL */
       DRN,    /* DEATH RATE NORMAL */
       LAND,   /* TOTAL LAND AVAILABLE */
       PDN,    /* POPULATION DENSITY INITIAL */
       CGN,    /* CAPITAL GROWTH RATE NORMAL */
       CDN,    /* CAPITAL DEPLETION RATE NORMAL */
       CAFN,   /* CAPITAL FRACTION IN AGRICULTURE, NORMAL */
       CAFT,   /* AJUSTMENT DELAY OF FRACTION OF AGRICULTURE CAPITAL */
       FC,     /* FOOD COEFICIENT TO EXPERIMENT WITH FR */
       FN,     /* FOOD NORMAL */
       ECRN,   /* EFECTIVE CAPITAL, NORMAL*/
       NRUN,   /* NATURAL RESOURCES USED PR PERSON, NORMAL*/
       POLS,   /* POLLUTION, INITIAL */
       POLN,   /* POLLUTION NORMAL VALUE */
       QLS;    /* QUALITY OF LIFE, STANDARD*/
       
  /*MULTIPLIERS*/
  GFUNCTIONS
    POLYG BRMM() =             /*BIRTH RATE(MATERIAL STD. OF LIVING) */
        (0,1.2)(1,1.0)(2,0.85)(3,0.75)(4,0.7)(5,0.7)(7,0.7)(7.01,0),       /*3*/
    POLYG BRFM() =                           /*BIRTH RATE(FOOD RATIO)*/
        (0,0)(1,1)(2,1.6)(3,1.9)(4,2.0)(10,2.0),                          /*17*/
    POLYG BRCM() =                       /*BIRTH RATE(CROWDING RATIO)*/
        (0,1.05)(1,1.0)(2,0.9)(3,0.7)(4,0.6)(5,0.55)(10,0.55)(10.01,0),	  /*16*/
    POLYG BRPM() =                     /*BIRTH RATE(POLLUTION RATIO) */
        (0,1.02)(10,0.9)(20,0.7)(30,0.4)(40,0.25)(50,0.15)(60,0.1)
        (200,0.1)(200.01,0),					          /*18*/

    POLYG DRMM() =              /*DEATH RATE(MATERIAL STD. OF LIVING)*/
        (0,3.0)(0.5,1.8)(1.0,1.0)(1.5,0.8)(2.0,0.7)(2.5,0.6)(3.0,0.53)
        (3.5,0.5)(4.0,0.5)(4.5,0.5)(5.0,0.5)(7,0.5)(7.01,0),              /*11*/
    POLYG DRFM() =                          /*DEATH RATE(FOOD RATIO) */
        (0.0,30)(0.25,3)(0.50,2)(0.75,1.4)(1.0,1.0)(1.25,0.7)(1.50,0.6)	
        (1.75,0.50)(2.0,0.50)(3.0,0.50)(3.01,0),                          /*13*/ 
    POLYG DRCM() =                      /*DEATH RATE(CROWDING RATIO) */
        (0,0.9)(1,1)(2,1.2)(3,1.5)(4,1.9)(5,3)(7,3)(7.01,0),
    POLYG DRPM() =                      /*DEATH RATE(POLLUTION RATIO)*/
        (0,0.92)(10,1.3)(20,2.0)(30,3.2)(40,4.8)(50,6.8)(60,9.2)
        (200,9.2)(200.01,0),                                              /*12*/

    POLYG CFFR() =                               /*CAP.FRAC.ADEQ(FOOD RATIO)  */
        (0.0,1.0)(0.5,0.6)(1.0,0.3)(1.5,0.15)(2.0,0.1)(3.0,0.1)(3.01,0),  /*36*/
    POLYG NREM() =                   /*NAT.RESOURCES (FRAC.OF NAT.RES.REMAIN.)*/
        (0.0,0.0)(0.25,0.15)(0.50,0.50)(0.75,0.85)(1.0,1.0)(1000,1),       /*6*/
    POLYG CMM() =                               /*CAP(MATERIAL STD. OF LIVING)*/
        (0.0,0.1)(1.0,1.0)(2.0,1.8)(3.0,2.4)(4.0,2.8)(5.0,3.0)(1000,3.0), /*26*/
    POLYG FCM() =                               /*FOOD FRACTION(CROWDING RATIO*/
        (0,2.4)(1,1)(2,0.6)(3,0.4)(4,0.3)(5,0.2)(1000,0.2),               /*20*/
    POLYG FPM() =                              /*FOOD FRACTION(POLLUTION RATIO*/
        (0,1.02)(10,0.9)(20,0.65)(30,0.35)(40,0.2)(50,0.1)(60,0.05)
        (1000,0.05),                                                      /*28*/
    POLYG FPC() = 
        (0,0.5)(1,1)(2,1.4)(3,1.7)(4,1.9)(5,2.05)(6,2.2)(1000,2.2),       /*21*/ 

    POLYG NRMM() =                   /*CONSU.OF RESO.(MATERIAL STD.OF LIVING) */
        (0,0.0)(1,1.0)(2,1.8)(3,2.4)(4,2.9)(5,3.3)(6,3.6)(7,3.8)
        (8,3.9)(9,3.95)(10,4.0)(1000,4.0),                                /*42*/   
    POLYG POLCM() =                        /*POLLUTION(MATERIAL STD.OF LIVING)*/
        (0,0.05)(1,1)(2,3)(3,5.4)(4,7.4)(5,8.0)(1000,8.0),                /*32*/
    POLYG POLAT() =                 /*POLLUTION ABSORBTION (POLLUTION RATIO)  */
        (0,0.6)(10,2.5)(20,5.0)(30,8.0)(40,11.5)(50,15.5)(60,20.0)
        (1000,20.0),                                                      /*34*/

    POLYG QLM() =                       /*QUAL.OF LIFE(MATERIAL STD.OF LIVING)*/
        (0,0.2)(1,1.0)(2,1.7)(3,2.3)(4,2.7)(5,2.9)(1000,2.9),             /*38*/
    POLYG QLC() =                                     /*QUAL.OF LIFE(CROWDING)*/
        (0,2)(0.5,1.3)(1,1.0)(1.5,0.75)(2,0.55)(2.5,0.45)(3,0.38)(3.5,0.3)
        (4,0.25)(4.5,0.22)(5,0.20)(1000,0.20),                            /*39*/
    POLYG QLF() =                                  /*QUAL.OF LIFE(FOOD RATIO) */
        (0,0)(1,1)(2,1.8)(3,2.4)(4,2.7)(1000,2.7),                        /*40*/
    POLYG QLP() =                             /*QUAL.OF LIFE(POLLUTION RATIO) */
        (0,1.04)(10,0.85)(20,0.60)(30,0.30)(40,0.15)(50,0.05)
        (60,0.02)(1000,0.02),                                             /*42*/
    POLYG CQR() =                   /*ADJUST. OF CAP.AGR (QUAL.LIFE RATIO)    */
        (0.0,0.7)(0.5,0.8)(1.0,1.0)(1.5,1.5)(2,2)(1000,2),                /*43*/
            
    POLYG APOL() =                   /*VALIDATION ACTUAL POLUTION    */
        (	0	,	0.00529345794392538	)
(	1	,	0.0106032249572195	)
(	2	,	0.0160760826642054	)
(	3	,	0.0218448334868925	)
(	4	,	0.0280376201132008	)
(	5	,	0.0347243385546963	)
(	6	,	0.0417698565223127	)
(	7	,	0.0491089377385751	)
(	8	,	0.0566670264578041	)
(	9	,	0.0643532578649531	)
(	10	,	0.0721187047518695	)
(	11	,	0.0800752007371257	)
(	12	,	0.0883671975779906	)
(	13	,	0.0970249835461452	)
(	14	,	0.10596701329472	)
(	15	,	0.115137370014486	)
(	16	,	0.124435869422149	)
(	17	,	0.133743688298014	)
(	18	,	0.14291404501778	)
(	19	,	0.151795498222991	)
(	20	,	0.160304172699743	)
(	21	,	0.168523943661963	)
(	22	,	0.176564314861122	)
(	23	,	0.184539449782804	)
(	24	,	0.192530893773856	)
(	25	,	0.200575924707126	)
(	26	,	0.208758417796496	)
(	27	,	0.217115650914836	)
(	28	,	0.225670922732663	)
(	29	,	0.234347347637219	)
(	30	,	0.243035421877056	)
(	31	,	0.251716506515724	)
(	32	,	0.26038361195208	)
(	33	,	0.269022758983808	)
(	34	,	0.277617638541543	)
(	35	,	0.286119323417149	)
(	36	,	0.294446268263786	)
(	37	,	0.302554205607477	)
(	38	,	0.310391878373037	)
(	39	,	0.317765907595093	)
(	40	,	0.324426997499019	)
(	41	,	0.330170119784135	)
(	42	,	0.334888100566005	)
(	43	,	0.338557641174159	)
(	44	,	0.341407068579696	)
(	45	,	0.343760234303014	)
(	46	,	0.345994576806635	)
(	47	,	0.348357061998154	)
(	48	,	0.351103975253388	)
(	49	,	0.354433355271822	)
(	50	,	0.358412768197967	)
(	51	,	0.363011925760164	)
(	52	,	0.368282085033574	)
(	53	,	0.374297801763855	)
(	54	,	0.381133631696729	)
(	55	,	0.388817533236799	)
(	56	,	0.39732853758063	)
(	57	,	0.406887982098201	)
(	58	,	0.425973352558411	)
(	59	,	0.44834007626514	)
(	60	,	0.464882132339907	)
(	61	,	0.483055095351612	)
(	62	,	0.496102350847196	)
(	63	,	0.509382593048085	)
(	64	,	0.520798941606706	)
(	65	,	0.546427479187337	)
(	66	,	0.572056016767968	)
(	67	,	0.586268205789953	)
(	68	,	0.6093338896125	)
(	69	,	0.640088134709253	)
(	70	,	0.666415632405724	)
(	71	,	0.683423661891028	)
(	72	,	0.717672707566962	)
(	73	,	0.751688766537617	)
(	74	,	0.767531862496542	)
(	75	,	0.796189227245795	)
(	76	,	0.81878893765778	)
(	77	,	0.863522385071238	)
(	78	,	0.89357767005215	)
(	79	,	0.943436824981729	)
(	80	,	0.982578591468505	)
(	81	,	1.00937206257551	)
(	82	,	1.03267073310336	)
(	83	,	1.07554028687458	)
(	84	,	1.10419765162383	)
(	85	,	1.14264045799477	)
(	86	,	1.16640510193315	)
(	87	,	1.22814657883194	)
(	88	,	1.27870469387736	)
(	89	,	1.31272075284799	)
(	90	,	1.34114513089196	)
(	91	,	1.35861913378785	)
(	92	,	1.37539417656787	)
(	93	,	1.40405154131713	)
(	94	,	1.44272733439334	)
(	95	,	1.48932467544902	)
(	96	,	1.51378827950325	)
(	97	,	1.55968666044311	)
(	98	,	1.62585488474217	)
(	99	,	1.65684211654418	)
(	100	,	1.685965454704	)
(	101	,	1.72906799518049	)
(	102	,	1.78428584433147	)
(	103	,	1.83763979984023	)
(	104	,	1.87398572586367	)
(	105	,	1.93153344206743	)
(	106	,	1.97300507560699	)
(	107	,	2.02263124383129	)
(	108	,	2.06386989066556	)
(	109	,	2.10068179009953	)
(	110	,	2.15566665254523	)
(	111	,	2.19620633926369	)
(	112	,	2.25235613523577	)
(	113	,	2.309437878029	)
(	114	,	2.35673417920049	)
(	115	,	2.42546525725764	)
(	116	,	2.49163348155671	)
(	117	,	2.54149263648626	)
(	118	,	2.5971764590478	)
(	119	,	2.65542313536741	)
(	120	,	2.70901007758145	)
(	121	,	2.76679078049049	)
(	122	,	2.81804785565173	)
(	123	,	2.88305114642442	)
(       150     ,       3.97196261682243        )
(	250	,	11.9158878504673	);  /*44*/
END

