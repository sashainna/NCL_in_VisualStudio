/* NAME :  ag_oefxd.h         MODULE : Solid  internal
** VERSION : 0.7              DATE LAST MODIFIED : 03/22/88
** CONTAINS:  offset edge / face intersection data
**  ag_oefxdh      offset edge/face int header
**  ag_oefxdl      offset edge/face intersection 
**  ag_oeofrbfh    offset edge/face roball fillet
**  ag_oeoedl      offset edge loop  
**  ag_oeofdl      offset edge/face data loop  
**  ag_ofofrbfh    offset face/face rbf header   
**  ag_ffrbfdl     fA/fB roball fillet data loop 
**  ag_ofofdl      o face/face data loop          
**  ag_efrbcdl     edge/face roball corner data loop
**  ag_efcdl       e/f corner data loop            
**  ag_rbfcted     roball fit C te data    
**  ag_rbqdata     roball q data  
**  ag_rbfsh       rbf segment header
**  ag_rbfsl       rbf segment loop
**  ag_rbf_tdat    temp defining data 
Copyright (c) 1988 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_oefxdh  {     /* offset edge/face int data header*/
   double             r;        /* offset distance(roball radius)  */
   struct ag_tedge    *te0;     /* tedge  with edge                */
   struct ag_face     *f1;      /* face                            */
   struct ag_oefxdl   *oefxd0;  /* initial intersection data       */
   } AG_OEFXDH, *AG_OEFXDHP;

typedef struct ag_oefxdl  {     /* offset edge/face intersection   */
   struct ag_oefxdl   *next;    /* data loop ordered by position   */
   struct ag_oefxdl   *prev;    /*          on edge                */
   double             t0;       /* point on edge of te0            */
   double             pV0[2];   /* point on face that owns te0     */
   double             pV1[2];   /* point on f1                     */
   int                e_on_f;   /* 0 not on e of f1                */
                                /* 1 start of e on f1              */
                                /* 2 start of e on e of f1         */
   struct ag_tedge    *te1;     /* te of f1 when e_on_f > 0        */
   double             t1;       /* point on te1                    */
   } AG_OEFXDL, *AG_OEFXDLP;

typedef struct ag_oeofrbfh {    /* offset edge/face roball fillet  */
   struct ag_oeofel   *oeofe0;  /* fillet header                   */
   } AG_OEOFRBFH, *AG_OEOFRBFHP;

typedef struct ag_oeofel  {     /* offset edge loop                */
   struct ag_oeofel   *next;    /* next, ordered by edge           */
   struct ag_oeofel   *prev;    /* previous                        */
   struct ag_curve    *edge;    /*                                 */
   struct ag_oeofdl   *oeofd0;  /* initial o e/f data of loop      */
   } AG_OEOFEL, *AG_OEOFELP;

typedef struct ag_oeofdl   {    /* offset edge/face data loop      */
   struct ag_oeofdl   *next;    /* ordered by position on edge     */
   struct ag_oeofdl   *prev;    /*                                 */
   struct ag_tedge    *te;      /* tedge of edge                   */
   struct ag_face     *f;       /* face for oe/f intersection      */
   int                C_id;     /* center point identifier         */
   double             t;        /* point on edge                   */
   double             pV0[2];   /* point on face of tedge          */
   double             pV1[2];   /* point on f                      */
   struct ag_tedge    *rte;     /* refined te with pV0 at end of pe*/
   } AG_OEOFDL, *AG_OEOFDLP; 

typedef struct ag_ofofrbfh {    /* offset face/face rbf header     */
   struct ag_ffrbfdl  *fAfBfd0; /* initial fA/fB fillet data       */
   struct ag_efrbcdl  *eAfBcd0; /* initial eA/fB corner data       */
   struct ag_efrbcdl  *eBfAcd0; /* initial eB/fA corner data       */
   } AG_OFOFRBFH, *AG_OFOFRBFHP;

typedef struct ag_ffrbfdl  {    /* fA/fB roball fillet data loop   */
   struct ag_ffrbfdl  *next;    /* ordered by fA then fB           */
   struct ag_ffrbfdl  *prev;    /*                                 */
   struct ag_face     *fA,*fB;  /*                                 */
   struct ag_ofofdl   *ofofd0;  /* initial of/f data of loop       */
   } AG_OFAOFBDL, *AG_OFAOFBDLP;

typedef struct ag_ofofdl  {     /* offset face/face data loop      */
   struct ag_ofofdl   *next;    /* ordered by position along C     */
   struct ag_ofofdl   *prev;    /*                                 */
   int                C_id;     /* center point identifier         */
   double        C[3],dC[3];    /* center and direction            */
   int                C_sie;    /* C is start(-1), int0), end(1)   */
   double      Va[3],dVa[3];    /* point and direction on fA       */
   double    pVa[2],dpVa[2];    /* point and direction on fA       */
   double      Vb[3],dVb[3];    /* point and direction on fB       */
   double    pVb[2],dpVb[2];    /* point and direction on fB       */
   struct ag_tedge *tea,*teb;   /* V, pV is on te                  */
   double             ta,tb;    /* point on te                     */
   } AG_OFOFDL, *AG_OFOFDLP;   

typedef struct ag_efrbcdl {     /* e/f roball corner data loop     */
   struct ag_efrbcdl  *next;    /* ordered by edge then face       */
   struct ag_efrbcdl  *prev;    /*                                 */
   struct ag_curve    *edge;    /*                                 */
   struct ag_face     *f;       /*                                 */
   struct ag_efcdl    *efcd0;   /* initial edge/face corner data   */
   } AG_EFRBCDL, *AG_EFRBCDLP;

typedef struct ag_efcdl   {     /* e/f corner data loop            */
   struct ag_efcdl    *next;    /* ordered by position along C     */
   struct ag_efcdl    *prev;    /*   start, interior, end          */
   int                 C_id;    /* center point identifier         */
   struct ag_tedge      *te;    /* tedge                           */
   double                 t;    /* point on edge                   */
   double        C[3],dC[3];    /* center and direction            */
   int                C_sie;    /* center start(-1),int(0),end(1)  */
   double      Ve[3],dVe[3];    /* point and direction at edge     */
   double      Vf[3],dVf[3];    /* point and direction at face     */
   double    pVf[2],dpVf[2];    /* point and direction at face     */
   } AG_EFCDL, *AG_EFCDLP;   

typedef struct ag_rbfcted  {    /* roball fit C te data            */
   double             r;        /* radius                          */
   int                flipn[2]; /* flip normal T or F              */
   int                C_ok;     /* fit is ok(1),not(0), or err(-1) */
   int                e_ok[2];  /*                                 */
   int                pe_ok[2]; /*                                 */
   double             f_tol;    /* fitting tolerance               */
   } AG_RBFCTED, *AG_RBFCTEDP; 

typedef struct ag_rbqdata {        /* roball q data                */
   int    Ctype,Qtype[2],qtype[2]; /* type: flat(1),orient(2),or(0)*/
   double *t0,    *tm,        *t1; /* parameter values             */
   double *C0,    *Cm,        *C1; /* 3d center points             */
   double *dC0,   *dCm,      *dC1; /* 3d center directions         */
   double *Q0[2], *Qm[2],  *Q1[2]; /* 3d edge points               */
   double *dQ0[2],*dQm[2],*dQ1[2]; /* 3d edge directions           */
   double *q0[2], *qm[2],  *q1[2]; /* 2d edge points               */
   double *dq0[2],*dqm[2],*dq1[2]; /* 2d edge directions           */
   } AG_RBQDATA, *AG_RBQDATAP; 

typedef struct ag_rbfsh     {    /* rbf segment header             */
   struct ag_rbfsl       *rbfs0; /* initial rbf segment            */
   } AG_RBFSH, *AG_RBFSHP;

typedef struct ag_rbfsl     {    /* rbf segment loop               */
   struct ag_rbfsl  *next,*prev; /* next previous of loop          */
   int              C_id0,C_id1; /* id for start and end of seg    */
   struct ag_face   *rbf0,*rbf1; /* start and end rbf faces        */
   } AG_RBFSL, *AG_RBFSLP;

typedef struct ag_rbf_tdat  {    /* temp defining data             */
   int              C_id0,C_id1; /* id for start and end           */
   struct ag_tedge  *tea0,*tea1; /* tea at start and end           */ 
   struct ag_tedge  *teb0,*teb1; /* teb at start and end           */ 
   double       ta0,ta1,tb0,tb1; /* pts on tea and teb             */
   struct ag_spline        *cbs; /* bs for center of ball          */
   double           c_tol,s_tol; /* fit tol for center and srf     */
   } AG_RBF_TDAT, *AG_RBF_TDATP;
