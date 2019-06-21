/* NAME :  ag_face.h          MODULE : Solid   faces internal
** VERSION : 0.6              DATE LAST MODIFIED : 02/25/88
** CONTAINS: Solid face working structures
**  ag_texrec      tedge interesection record
**  ag_pqdata      parametric data
Copyright (c) 1988 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_texrec {      /* tedge intersection record       */
    struct ag_tedge  *te;       /* tedge                           */
    double           t;         /* te(t) is intersection point     */
    } AG_TEXREC, *AG_TEXRECP;

typedef struct ag_pqdata {      /* parametric qdata                */
    int              qtype;
    double           *t0, *tm, *t1;
    double           *q0, *qm, *q1;
    double           *dq0,*dqm,*dq1;
    } AG_PQDATA, *AG_PQDATAP;
                                                                
