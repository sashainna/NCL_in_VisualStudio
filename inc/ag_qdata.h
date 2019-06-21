/* NAME :  ag_qdata.h         MODULE :Intersection internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: Surface/surface intersection points
**  ag_qdata     surface/surface intersection points 
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_qdata {  /* surface/surface intersection points  */
   int               qtype;
   double            *Q0,     *Qm,     *Q1;
   double            *dQ0,    *dQm,    *dQ1;
   double            *t0,     *tm,     *t1;
   double            *q0[2],  *qm[2],  *q1[2];
   double            *dq0[2], *dqm[2], *dq1[2];
   } AG_QDATA, *AG_QDATAP;
