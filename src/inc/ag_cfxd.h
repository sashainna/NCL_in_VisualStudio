/* NAME :  ag_cfxd.h          MODULE : Intersection curve/face 
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: curve/face intersection structures
**  ag_cfxdh       curve/face intersect data header
**  ag_cfxd        curve/face int data  
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/
 
typedef struct ag_cfxh {       /* curve/face intersect data header */
   struct ag_curve     *crv;   /* curve                            */
   struct ag_face      *f;     /* face                             */
   struct ag_cfxd      *cfxd0; /* initial intersection data        */
   } AG_CFXH, *AG_CFXHP;    

typedef struct ag_cfxd {       /* curve/face int data              */
   struct ag_cfxd      *next;  /* data of loop, ordered by         */
   struct ag_cfxd      *prev;  /*               t value on curve   */
   double              uv[2];  /* srf(u,v) = intersect point       */
   double              t;      /* crv(t) = intersect point         */
   int                 c_on_f; /* (1) start of crv on face  
                                  (2) crv on edge of face 
                                  (0) not                          */
   struct ag_tedge     *te;    /* tedge containing edge(or NULL)   */
   double              et;     /* tedge(et) = int point            */
   } AG_CFXD, *AG_CFXDP;      

/*
Notes:  If c_on_f = 1 or 2 then this point is the start of an 
        interval of the curve which is on the face (1) or on an
        edge of the face (2).  The end of the interval is the
        next point.
        te is NULL if point not on edge of face
        if te != NULL then te(et) is point on edge
*/
