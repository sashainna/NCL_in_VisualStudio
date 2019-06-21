/* NAME :  ag_ssxp.h          MODULE : Solid Intersection internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: span/span intersection working data
**  ag_ssxph     span/span intersection point header  
**  ag_spAspd    spanA/span data loop
**  ag_spAspBd   spanA/spanB data     
**  ag_pVspd     pV int data for a bispan
**  ag_ssxd      span/span int data loop    
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_ssxph { /* span/span intersection point header   */
   struct ag_surface    *srfA;       /* surfaces                   */
   struct ag_surface    *srfB; 
   struct ag_spAspd     *spAspd0;    /* first spanA/span data      */
   } AG_SSXPH, *AG_SSXPHP;

typedef struct ag_spAspd { /* spanA/span data loop                 */
   struct ag_spAspd     *next;       /* ordered by u,v of node A   */
   struct ag_spAspd     *prev;     
   struct ag_snode      *nodeA;      /* lower left node of bispan  */
   struct ag_spAspBd    *spAspB0;    /* first spanA/spanB data     */
   } AG_SPASPD, *AG_SPASPDP;

typedef struct ag_spAspBd { /* spanA/spanB data                    */
   struct ag_spAspBd    *next;       /* ordered by u,v of node B   */
   struct ag_spAspBd    *prev;
   struct ag_snode      *nodeB;      /* lower left node of bispan  */
   struct ag_ssxd       *ssxd0;      /* first intersection data    */
   } AG_SPASPBD, *AG_SPASPBDP;

typedef struct ag_pVspd { /* pV int data for a bispan              */
   struct ag_snode      *node;       /* lower left node of bispan  */
   double               pV[2];       /* u,v for V                  */
   double               dpV[2];      /* direction at pV            */
   int                  c_on_s[2];   /* [0]start:on(1),(2),(0)else */
                                     /* [1]end:  on(1),(2),(0)else */
   } AG_PVSPD, *AG_PVSPDP;

typedef struct ag_ssxd { /* span/span int data loop                */
   struct ag_ssxd       *next;       /*                            */
   struct ag_ssxd       *prev;
   int                  V_id;        /* identifier for V           */
   int                  ss_tan;      /* tang (Na*Nb=+-1),else (0)  */
   int                  s_e_i;       /* V start(-1),end(1),int(0)  */
   double               V[3];        /* point on sp/sp intersectn  */
   double               dV[3];       /* unit direction at V        */
   struct ag_pVspd      pVspA;       /* int point data for span A  */
   struct ag_pVspd      pVspB;       /* int point data for span B  */
   } AG_SSXD, *AG_SSXDP;


/*
Notes: use Na = Sua x Sva/| |, Nb = Sub x Svb/| | 
       and dV = Nb x Na/| |.
       dpVa and dpVb have the same direction as dV so that V,
       pVa, andd pVb have the same classification as start
       end or interior points.

       Use s_e_i < 0  for a start point
          s_e_i > 0  for an end point 
          s_e_i = 0  for an interior (to both spans) point
          s_e_i = -2 for start of a component
          s_e_i = -1 for start of a segment of a component
          s_e_i = 2  for end of a component
          s_e_i = 1  for end of a segment of a component
      A component is a list of start,interior...,end points. 
      If s_e_i = 1 then the next point has the same V_id and has  
      s_e_i = -1 (the component has crossed to a different span 
      on at least one surface). The s_e_i = 0 points occur only
      once in the component list.      

      Isolated tangent points (ss_tan != 0 ) have s_e_i = +- 2
*/
