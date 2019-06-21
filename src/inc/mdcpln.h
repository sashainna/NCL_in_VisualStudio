/*********************************************************************
**    NAME         :  mdcpln.h
**       CONTAINS: construction plane definitions
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       mdcpln.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:29
*********************************************************************/

#ifndef UM_MDCPLN

#include "usysdef.h"
#include "mdcoord.h"

#define UM_MAX_UNITS       12    /* Size of conversion factor array */

typedef struct    {
   UU_KEY_ID   vpid;             /* viewport id               */
   UU_KEY_ID   viewid;           /* view id                   */
   int   dsegid;                 /* grid segment number       */
   int   color;                  /* marker color              */
   UU_LOGICAL  disp;             /* display the grid?         */
   UU_LOGICAL  snap;             /* snap to the grid?         */
   UU_REAL  sx, sy;              /* grid space                */
   UU_REAL  dx, dy;              /* grid snap                 */
   UM_coord cpln_ccllf;          /* lower left corner of grid
                                    in construction plane 
                                    coordinate system         */
   int xnum, ynum;               /* number of grid markers    */
   }  UM_GRID;


struct  UM_cpln_def   {
   UM_vector   xaxis;            /* construction plane x axis */
   UM_vector   yaxis;            /* construction plane y axis */
   UM_vector   zaxis;            /* construction plane z axis */
   UM_coord    origin;           /* construction plane origin */ 
   UM_length   zdepth;           /* distance to construction plane */
   int         length_unit;      /* current input/output linear units */
   int         angle_unit;       /* currrent input/output angular units */
   UU_REAL     length_to_cm;     /* convert linear units to centimeters */
   UU_REAL     ang_to_radians;   /* convert angular units to radians */
   UU_REAL     conv_factors[ UM_MAX_UNITS];  /* array of conversion factors */
   UM_GRID     grid;
};               

struct UM_cpaxis_def {
   int dsegid;
   UU_REAL length;
   int color;
   };

/*************************************************************************
*
*  define external variables
*
*************************************************************************/

#ifdef  UM_MPGM
#define EXT 
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif

EXT struct  UM_cpln_def       UM_cpln;
EXT struct  UM_cpaxis_def     UM_cpaxis;

/*******************MACROS ************************************/

#define  UM_len_inttoext(n1,n2)\
   { n2 = n1 / UM_cpln.length_to_cm;   }
#define  UM_cc_inttoext(v1,v2)\
   { v2[0] = v1[0] / UM_cpln.length_to_cm;\
     v2[1] = v1[1] / UM_cpln.length_to_cm;\
     v2[2] = v1[2] / UM_cpln.length_to_cm;  }
#define  UM_cy_inttoext(v1,v2)\
   { v2[0] = v1[0] / UM_cpln.length_to_cm;\
     v2[1] = v1[1] / UM_cpln.ang_to_radians;\
     v2[2] = v1[2] / UM_cpln.length_to_cm;  }
#define  UM_sp_inttoext(v1,v2)\
   { v2[0] = v1[0] / UM_cpln.length_to_cm;\
     v2[1] = v1[1] / UM_cpln.ang_to_radians;\
     v2[2] = v1[2] / UM_cpln.ang_to_radians;  }
#define  UM_ang_inttoext(g1,g2)\
   { g2 = g1 / UM_cpln.ang_to_radians;    }

#define  UM_len_exttoint(n1,n2)\
   { n2 = n1 * UM_cpln.length_to_cm;   }
#define  UM_cc_exttoint(v1,v2)\
   { v2[0] = v1[0] * UM_cpln.length_to_cm;\
     v2[1] = v1[1] * UM_cpln.length_to_cm;\
     v2[2] = v1[2] * UM_cpln.length_to_cm;  }
#define  UM_cy_exttoint(v1,v2)\
   { v2[0] = v1[0] * UM_cpln.length_to_cm;\
     v2[1] = v1[1] * UM_cpln.ang_to_radians;\
     v2[2] = v1[2] * UM_cpln.length_to_cm;  }
#define  UM_sp_exttoint(v1,v2)\
   { v2[0] = v1[0] * UM_cpln.length_to_cm;\
     v2[1] = v1[1] * UM_cpln.ang_to_radians;\
     v2[2] = v1[2] * UM_cpln.ang_to_radians;  }
#define  UM_ang_exttoint(g1,g2)\
   { g2 = g1 * UM_cpln.ang_to_radians;    }

#undef EXT

#define UM_MDCPLN
#endif
