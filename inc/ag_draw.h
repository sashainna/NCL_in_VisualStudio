/* NAME :  ag_constant.h      MODULE : AG only internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: constants and structures for graphics ( AGonly )
**  ag_view    connection between screen and world 
**  ag_sbox    screen box 
**  ag_wtost   world to screen transformation 
**  ag_stowt   screen to world transformation
**  ag_oblnk   object link 
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

/* *** size of the graphics screen *** */
#define DR_SCR_COLS 1275
#define DR_SCR_ROWS  991

/* *** to be used with the draw functions *** */
#define ALL_ID       100
#define ASSEMB_ID      0
#define PART_ID        1
#define SHELL_ID       2
#define SH_ARROW_ID     21
#define FACE_ID        3
#define FACEALL_ID      31
#define BNDRY_ID       4
#define TEDGE_ID       5
#define CURVE_ID       6
#define CRVSL_ID        61
#define CPOLY2_ID       62
#define CPOLY3_ID       63
#define CPOLYC_ID       64
#define SRFPOLY_ID      65
#define BSPLINE_ID     7
#define BSPLINE1_ID     71
#define BSPLINE2_ID     72
#define BSPLINE3_ID     73
#define SPAN_ID        8
#define SPAN1_ID        81
#define SPAN2_ID        82
#define SPAN3_ID        83
#define POINT_ID       9
#define POINT2_ID       92
#define BSURF_ID      10
#define SRF_ARROW_ID    101   
#define CCXH_ID       1001
#define CFXH_ID       1002 
#define CSXH_ID       1003
#define CAPSULE_ID    1004
#define PANCAKE_ID    1005
#define CAPSULE2_ID   1006
#define CAPSULE3_ID   1007
 
typedef struct     ag_view {    /* connection between screen world */
   double             center[3];/* center of rotation              */
   struct ag_sbox     *swind;   /* screen window                   */
   struct ag_mmbox    *wask;    /* requested world box             */
   struct ag_mmbox    *wget;    /* actual world box                */
   struct ag_wtost    *ws;      /* world to screen transformation  */
   struct ag_stowt    *sw;      /* screen to world transformation  */
   double             **H;      /* homogeneous trans               */
   double             **S;      /* hom trans from world to screen  */
   int                Smode;    /* 0=S inact 1=S wt1 2=S proj      */
   int                Dmode;    /* displaymode 0=none,1=draw,2=xor */
   } AG_VIEW, *AG_VIEWP;
 
typedef struct ag_sbox {        /* screen box     .                */
   int                minr,     /* minimum row                     */
                      minc,     /* minimum column                  */
                      maxr,     /* maximum row                     */
                      maxc;     /* maximum column                  */
   } AG_SBOX, *AG_SBOXP;

typedef struct ag_wtost {       /* world to screen transformation  */
  double              fx, fy;   /* col = fx * x  + tx              */
  double              tx, ty;   /* row = fy * y  + ty              */
  } AG_WTOST, *AG_WTOSTP;
 
typedef struct ag_stowt {       /* screen to world transformation  */
  double              fr, fc;   /* x = fc * col + tc               */
  double              tr, tc;   /* y = fr * row + tr               */
  } AG_STOWT, *AG_STOWTP;
 
typedef struct ag_oblnk {       /* object link                     */
  struct ag_oblnk     *next;    /* pointer to next object link     */
  int                 kind;     /* object type identifier          */
  int                 vis;      /* visible                         */
  char                *ob;      /* pointer to object               */
  } AG_OBLNK, *AG_OBLNKP;
 
/*
Notes: S is usually to be defined as the composition of the matrix H
       and the world to screen transformation.
       Smode is used as follows:
       Smode = 0:  S is inactive and should not be used.
       Smode = 1;  S is active, has no projective components
                     and has weight 1. This means that one need
                     not compute the weight coordinate of the
                     transformed points since it will be 1.
       Smode = 2;  S is active and should be fully computed
                     since either the weight is not 1 or there
                     are projective components.
*/
