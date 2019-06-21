/* NAME :  ag_solids.h        MODULE : Solid definition
** VERSION : 0.6              DATE LAST MODIFIED : 02/12/88
** CONTAINS: Solid data structures
**  ag_assembly  an assembly of parts  
**  ag_part      part, if closed, this is a solid
**  ag_shell     shell (bounding surf of solid)
**  ag_face      face (bounded bspline patch)
**  ag_boundary  boundary (of face)
**  ag_tedge     twin (face boundary) edge  
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/
 
typedef struct ag_assembly {  /* an assembly of parts              */
  struct ag_part     *part0;  /* pointer to loop of parts          */
  struct ag_mmbox    *abox;   /* mmax box of loop of parts         */
  } AG_ASSEMBLY, *AG_ASSEMBLYP;
                                                                 
typedef struct ag_part {      /* if closed, this is a solid        */
  struct ag_part     *next;   /* next in a loop of parts           */
  struct ag_part     *prev;   /* next in a loop of parts           */
  struct ag_assembly *owner;  /* assembly of which this is a part  */
  struct ag_shell    *oshell; /* pointer to outer shell. mmbox of
                                 part is same as mmbox of this
                                 outer shell                       */
  } AG_PART, *AG_PARTP;
 
typedef struct ag_shell {     /* shell (bounding surf of solid)    */
  struct ag_shell    *next;   /* next shell in loop                */
  struct ag_shell    *prev;   /* previous shell in loop            */
  struct ag_part     *owner;  /* part of which this is a shell     */
  int                outer;   /* closed outer(1),inner(-1), not(0) */
  struct ag_face     *f;      /* current face in loop of faces     */
  struct ag_mmbox    *shbox;  /* min-max box                       */
  } AG_SHELL, *AG_SHELLP;

typedef struct ag_face {      /* face (bounded bspline patch)      */
  struct ag_face     *next;   /* next face in loop                 */
  struct ag_face     *prev;   /* previous face in loop             */
  struct ag_shell    *owner;  /* shell of which this is a face     */
  struct ag_surface  *srf;    /* bspline surface                   */
  int                flipnorm;/* False if normal = dS/du X dS/dv 
                                 True if normal = -dS/du X dS/dv   */ 
  int                ftype;   /* special face (!= 0) or not (0)    */
  char               *fdata;  /* data needed for special faces     */
  struct ag_boundary *ob;     /* outer boundary                    */
  struct ag_mmbox    *fbox;   /* mmax box                          */
  } AG_FACE, *AG_FACEP;
 
typedef struct ag_boundary {  /* boundary (of face), surface on 
                                 left, viewed from top             */
  struct ag_boundary *next;   /* next boundary in loop             */
  struct ag_boundary *prev;   /* previous boundary in loop         */
  struct ag_face     *owner;  /* face being bounded                */
  int                btype;   /* inner(1),outer(0),segment(<0) */
  struct ag_tedge    *te0;    /* a twin edge in loop               */
  struct ag_mmbox    *bbox;   /* mmax box (parameter space)        */
  } AG_BOUNDARY, *AG_BOUNDARYP;
 
typedef struct ag_tedge    {  /* twin (face boundary) edge         */
  struct ag_tedge    *next;   /* next twin edge in loop            */
  struct ag_tedge    *prev;   /* previous twin edge in loop        */
  struct ag_boundary *owner;  /* bndry, twin edge is a part        */
  struct ag_curve    *edge;   /* 3D edge curve, shared with other 
                                   twin;null if twin's is non null */
  double             e_tol;   /* tolerance used to build edge      */
  int                revedge; /* edge is reversed (T or F)         */
  int                gcon;    /* geometric continuity along edge   */
                              /* 0, 1, 2, or unknown(-1)           */
  struct ag_curve    *pedge;  /* 2-D param edge. Surface left      */
  int                mono;    /* each pebs of pedge is monotone    */
  double             pe_tol;  /* tol (3d) used to build pedge      */
  struct ag_tedge    *twin;   /* twin edge on neighboring face
                                 If NULL and edge not NULL a true
                                 edge of face                      */
  } AG_TEDGE, *AG_TEDGEP;  

/* Notes:
   gcon           geometric continuity between faces.
     gcon = 0     f0 and f1 are continuous along the edge and f0 and
                  f1 may only be tangent at the start or end of the
                  edge, i.e. N1xN0 != 0 on the interior of the edge.
                  Isolated tangencies must be vertices and must
                  separate tedges.
     gcon = 1     f0 and f1 have tangent continuity along the edge,
                  i.e. at every point on the edge the normal to f0
                  and the normal to f1 are the same.
     gcon = 2     curvature continuity along the edge
                  e.g. the edge between the Bez faces of a cylinder. 
     gcon = -1    continuity not known along edge.
                 
   mono 
     true         each bs of pe is monotone in both u and v
                  (pe has been prepared for fast boundary checking).
     false        bs of pe may not be monotone in both u and v.
                  (pe has not been prepared for fast boundary 
                  checking).


   bndry 
     btype =  1   an inner boundary, clockwise.
     btype =  0   an outer boundary, counter clockwise.
     btype = -1   a boundary segment (a curve on face).
     btype = -2   a closed boundary segment (a closed curve on face).

   shell
     outer =  1   shell is closed and outer.
     outer = -1   shell is closed and inner.
     outer =  0   shell is not closed.
*/
