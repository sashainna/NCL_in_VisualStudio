/* NAME :  ag_bi_poly.h       MODULE : Intersection internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: polynomial surface data file header
**  ag_bi_poly_dat         polynomial surface data    
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_bi_poly_dat { /* polynomial surface data         */
   struct ag_surface       *bez;
   struct ag_surface       *bezh;
   struct ag_surface       *pow;
   struct ag_bis_tnd       *tree;
   int                      hold_tree;
   } AG_BI_POLY_DAT, *AG_BI_POLY_DATP;

/* 
 The tree hold flag is used to keep all parts of the tree
 structure from being deallocated after they have been used.
 To insure that the tree will be kept, set it to 1. Otherwise,
 it should be set to 0.

 The tree hold flag works as follows:
 
 When the tree is first constructed, its count will be set to 
 the value of the tree hold flag.

 When the poly dat structure is deallocated, tree_hold is subtracted
 from the tree count before it is deallocated.
*/

