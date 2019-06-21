/* NAME :  ag_bl_pr.h         MODULE : Intersection internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: Polynomial spline data file header
**  ag_poly_data   polynomial spline data  
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_poly_data {   /*  polynomial spline data         */
   int                  dim;
   struct ag_spline    *bez;
   struct ag_spline    *bezh;
   struct ag_spline    *pow;
   struct ag_spn_tnd   *tree; 
   int                  hold_tree;
   } AG_POLY_DAT, *AG_POLY_DATP;

/*
Notes:  The tree hold flag is used to keep all parts of the tree
        structure from being deallocated after they have been used.
        To insure that the tree will be kept, set it to 1.
        Otherwise, it should be set to 0.

        The tree hold flag works as follows:
 
        When the tree is first constructed, its count will be set
        to the value of the tree hold flag.

        When the poly dat structure is deallocated, tree_hold 
        is subtracted from the tree count before it is deallocated.
*/
 
