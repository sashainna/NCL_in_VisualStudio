/*********************************************************************
**    NAME         :  nclclip.h
**       CONTAINS: polygon clipping definitions.
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nclclip.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:34
*********************************************************************/

#ifndef NCLCLIP

typedef enum
{
	NCL_DIFF,
	NCL_INTOF,
	NCL_UNION,
	NCL_CONTOUR
} ncl_polygon_op;

/* 
..... Polygon set structure
*/
typedef struct                      
{
	int num_contours;
	int *np;           /* number of points in each contour, negative
								iff a hole */
	UM_2box *box;
	UU_LIST *contour;  /* contour points list */
} ncl_polygon;

int ncl_polygon_clip ();
int ncl_init_polygon ();
void ncl_free_polygon ();

#define NCLCLIP

#endif
