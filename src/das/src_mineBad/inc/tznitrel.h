#ifndef UM_INITREL
/*********************************************************************
**    NAME         :  minitrel.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tznitrel.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:01
*********************************************************************/
/**********************************************************************/
/*								      													*/
/*   Number of entries per relation                                   */
/*																					      */
/**********************************************************************/

#define  UM_NE_MTID 2000  /* number of master tuple id  relational entries*/
#define  UM_NE_POINT 1000 /* number of point relational entries */
#define  UM_NE_LINE 500   /* number of line relational entries */
#define  UM_NE_CIRCLE 500 /* number of circle relational entries */
#define  UM_NE_CONIC 50   /* number of conic relational entries */
#define  UM_NE_COMPCRV 50 /* number of composite curve relational entries */
#define  UM_NE_BSPLCRV 25 /* number of bspline curve relational entries */
#define  UM_NE_RBSPLCRV 25 /* number of rational bspline curve relational */
#define	UM_NE_POLY	50		/* number of polygons */	
#define	UM_NE_POLYLINE	50 /* number of polyline curves */	
#define  UM_NE_WFSPLCRV 1 /* number of wilson-fowler spline curve */
#define  UM_NE_CSPLCRV 1  /* number of cardinal spline relational entries */
#define  UM_NE_RESTCRV 1  /* number of restricted curve relational entries*/
#define  UM_NE_MACHCRV 1  /* number of machining curve relational entries */
#define  UM_NE_INTCRV 1   /* number of intersection curve relational */
#define  UM_NE_PLN 1      /* number of plane relational entries */
#define  UM_NE_CYL 1      /* number of cylinder relational entries */
#define  UM_NE_CONE 1     /* number of cone relational entries */
#define  UM_NE_SPH 1      /* number of sphere relational entries */
#define  UM_NE_REVSRF 1   /* number of surface of revolution relational */
							 /* entries */
#define  UM_NE_RULSRF 1   /* number of ruled surface relational entries */
#define  UM_NE_TABCYL 1   /* number of tabulated cylinder realtional */
							 /* entries */
#define  UM_NE_BSPLSRF 10  /* number of bspline surface relational entries */
#define  UM_NE_RBSPLSRF 10 /* number of rational bspline surface relational*/
							 /* entries */
#define  UM_NE_COONSRF 1  /* number of coons interpolated surface */
							 /* relational entries */
#define  UM_NE_CDRVSRF 1  /* number of curve driven surface relational */
							 /* entries */
#define  UM_NE_COMPSRF 1  /* number of composite surface relational */
							 /* entries */
#define  UM_NE_RESTSRF 1  /* number of restricted surface relational */
							 /* entries */
#define  UM_NE_EXTSRF 1   /* number of extrapolated surface relational */
							 /* entries */
#define  UM_NE_PLANARFACE 1   /* number of planar facet relational entries */
#define  UM_NE_COMPFACE 1   /* number of planar faceted surface relational */
							 /* entries */
#define  UM_NE_BODY 50	 /* number of body relational entries */
#define  UM_NE_TEXT 50    /* number of text primitive entries */
#define  UM_NE_TEXTATTR 50 /* number text attribute entries */
#define  UM_NE_SURFREND 5 /* number text attribute entries */

#define  UM_NE_TRANSFORM 50 /* number of transformation entries */
#define  UM_NE_GROUP 5 /* number group */
#define  UM_NE_COORDSYS 15 /* number coordinate systems */
#define  UM_NE_SHEET 15 /* number sheets */
#define  UM_NE_DRAWING 15 /* number drawing */
#define  UM_NE_LAYER 15 /* number layers */

#define  UM_NE_ATTR 50   /* number of attribute entries */


#define  US_NE_SYMBOL 20	/* number of symbol entries */

/**********************************************************************/
/*																						   */
/*  Number of variable lists per relation                             */
/*																							*/
/**********************************************************************/

#define  UM_NV_MTID     0    /* number of master tuple id  variable lists */
#define  UM_NV_POINT    0    /* number of point variable lists */
#define  UM_NV_LINE     0    /* number of line variable lists */
#define  UM_NV_CIRCLE   0    /* number of circle variable lists */
#define  UM_NV_CONIC    0    /* number of conic variable lists */
#define  UM_NV_COMPCRV  1    /* number of composite curve variable lists */
#define  UM_NV_BSPLCRV  1    /* number of bspline curve variable lists */
#define  UM_NV_RBSPLCRV 3    /* number of rational bspline curve variable */
							  	 	  /* lists */
#define	UM_NV_POLY	0			/* number of filled polygon vlists	*/
#define	UM_NV_POLYLINE	1		/* number of polyline vlists	*/
#define  UM_NV_WFSPLCRV 0    /* number of wilson-fowler spline curve */
								 	  /* variable lists */
#define  UM_NV_CSPLCRV  0    /* number of cardinal spline variable lists */
#define  UM_NV_RESTCRV  0    /* number of restricted curve variable lists */
#define  UM_NV_MACHCRV  1    /* number of machining curve variable lists */
#define  UM_NV_INTCRV   0    /* number of intersection curve variable */
								 	  /* lists */
#define  UM_NV_PLN      0    /* number of plane variable lists */
#define  UM_NV_CYL      0    /* number of cylinder variable lists */
#define  UM_NV_CONE     0    /* number of cone variable lists */
#define  UM_NV_SPH      0    /* number of sphere variable lists */
#define  UM_NV_REVSRF   0    /* number of surface of revolution variable */
								 	  /* lists */
#define  UM_NV_RULSRF   0    /* number of ruled surface variable lists */
#define  UM_NV_TABCYL   0    /* number of tabulated cylinder variable */
								 	  /* lists */
#define  UM_NV_BSPLSRF  1    /* number of bspline surface variable lists */
#define  UM_NV_RBSPLSRF 4    /* number of rational bspline surface */
								 	  /* variable lists */
#define  UM_NV_COONSRF  2    /* number of coons interpolated surface */
								 /* variable lists */
#define  UM_NV_CDRVSRF  0    /* number of curve driven surface variable */
								 /* lists */
#define  UM_NV_COMPSRF  1    /* number of composite surface variable lists*/
#define  UM_NV_RESTSRF  0    /* number of restricted surface variable */
								 /* lists */
#define  UM_NV_EXTSRF   0    /* number of extrapolated surface variable */
								 /* lists */
#define  UM_NV_PLANARFACE   1    /* number of planar facet variable lists */
#define  UM_NV_COMPFACE   1    /* number of planar faceted surface variable */
								 /* lists */
#define  UM_NV_BODY     1    /* number of body variable lists */
#define  UM_NV_TEXT     1    /* number of text variable lists */
#define  UM_NV_TEXTATTR 0    /* number text attribute var lists */

#define  UM_NV_SURFREND 1    /* number text attribute var lists */

#define	UM_NV_GROUP		1		/* number of group var lists */
#define	UM_NV_COORDSYS	0		/* number of coordsys var lists */
#define	UM_NV_SHEET		1		/* number of sheet var lists */
#define	UM_NV_DRAWING	1		/* number of drawing var lists */
#define	UM_NV_LAYER		0		/* number of layer var lists */
#define  US_NV_SYMBOL   3		/* number of symbol var lists */
#define UM_INITREL
#endif
