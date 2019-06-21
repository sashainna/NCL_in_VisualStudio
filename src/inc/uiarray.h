/********************************************************************* 
**  NAME:  uiarray.h 
**
**  COPYRIGHT  1986  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       uiarray.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:04
*********************************************************************/

#ifndef UIARRAYH

#define MAPSIZE	512				/* Initial size of block map */
#define BLOCKSIZE	1024				/* Size of one block */
#define BLOCKMASK	1023				/* Bit mask for blocksize */
#define MAPSHIFT	10					/* Shift count to get map index */

/* For debugging, the constants can be set to... */
/* #define MAPSIZE	4*/			/* Initial size of block map */
/* #define BLOCKSIZE	4*/			/* Size of one block */
/* #define BLOCKMASK	3*/			/* Bit mask for blocksize */
/* #define MAPSHIFT	2*/			/* Shift count to get map index */

typedef struct {
	int **map;
	int len;
} UU_IARRAY;

/* These macros are intended to be the user interface to the package */

#define UU_IAINIT(a)		a.map = NULL; a.len = 0

#define UU_IADEL(a)  	uu_iadel(&a)

#define UU_IAVAL(a,i) 	( a.map[i>>MAPSHIFT][i&BLOCKMASK] )

#define UU_IASET(a,i,n) ( a.map[i>>MAPSHIFT][i&BLOCKMASK] = n )

#define UU_IAADD(a,n) 	if( !(a.len & (BLOCKSIZE-1)) ) uu_iaadd(&a,n); \
								else { \
									a.map[a.len>>MAPSHIFT][a.len&BLOCKMASK] = n; \
									a.len++; \
								}
								
#define UU_IALEN(a)		( a.len )

#define UIARRAYH
#endif
