#include <stdio.h>
#include "udebug.h"
#include "uiarray.h"

/********************************************************************* 
**  NAME:  uiarray.c
**
**		An unbounded integer array package.  Elements must be added
**		in order 0 -> n-1.  N can be arbitraily large. (It's maximum
**		is actually dependent on word size.  For 32 bit machines the
**		maximum length is (2**22 * 1024).
**
**		uu_iainit(a) -- Initialize array a.
**
**		uu_iadel(a)	--	 Free all memory associated with array a.
**
**		uu_iaadd(a,n) --  Add array element with value n.
**
**		uu_iaval(a,i) --  Returns value of ith element of array.
**
**		uu_iaset(a,i,n) --  Set value of ith element of array to n.
**
**		uu_ialen(a) -- Return maximum index set by user.
**
**		  Stores pointers to malloc'd blocks of size BLOCKSIZE in a map 
**		of size MAPSIZE.  BLOCKSIZE must be a power of 2.
**		Elements are accessed by:  map[i>>MAPSHIFT][i&BLOCKMASK].  
**		Elements must be added in sequence.  The first time a new block is 
**		needed, it is allocated by a call to uu_toolmalloc.  All blocks 
**		are free'd in uu_iadel.  If the map fills, a new one is allocated 
**		with MAPSIZE more elements in it.  And the old map is copied 
**		into the new map.
**
**		The include file has macros for each of the above functions.  They
**		are the intended user interface.
**		
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uiarray.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:53
*********************************************************************/

/*********************************************************************
**    I_FUNCTION     :  int uu_iainit(a)
**       
**    PARAMETERS   
**       INPUT  : 
**          UU_IARRAY *a		Array to initialize.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_iainit(a)
UU_IARRAY *a;
{
	uu_denter(UU_UITRC,(us,"uu_iainit(%x)",a));

	a->map = NULL;
	a->len = 0;

	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  uu_iadel(a)	
**
**		Delete array a.  Free all memory associated with array.
**       
**    PARAMETERS   
**       INPUT  : 
**          UU_IARRAY *a		Array to free.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_iadel(a)
UU_IARRAY *a;
{
	int i;
	int nblocks;

	uu_denter(UU_UITRC,(us,"uu_iadel(%x)",a));

	nblocks = a->len >> MAPSHIFT;
	for(i=0; i < nblocks; i++) {
		uu_dprint(UU_UITRC,(us,"block %x", a->map[i]));
		uu_toolfree( a->map[i] );
	}

	if( a->map != NULL )
		uu_toolfree( a->map );

	a->map = NULL;
	a->len = 0;

	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  uu_iaval(a,i) 
**		Returns value of ith element of array.
**    PARAMETERS   
**       INPUT  : 
**          UU_IARRAY *a		Array to free.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_iaval(a,i)
UU_IARRAY *a;
int i;
{
	uu_denter(UU_UITRC,(us,"uu_iaval(%x, %d)",a,i));

	uu_dprint(UU_UITRC,(us,"returns %d", a->map[i>>MAPSHIFT][i&BLOCKMASK] ));
	uu_dexit;
	return( a->map[i>>MAPSHIFT][i&BLOCKMASK] );

}

/*********************************************************************
**    I_FUNCTION     :  uu_iaset(a,i,n) 
**		Sets value of ith element of array.
**    PARAMETERS   
**       INPUT  : 
**          UU_IARRAY *a		Array.
**				int i					Element to set.
**				int n					Value to set it to.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_iaset(a,i,n)
UU_IARRAY *a;
int i;
int n;
{
	uu_denter(UU_UITRC,(us,"uu_iaval(%x, %d, %d)",a,i,n));

	uu_dprint(UU_UITRC,(us,"returns %d", a->map[i>>MAPSHIFT][i&BLOCKMASK] ));
	uu_dexit;
	return( a->map[i>>MAPSHIFT][i&BLOCKMASK] );
}

/*********************************************************************
**    I_FUNCTION     :  uu_iaadd(a,n)
**		Add an element at end of array with value n.
**    PARAMETERS   
**       INPUT  : 
**          UU_IARRAY *a		Array.
**				int n;
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_iaadd(a,n)
UU_IARRAY *a;
int n;
{
	int maplen;		/* Current length of map */
	int **tmp;

	uu_denter(UU_UITRC,(us,"uu_iaadd(%x,%d)",a,n));

	/* Add a block if necessary */
	if( !(a->len & (BLOCKSIZE-1)) ) {		/* Length mod BLOCKSIZE */

		/* Add MAPSIZE elements to the map if necessary */
		maplen = a->len >> MAPSHIFT;
		if( !( (maplen) & (MAPSIZE-1) )) {	/* Map length mod MAPSIZE */

			uu_dprint(UU_UITRC,(us,"adding a map"));
			tmp=(int **)uu_toolmalloc( (maplen+MAPSIZE)*sizeof(int *) );
			if( a->map != NULL ) {
				uu_move_byte( a->map, tmp, maplen*sizeof(int *) );
				uu_toolfree(a->map);
			}
			a->map = tmp;
		}

		uu_dprint(UU_UITRC,(us,"adding a block"));
		a->map[a->len>>MAPSHIFT] = (int *)uu_toolmalloc(BLOCKSIZE*sizeof(int));
	}

	a->map[a->len>>MAPSHIFT][a->len&BLOCKMASK] = n;
	a->len++;

	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  uu_ialen(a) -- Return maximum index set by user.
**    PARAMETERS   
**       INPUT  : 
**          UU_IARRAY *a		Array whose max index we want.
**       OUTPUT :  
**          none
**    RETURNS      : maximum index set by user.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_ialen(a)
UU_IARRAY *a;
{
	uu_denter(UU_UITRC,(us,"uu_ialen(%x)",a));

	uu_dprint(UU_UITRC,(us,"returns(%d)",a->len));
	uu_dexit;
	return(a->len);

}

