/*********************************************************************
**    NAME         :  msurf.h
**       CONTAINS: data structures for drawing surfaces. 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       msurf.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:33
*********************************************************************/

#ifndef UM_MSURF

#include "usysdef.h"


#define UM_LSTINDEX 200/*size of the array-1 in each member of the linked */
                   	  /*lists used for storing parameter values and crv*/
                   	  /*values                                         */
typedef struct buflist
	{
   struct buflist *nxt;
   UU_REAL pval[UM_LSTINDEX + 1];
	UU_LOGICAL nxtbufused;				/* TRUE iff the next buffer in the list is
												 * being used
												 */
   } UM_PARAMBUFFER;


#define UM_PARAMBUFSIZE sizeof(UM_PARAMBUFFER)

#define UM_LSTINDEX_N_GKS_BUF 70 /*size of array-1 passed to gpolyline3 */

typedef struct crvptlist
	{
	struct crvptlist *nxt;
	UU_REAL pt[UM_LSTINDEX + 1][3];
	UU_LOGICAL nxtbufused;				/* TRUE iff the next buffer in the list is
												 * being used
												 */
   }  UM_CRVPTBUF;

#define  UM_CRVPTBUFSIZE sizeof(UM_CRVPTBUF)

#define UM_BSPLSRF_CNTLPTS 20

#define UM_MSURF
#endif
