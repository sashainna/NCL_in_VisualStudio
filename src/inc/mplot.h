
/*********************************************************************
**    NAME         :  mplot.h
**       CONTAINS: data for plotting
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       mplot.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:32
*********************************************************************/

#ifndef	UM_MPLOT

#include "usysdef.h"

#ifdef  UM_MPGM
#define EXT 
#else
#define EXT extern
#endif

EXT UU_LOGICAL	UM_plotting;	/* Is this for plotting? */
EXT UU_REAL	UM_plotprec;		/* plot precision */
EXT UU_REAL	UM_long_dash;		/* length of dash for long dashed lines */
EXT UU_REAL	UM_long_gap;		/* length of gap for long dashed lines */
EXT UU_REAL	UM_short_dash;		/* length of dash for short dashed lines */
EXT UU_REAL	UM_short_gap;		/* length of gap for short dashed lines */

#undef	EXT

#define	UM_MPLOT
#endif
