/*********************************************************************
**    NAME         :  rilrgpw2.c
**       CONTAINS:
**       ur_largest_pow_2()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rilrgpw2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:44
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) rilrgpw2.c 2.1 10/31/86 21:00:53 single"};
#else
static char uu_sccsident[]={"@(#) rilrgpw2.c 2.1 10/31/86 21:00:53 double"};
#endif
#endif

#include "udebug.h"

/*********************************************************************
**    E_FUNCTION :  numpow2 = ur_largest_pow_2(number)
**       returns the largest power of 2 number which is <= given number
**    PARAMETERS   
**       INPUT  : 
**          number	int	the given number
**       OUTPUT :  
**          none
**    RETURNS      : the power of two number
**    SIDE EFFECTS : none
**    WARNINGS     : all negatives return 0
*********************************************************************/

ur_largest_pow_2(number)
int	number;
{
	int	test;		/* current number being tested against */

	uu_denter(UU_RTRC,(us,"ur_largest_pow_2(%d)",number));
	test = 1;		/* bit in lowest position */
	while (test <= number && test != 0)
	{
		test <<= 1;		/* next power of two */
	}
	test >>= 1;			/* return last shift */
	uu_dprint(UU_RITRC,(us," returns %d",test));
	uu_dexit;
	return(test);
}

