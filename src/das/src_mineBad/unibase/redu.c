/*********************************************************************
**    NAME         :  redu.c
**       CONTAINS:
**       ur_dump_unibase()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       redu.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:29
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) redu.c 3.1 11/2/87 21:23:04 single"};
#else
static char uu_sccsident[]={"@(#) redu.c 3.1 11/2/87 21:23:04 double"};
#endif
#endif

#include "rbase.h"
#include "ribase.h"

/*********************************************************************
**    E_FUNCTION :  ur_dump_unibase()
**       Dump the contents of Unibase into the trace file.
**			Intended for debugging.
**    PARAMETERS   
**       INPUT		: none
**       OUTPUT	: none
**    RETURNS     : none
**    SIDE EFFECTS: lots of junk in trc
**    WARNINGS    : none
*********************************************************************/
ur_dump_unibase()
{
	UU_KEY_ID 	key;				/* a key_id tuple */
	long			tuple_indx;		/* an entry id */
	UR_REL_NUM	rel;				/* relation number */

	/* first dump all the master tuple objects... */
	tuple_indx = 1;
	while(tuple_indx > 0)
	{
		ur_get_next_key(&tuple_indx, &key);
		if(tuple_indx > 0 && key > 0)
		{
			ur_dump_data(key);
			tuple_indx++;
		}
	}

	/* now find all the tuples that didn't catch */
	for (rel = 1; rel <= UR_MAX_REL; rel++)
	{
		if (!uu_tst_bit(&UR_rcb[rel].rel_flags, UR_MTUPLE_REQD))
		{
			/* Found relation with no master tuples.  Dump it. */
			tuple_indx = 1;
			while(tuple_indx > 0)
			{
				ur_get_next_data_key(rel, &tuple_indx, &key);
				if(tuple_indx > 0 && key > 0)
				{
					ur_dump_data(key);
					tuple_indx++;
				}
			}
		}
	}
}

