/*********************************************************************
**    NAME         :  rerrn.c
**       CONTAINS:
**       ur_retrieve_rel_name()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rerrn.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:35
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) rerrn.c 3.2 3/3/88 09:59:36 single"};
#else
static char uu_sccsident[]={"@(#) rerrn.c 3.2 3/3/88 09:59:36 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include	"rbase.h"
#include "ribase.h"
#include "rmtuple.h"

/*********************************************************************
**    E_FUNCTION :  status = ur_retrieve_rel_name(key, &name)
**       retrieve the name given to a relation at init time
**				given a key to an entity in the relation
**    PARAMETERS   
**       INPUT  : 
**          key	UU_KEY_ID	key of entry whose relation name is desired
**				name	char[]		address of a character array to hold name
**       OUTPUT :  
**          name	the relation's name is filled in
**    RETURNS      : 0 if all is well non-zero otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_rel_name(key, name)
UU_KEY_ID	key;
char			*name;
{
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	int				status;
	UU_REL_ID		rel_key;

	uu_denter(UU_RTRC, (us, "ur_retrieve_rel_name(key=0x%x, to 0x%x)",
									key, name));
	ur_k2rt(key, &rel, &tuple);	/* find out which relation */
	if (rel == UR_MTUPLE_REL)
	{
		/*if (tuple == 1378897)
				{
					sprintf (" ","Invalid value for DNC modal. /%s/ %s"," "," ");
				}*/
		status = uri_retrieve_master_attr(rel, tuple, UR_DATA_INDX, &rel_key);
		ur_k2rt(rel_key, &rel, &tuple);   /* find out which relation */
	}
	strcpy(name, UR_rcb[rel].relname);
	uu_dexit;
	return(0);
}

