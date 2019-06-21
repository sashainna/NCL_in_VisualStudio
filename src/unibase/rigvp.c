#define MODULEDBG 0
/*********************************************************************
**    NAME         :  rigvp.c
**       CONTAINS:
**       ur_get_varlist_ptr()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rigvp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:44
*********************************************************************/

#include "usysdef.h"
#include "rbase.h"
#include "ribase.h"
#include	"udebug.h"

/*********************************************************************
** I_FUNCTION : status = ur_get_varlist_ptr(rel,tuple,list,&lst_ptr,&lst_len)
**      get a pointer for a list within a relation entry
**    PARAMETERS   
**       INPUT  : 
**				rel,	a relation number
**				tuple index within the relation
**				list, list within the rel,tuple for which to get the ptr
**       OUTPUT :  
**				lst_ptr, pointer to the list data
**				lst_len,	length of the data in atoms
**							(length of fixed data in bytes)
**    RETURNS      :  0 if function was successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_varlist_ptr(rel, tuple, list, lst_ptr, lst_len) 
UR_REL_NUM		rel;			/* relation id to retrieve the list for */
UR_TUPLE_INDX	tuple;		/* entry id to retrieve the list for */
int				list;			/* # of the list from which to get adr */
char				**lst_ptr;	/* address to return */
int				*lst_len;	/* length to return */
{
	int					status;	/* return status */
	struct UR_lpacket	*pack_ptr;

	uu_denter(UU_RTRC,(us,"ur_get_varlist_ptr(rel=%d, tuple=%d, list=%d)",
			rel, tuple, list));

	/* get a pointer to the relation entry */
	status = ur_get_tuple_ptr(rel, tuple, lst_ptr);
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us,"ptr = 0x%x, len = ??", *lst_ptr));
#endif

	/* if we got an entry pointer and the list number is legal, */
	/* then build the pointer to the list */
	if(status == 0 && UR_rcb[rel].n_varl >= list)
	{
		if(list == 0)	
		{
			/*	if list 0, then entry pointer is the list pointer */
			/*	and size is the size of the entry */
			*lst_len = UR_rcb[rel].tuple_size;	/* fixed size is in bytes */
		}
		else	
		{
			/*	otherwise fetch the pointer and length from */
			/*	fixed data after the users entry fixed data */
			ur_get_list_packet_ptr(rel, list, *lst_ptr, &pack_ptr);
			*lst_ptr = pack_ptr->list_ptr;
			*lst_len = pack_ptr->atom_cnt;	/* get the list length in atoms */
		}
	}
	else
	{
		*lst_ptr = 0;
		*lst_len = 0;
		status = -1;	
	}
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us,"ur_get_varlist_ptr, ptr = 0x%x, len = %d",
		*lst_ptr,*lst_len));
#endif
	uu_dexit;
	return(status);
}
