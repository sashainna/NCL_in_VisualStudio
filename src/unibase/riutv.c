#define MODULEDBG 0
/*********************************************************************
**    NAME         :  riutv.c
**       CONTAINS:
**       ur_update_tuple_varlist()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riutv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:48
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "umoveb.h"
#include "rbase.h"

/*********************************************************************
** I_FUNCTION :  status = ur_update_tuple_varlist(rel,tuple_indx,
**								list,disp,length,data_ptr)
**		update a given tuple's varlist from caller supplied data
**    PARAMETERS   
**       INPUT  : 
**				rel,			relation number to update
**				tuple_indx,	tuple index within the relation 
**				list, 		list number to store
**				disp,			list displacement start atom
**				length,		length of list to store in atoms
**				data_ptr,	pointer to data to store
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function was successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_tuple_varlist(rel,tuple_indx,list,disp,length,data_ptr)
UR_REL_NUM		rel;			/* the relation number */
UR_TUPLE_INDX	tuple_indx;	/* the relation number */
int				list;			/* which list of data to update */
int				disp;			/* displacement in atoms into the list */
int				length;		/* length in atoms of the varlist data */
char 				*data_ptr;	/* pointer to the list of data */
{
	UU_REL_ID	rel_key;			/* a relation,tuple key */
	int			status;			/* return status */
	char			*lst_ptr;		/* pointer to the existing list */
	char			*new_ptr;		/* pointer , where to put the data */
	int			atom_cnt;		/* length of existing list */
	int			new_cnt;			/* new atom count */
	int			new_len;			/* new size in bytes */
	int			atom_size;		/* size of atoms in the list */

	extern char	*uu_toolmalloc();

	uu_denter(UU_RTRC,(us,
	"ur_update_tuple_varlist(rel %d, tup %d, lst %d, dsp %d, len %d, data 0x%x)",
					rel, tuple_indx, list, disp, length, data_ptr));

	/* go get pointer to any existing list data */
	ur_get_atom_size(rel, list, &atom_size);
	status = ur_get_varlist_ptr(rel, tuple_indx, list, &lst_ptr, &atom_cnt);
	if(status == 0)
	{
		if(atom_cnt < (disp + length - 1))
		{
			/* won't fit get new area */
			new_cnt = disp + length - 1;
			new_len = new_cnt * atom_size;
#if MODULEDBG != 0
			uu_dprint(UU_RITRC,(us,"get %d bytes for varlist", new_len));
#endif
			new_ptr = uu_toolmalloc(new_len)	;	/* get a new piece of mem	*/
			if (new_ptr != UU_NULL)
			{
#if MODULEDBG != 0
				uu_dprint(UU_RITRC,(us,"...allocated at 0x%x",new_ptr));
#endif
				if(atom_cnt != 0)
				{
					/* existing list, copy the existing data*/
					uu_move_byte(lst_ptr, new_ptr, atom_cnt * atom_size);
					uu_toolfree(lst_ptr);				/* and, release the memory*/
				}
				/* update the pointer and count */
				ur_update_varlist_info(rel, tuple_indx, list, new_ptr, new_cnt);

				/* now copy in the new data */
				uu_move_byte(data_ptr, new_ptr+(disp-1)*atom_size, length*atom_size);
				ur_unibase_change(rel);
			}
		}
		else
		{
			uu_move_byte(data_ptr, lst_ptr+(disp-1)*atom_size, length*atom_size);
			ur_unibase_change(rel);
		}
	}
	else
	{
		status = 1;
	}
	uu_dexit;
	return(status);
}
