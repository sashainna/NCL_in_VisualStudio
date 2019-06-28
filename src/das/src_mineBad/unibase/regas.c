/*********************************************************************
**    NAME         :  regas.c
**       CONTAINS:
**       ur_get_atom_size()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       regas.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:30
*********************************************************************/

#include "usysdef.h"
#include "rbase.h"
#include "ribase.h"
#include	"udebug.h"

/*********************************************************************
**    E_FUNCTION : status = ur_get_atom_size(rel_num,list_num,&atom_size)
**	get relation list atom size
**    PARAMETERS   
**       INPUT  : 
**				rel_num,	relation to get the atom size for
**				lst_num,	list number within relation to get atom size for
**       OUTPUT :  
**				atom_size, size of an atom for rel_num,list number
**    RETURNS      :  0 if function was successful(always)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_atom_size(rel_num, lst_num, atom_size)
UR_REL_NUM	rel_num;		/* relation to get atom size for			*/
int			lst_num;		/* list number to get atom size for		*/
int			*atom_size;	/* return the size of an atom				*/
{

/*----------------------------------------------------------------------
** Start of Executable Code
**----------------------------------------------------------------------
**
*/

	uu_denter(UU_RTRC,(us,"ur_get_atom_size(rel=%d, list=%d)",rel_num,
				lst_num));
	/* get the atom size out of the relation control block */
	*atom_size = UR_rcb[rel_num].lparms[lst_num-1].atom_size;
	uu_dprint(UU_RTRC,(us,"atom size = %d",*atom_size));
	uu_dexit;
	return(0);
}

