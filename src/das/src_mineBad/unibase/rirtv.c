/*********************************************************************
**    NAME         :  rirtv.c
**       CONTAINS:
**       ur_retrieve_tuple_varlist()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rirtv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:47
*********************************************************************/

#include "usysdef.h"
#include "rbase.h"
#include "ribase.h"
#include "udebug.h"
#include "umoveb.h"

/*********************************************************************
** I_FUNCTION : status = ur_retrieve_tuple_varlist(rel, tuple_indx,
**							list, disp, length, &data)
**	retrieve a given tuple's varlist into a caller supplied data area 
**    PARAMETERS   
**       INPUT  : 
**				rel,			a relation number
**				tuple_indx,	a tuple index
**				list, 		list number to retrieve
**				disp,			list displacement in atoms
**				length,		length of data to retrieve in atoms
**				data,			pointer to where to put the data
**       OUTPUT :  
**           data, 		the retrieved data
**    RETURNS      :  0 if function was successful, -1 error
**								atom count if partially filled
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_tuple_varlist(rel, tuple_indx, list, disp, length, data_ptr)
/* argument declarations         */
UR_REL_NUM		rel;			/* relation num to retrieve from				*/
UR_TUPLE_INDX	tuple_indx;	/* tuple index to retrieve from				*/
int				list;			/* which list of data to return				*/
int				disp;			/* displacement to the data start in atoms */
int				length;		/* length in atoms of retrieve */
char 				*data_ptr;	/* ptr to where to put the data */
{
	/* local  parameter declarations */
	int	status;		/* return status */
	char	*lst_ptr;	/* pointer to the existing list */
	int	xfr_len;		/* length of existing list, if any */
	int	atom_cnt;	/* number of atoms in the list */
	int	atom_size;	/* size of atom in bytes */

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC,(us,"ur_retrieve_tuple_varlist(rel %d, tuple_indx %d, list %d,",
						rel,tuple_indx,list));

	uu_dprint(UU_RTRC,(us,"start %d, len %d , to 0x%x)",disp, length, data_ptr));
	/* make sure it is a valid entry before moving data */
	if(data_ptr != 0 && uu_tst_bit(UR_rcb[rel].bmap_ptr,tuple_indx - 1))
	{
		/* go get list adr */
		ur_get_atom_size(rel, list, &atom_size);
		status = ur_get_varlist_ptr(rel,tuple_indx,list,&lst_ptr,&atom_cnt);

		/* check displacement, if not enough data exists, give'em what we got */
		if(disp > 0)
		{
			if(atom_cnt < (disp + length - 1))
			{
				/* less data than requested */
				status = atom_cnt - disp + 1;	/* status = # atoms retrieved */
				xfr_len = status * atom_size;
			}
			else
			{
				xfr_len = length * atom_size;
				status = 0;
			}
			if(xfr_len > 0)
			{
				uu_dprint(UU_RITRC,(us,"xfr %d bytes of varlist", xfr_len));
				uu_move_byte(lst_ptr+((disp-1)*atom_size),data_ptr,xfr_len);
			}
			else
			{
				status = -1;
			}
		}
		else
		{
			status = -1;
		}
	}
	else
	{
		status = -1;
	}
	uu_dexit;
	return(status);
}
