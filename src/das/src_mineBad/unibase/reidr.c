/*********************************************************************
**    NAME         :  reidr.c
**       CONTAINS:
**       ur_init_data_rel()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reidr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:31
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include	"udebug.h"

/*********************************************************************
** E_FUNCTION : ur_init_data_rel(rel_num, rel_name, n_ent, tuple_size,
**										n_varl, atom_sizes, list_sizes)
**      initialize a data relation control block, there will be no
**		 associated master tuple created upon create data
**    PARAMETERS   
**       INPUT  : 
**		rel_num,		the relation number to be initialized, i.e. - 89
**		rel_name,	a 1-8 character relation name, i.e. - "LAYER"
**		n_ent,		the initial number of entries in the entry,
**						this will also be used as an expansion factor
**						if and when the relation is full
**		tuple_size,	the  size of each entry within the relation,
**						the fixed data size and any variable length list info
**		n_varl,		the number of possible variable length lists
**						associated with each entry
**		atom_sizes,	an integer array containing the atom sizes for 
**						each variable length list 
**		list_sizes,	an integer array containing the size in atoms 
**						to be used when setting up or expanding 
**						each variable length list 
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_init_data_rel(rel_num,relname,n_ent,tuple_size,n_varl,atom_sizes,list_sizes)
int	rel_num;			/* relation id number */
char 	*relname;		/* relation name */
int 	n_ent;			/* number of initial entries */
int 	tuple_size;		/* size of fixed data for the relation in bytes	*/
int	n_varl;			/* number of variable lists for this type rel */
int	atom_sizes[];	/* an array of byte counts, 1 for each varl */
int	list_sizes[];	/* an array containing the size to be used when	*/
							/* setting up or expanding each variable list */
{
	struct UR_rcb_rec	*rcb;		/* ptr to relation control block */
	int					size;	
	int					i,j;		/* indexs */
	int					status;	/* return status */
	char					*m_ptr;	/* pointer to allocated memory */

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**

		initialize entity relation control block(rcb)
*/

 uu_denter(UU_RTRC,(us,"ur_init_data_rel rel=%d, name=%s, with %d lists",
						rel_num,relname,n_varl));
 uu_dprint(UU_RTRC,(us,"entry size = %d, with = %d, initial entries ",
						tuple_size,n_ent));
	status = ur_init_rel(rel_num, relname, n_ent, tuple_size, n_varl,
							atom_sizes, list_sizes);
	if(status) goto exit;
	uu_clr_bit(&UR_rcb[rel_num].rel_flags,UR_MTUPLE_REQD);

exit:
	uu_dprint(UU_RITRC,(us,"ur_init_data_rel exit status = %d",status));
	uu_dexit;
	return(status);
}
