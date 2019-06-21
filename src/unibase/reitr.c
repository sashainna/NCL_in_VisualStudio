/*********************************************************************
**    NAME         :  reitr.c
**       CONTAINS:
**       ur_init_transf()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reitr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:31
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include	"udebug.h"
#include	"mattrddl.h"

/*********************************************************************
** E_FUNCTION : ur_init_transf(rel_num,rel_name,n_ent,tuple_size)
**      Initialize a transformation type relation control block.
**      All we do is call the init_rel with no variable list info
**      and set the relation type to transformation.
**    PARAMETERS   
**       INPUT  : 
**		rel_num,		the relation number to be initialized, i.e. - LINE
**		rel_name,	a 0-8 character relation name, i.e. - "LINE"
**		n_ent,		the initial number of entries in the entry,
**						this will also be used as an expansion factor
**						if and when the relation is full
**		tuple_size,	the  size of each entry within the relation,
**						the fixed data size
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_init_transf(rel_num,relname,n_ent,tuple_size)
UR_REL_NUM rel_num;		/* relation id number */
char 	     *relname;		/* relation name	*/
int 	     n_ent;			/* number of initial entries */
int 	     tuple_size;	/* size of fixed data for the relation in bytes	*/
{
	int					i;			/* an index */
	int					status;	/* return status */
	struct	UM_transf_rec	transf_packet	;	/* a dummy transformation	*/

 	uu_denter(UU_RTRC,(us,"ur_init_transf(rel=%d, name=%s, entries=%d, size=%d",
						rel_num, relname, n_ent, tuple_size));
 	status = ur_init_rel(rel_num, relname, n_ent, tuple_size, 0, 0, 0);
 	if(status == 0)
	{
		uu_clr_bit( &UR_rcb[rel_num].rel_flags, UR_MTUPLE_REQD);
		uu_clr_bit( &UR_rcb[rel_num].rel_flags, UR_DATA_REL);
		uu_clr_bit( &UR_rcb[rel_num].rel_flags, UR_ATTR_REL);
		uu_set_bit( &UR_rcb[rel_num].rel_flags, UR_TRANSF_REL);

		/* set THE transformation relation, and */
		/* reserve the default transformation matrix	*/
		UR_transf_relnum = rel_num	;
		UR_default_transf = UU_FALSE	;	/* no default defined yet	*/
		transf_packet.key = 0 ;
		transf_packet.rel_num = rel_num ;
		transf_packet.use_count = 0;    /* reset use count to 1 */
		status = ur_create_tuple(rel_num,&i,&transf_packet);
	}
	/*--------------- function exit ------------------------------------*/
	uu_dprint(UU_RITRC,(us,"ur_init_transf exit status=%d",status));
	uu_dexit;
	return(status);
}
