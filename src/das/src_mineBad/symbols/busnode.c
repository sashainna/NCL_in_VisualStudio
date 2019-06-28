/*********************************************************************
**    NAME:  busnode.c
**       CONTAINS:
**      		ubu_get_snap_nodes 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       busnode.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:07
*********************************************************************/
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "dmark.h"		/* for UD_MARK */
#include "dselmask.h"	/* for mask types to limit DAS */
#include "dasnog.h"		/* for DAS */
#include "mdrel.h"		/* for relation numbers */
#include "mattr.h"		/* for point attributes */
#include "mcrv.h"			/* for point definition */

#include "bsym.h"		/* declares symbol storage and definitions */

#define TRACE UU_FALSE /* for debugging only */
/*********************************************************************
**    E_FUNCTION :int ubu_get_snap_nodes(symptr)
**       Get the snap nodes for a master symbol; note, we assume each snap node 
**			has the default transformation associated with it.
**    PARAMETERS   
**       INPUT  : 
**			symptr		pointer to the master symbol record.
**       OUTPUT :  
**			symptr		pointer to the master symbol record with the
**							snap node data filled-in.
**    RETURNS: UU_SUCCESS if no problems encountered, otherwise UB_FAILURE.
**    SIDE EFFECTS: none
**    WARNINGS: Note, MARK's and UNMARK's must taken care of in calling
**				  function.
*********************************************************************/
int ubu_get_snap_nodes(symptr)
	struct UB_symbol_rec *symptr;
{
	UU_LOGICAL more_snap_nodes;	/* TRUE iff there are potentially more snap
											 * nodes */
	struct UM_point_rec e;			/* point record */
	struct UM_attrdata_rec ptattr;	/* point (snap node) attribute rec */
	struct UB_snap_nod_rec snapnod;
	int i,j;
	int numint;		/* number of appropriate user interactions */
	int status = UU_SUCCESS;		/* either UU_SUCCESS or UB_FAILURE */ 

    UD_NDCLOCREC tmp;

	uu_denter(UU_BTRC, (us,"ubu_get_snap_nodes(symptr:%x)", symptr));
	
	i = 0;
	/* set marker type for snap nodes */
	ubi_set_marker_type(UB_snap_node_marker_type);

	more_snap_nodes = UU_TRUE;
	while (more_snap_nodes)
	{
		ur_setup_data(UM_POINT_REL, &e, sizeof(struct UM_point_rec));

		/* get next snap node */
		ud_ldas(UD_DASCART, UB_SYMBOL, 4, &tmp, 1, &numint, UD_NODEFAULT);
        for(j=0; j < 3; j++) e.pt[j] = tmp.cord[j];
		/* prompt is: Enter next snap node. */

		if (numint <= 0) /* done with snap nodes */
			more_snap_nodes = UU_FALSE;
		else /* create a snap node at the location specified */
		{
			e.snap_node = UU_TRUE;
			/* set attribute to draw snap nodes  */
			e.markertype = UB_snap_node_marker_type;	
			if (um_current_default_attr(UM_POINT_REL, &ptattr) != UU_SUCCESS)
				goto failed;
			
			ptattr.color = UB_snap_node_color;/* set snap node color  */
			if (um_create_geom(&e, UM_DEFAULT_TF, &ptattr) != UU_SUCCESS)
				goto failed;

			if (uc_display(&e) != UU_SUCCESS)
				goto failed;

			snapnod.snap_key = e.key;
			snapnod.nbr = i; /* set snap node number */

			/* put the text node record into the variable list */
			if (ubi_update_app_varlist(symptr,UB_MSNAP_LIST,&snapnod, i+1, 1) 
						!= UU_SUCCESS) goto failed;
			i++;			
		}
	} /* endwhile more snap nodes */
	if (ubi_reset_marker_type() != UU_SUCCESS)
		goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

