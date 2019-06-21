/*********************************************************************
**    NAME:  bconect.c
**       CONTAINS:
**			int ub_snapnode_of_instance(pick, pt, foundptr)
**			int ub_disp64_connect(con)
**			int ub_drw64_connect(con, tfmat, attrptr)
**			int ub_p64_connect(con, tfmat)
**			int ub_transf64_connect(con, tfmat, store)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       bconect.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:02
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"	/* for UC_attributedatabag */
#include "mdgenent.h"	/* for UM_entitydatabag */
#include "mdrel.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mattr.h"	/* for UM_transf_rec and UM_attrmdl */ 
#include "mdattr.h"
#include "mdpick.h"
#include "modef.h"
#include "bsym.h"
#include "r1esched.h"

#define TRACE UU_TRUE /* for debugging only */
/*********************************************************************
**    E_FUNCTION: int ub_snapnode_of_instance(pick, pt, foundptr)
**       This function returns the coordinate of the picked snap node
**			of a symbol instance.
**    PARAMETERS   
**       INPUT  : 
**				pick		Pick record containing entity picked.
**       OUTPUT :  
**          pt			Point returned if a snap node was picked.
**				foundptr	Pointer to UU_TRUE iff a snap node was picked.
**    RETURNS: UU_SUCCESS if no problems; UB_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_snapnode_of_instance(pick, pt, foundptr)
	UM_PLOCREC *pick;
	UM_coord pt;
	UU_LOGICAL *foundptr;
{
	struct UM_entitydatabag e;
	UM_transf tfmat;
	struct UM_point_rec *ptptr;
	int level, status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us,"ub_snapnode_of_instance(pick=%x,?,?)", pick));

	*foundptr = UU_FALSE;
	/* um_m_nrendpt(2, &(pick->pent), &(pick->ploc), pt); */
	/*um_ploctocc(&(pick->ploc), pt);*/
#if (TRACE)
	e.key = um_get_pickkey(&(pick->pent), 1);
	uu_dprint(UU_BTRC,(us,"level 1 pick is entity with key:%d",e.key));
#endif
	level = 2;
	e.key = um_get_pickkey(&(pick->pent), level);
	uu_dprint(UU_BTRC,(us,"level 2 pick is entity with key:%d",e.key));

	if (um_get_all_geom(&e, sizeof(e)) != UU_SUCCESS) goto failed;
	ptptr = (struct UM_point_rec *) &e;
	if ((e.rel_num != UM_POINT_REL) || (!ptptr->snap_node)) 
	{
		uu_uerror0(UB_SYMBOL, 103);
		/* error is: No snap node picked */
		goto done;
	}
	if (um_get_transformation(e.key, tfmat) != UU_SUCCESS) goto failed;
	if (uc_transform(&e, tfmat, UU_FALSE) != UU_SUCCESS) goto failed;
	um_vctovc(ptptr->pt, pt);
	*foundptr = UU_TRUE;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION: int ub_disp64_connect(con)
**       Display a connector.
**    PARAMETERS   
**       INPUT  : 
**          con					connector entity data
**       OUTPUT :  none
**    RETURNS: UU_SUCCESS  iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_disp64_connect(con)
	struct UB_conector_rec *con;
{
	int status;
	uu_denter(UU_BTRC,(us,"ub_disp64_connect(key=%d)", con->key))
	if (ur1i_hash_print() != UU_SUCCESS) goto failed;
	status = uv_disp_entity(con);
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION: int ub_drw64_connect(con, tfmat, attrptr)
**       Draw a connector.
**    PARAMETERS   
**       INPUT  : 
**          con					connector entity data
**				tfmat					transformation matrix
**				attrptr				display attribute bundle
**       OUTPUT :  none
**    RETURNS: UU_SUCCESS  iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_drw64_connect(con, tfmat, attrptr)
	struct UB_conector_rec *con;
	UM_transf tfmat;
	struct UC_attributedatabag *attrptr;
{
	struct UM_polyline_rec pline;
	int status;

	uu_denter(UU_BTRC,(us,"ub_drw64_connect(con>key=%d, tfmat=%x, attrptr=%x)",
		con->key, tfmat, attrptr));
	pline.key = con->pline;
	status = uc_retrieve_data(&pline, sizeof(pline));
	if (status == UU_SUCCESS) 
		uc_draw(&pline, tfmat, attrptr);

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION     : int ub_p64_connect(con, tfmat)
**			Print a connector entity.
**    PARAMETERS   
**       INPUT  : 
**          con						connector entity
**       OUTPUT :  none
**    RETURNS: UU_SUCCESS iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_p64_connect(con)
	struct UB_conector_rec *con;
{
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us,"ub_p64_connect(con>key=%d)", con->key));
	sprintf(UB_sbuf, "CONNECT: key=%x", con->key);
	ubi_pscroll(UB_sbuf);

	sprintf(UB_sbuf, "  pline=%x", con->pline);
	ubi_pscroll(UB_sbuf);
	if (ubi_print_list(con->ainst,con->no_ainst,"%d",sizeof(UU_KEY_ID),7)
				!= UU_SUCCESS) goto failed;
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION: int ub_switch_inst_on_connect(conkey,oldinstkey, newintstkey)
**			Replace all old instance keys (OLDINSTKEY) on a 
**			connector (CONKEY) with a new instance key (NEWINSTKEY)
**    PARAMETERS   
**       INPUT  : 
**				conkey				key of connector entity
**				oldinstkey			key of old instance entity
**				newinstkey			key of new instance entity
**       OUTPUT :  none
**    RETURNS: UU_SUCCESS iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_switch_inst_on_connect(conkey, oldinstkey, newinstkey)
	UU_KEY_ID conkey;
	UU_KEY_ID oldinstkey;
	UU_KEY_ID newinstkey;
{
	struct UB_conector_rec con;
	int i, status;
	UU_LOGICAL notify;
	uu_denter(UU_BTRC,(us,
		"ub_switch_inst_on_connect(conkey=%d,newinstkey=%d,oldinstkey=%d)",
		conkey, oldinstkey, newinstkey));

	if (ur1_notifyAssocOfUpdates(conkey, &notify) != UU_SUCCESS) goto failed;
	con.key = conkey;
	status = uc_retrieve_data(&con, sizeof(con));
	if (status == UU_SUCCESS)
	{
		for (i=0; i<con.no_ainst; i++)
			if (con.ainst[i] == oldinstkey) con.ainst[i] = newinstkey;
		ur_delete_data_varlist(con.key, 1);
		ur_update_data_varlist(con.key, 1, con.ainst, 1, con.no_ainst); 
	}
	if (ur1_notifyAssocOfUpdates(conkey, &notify) != UU_SUCCESS) goto failed;
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ub_transf64_connect(con, tfmat, store)
**       Transform a connector (CON) by the specified transformation
**			(TFMAT).
**    PARAMETERS   
**       INPUT  : 
**          con						connector entity
**				tfmat						transformation matrix
**				store						UU_TRUE => store transformed entity
**       OUTPUT :  none
**    RETURNS: UU_SUCCESS iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_transf64_connect(con, tfmat, store)
	struct UB_conector_rec *con;
	UM_transf tfmat;
	UU_LOGICAL store;
{
	struct UM_polyline_rec pline;
	char *argv[1];
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us,"ub_transf64_connect(con>key=%d, tfmat=%x, store=%d)",
		con->key, tfmat, store));

	pline.key = con->pline;
	status = uc_retrieve_data(&pline, sizeof(pline));
	if (status == UU_SUCCESS) 
		status = uc_transform(&pline, tfmat, store);
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

