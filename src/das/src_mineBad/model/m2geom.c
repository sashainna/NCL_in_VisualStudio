/*********************************************************************
**    NAME         :  m2geom
**       CONTAINS:
**			um_delete_last()
**			umi_print_transformation(tfmat)
**			um_print_attr_bundle(attrptr)
**
**    vp 12.8.95 routine umi_print_transf (still dummy!) has been moved
**               to m2dba.c file to satisfy IGES link.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2geom.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:47
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mdcoord.h"
#include "mattr.h"
#include "modef.h"
#include "mdebug.h"
#include "mderror.h"	/* for UM_FAILURE */

/*********************************************************************
**    E_FUNCTION     : um_delete_last()
**       Delete the last created entity.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_delete_last()
	{
	UU_KEY_ID key;
	int status;

	uu_denter(UU_MTRC,(us,"um_delete_last()"));
	status = ur_get_last_modified_mkey(&key);
	if (status != -1)
		{
		ur_enable_del_stack() ;
		uc_delete(key);
		ur_disable_del_stack() ;
		}
	uu_dexit;
   }

/*********************************************************************
**    I_FUNCTION :  umi_print_transformation(tfmat)
**       Prints a transform to psfile.
**    PARAMETERS   
**       INPUT  : 
**          tfmat			transformation to print.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umi_print_transformation(tfmat)
	UM_transf tfmat;

	{
	sprintf(UM_sbuf,"TRANSFORMATION: %g, %g, %g", 
		tfmat[0][0],tfmat[0][1],tfmat[0][2]);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf,"                %g, %g, %g",
		tfmat[1][0],tfmat[1][1],tfmat[1][2]);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf,"                %g, %g, %g",
		tfmat[2][0],tfmat[2][1],tfmat[2][2]);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf,"                %g, %g, %g",
		tfmat[3][0],tfmat[3][1],tfmat[3][2]);
	um_pscroll(UM_sbuf);
	}

/*********************************************************************
**    I_FUNCTION     : um_print_attr_bundle(attrptr)
**       Print the contents of an attribute bundle.
**    PARAMETERS   
**       INPUT  : 
**          attrptr				pointer to attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_print_attr_bundle(attrptr)
	struct UM_attrdata_rec *attrptr;

	{
	uu_denter(UU_MTRC,(us,"um_print_attr_bundle(%x)",attrptr));

	sprintf(UM_sbuf,"ATTRIBUTE: rel=%d, key=%d",attrptr->rel_num, attrptr->key);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PINT,"color",1,&attrptr->color);
	um_p_ary(UM_PINT,"layer",1,&attrptr->layer);
	um_p_ary(UM_PINT,"pen",1,&attrptr->pen);
	um_p_ary(UM_PINT,"line_style",1,&attrptr->line_style);
	um_p_ary(UM_PFLOAT,"line_width",1,&attrptr->line_width);
	uu_dexit;
	}

