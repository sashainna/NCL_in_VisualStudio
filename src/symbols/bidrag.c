/*********************************************************************
**    NAME:  bidrag.c
**       CONTAINS:
**      		ubu_drag_syminstance 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bidrag.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:03
*********************************************************************/

#include "usysdef.h"		/* for UU_REAL, etc */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "mdcoord.h"		/* for debugging trace facility */
#include "dinput.h"		/* for UD_LOCATOR */
#include "dasnog.h"		/* for UD_RUBBER used in dragging */
#include "dmark.h"		/* for reseting rubber banding, MARK and UNMARK */
#include "view.h"			/* for viewing data types */
#include "drubber.h"		/* for dragging macros */
#include "bsym.h"

UU_LOGICAL	ud_qlocpet();

#define TRACE UU_FALSE 	/* for debugging only */
/*********************************************************************
**    E_FUNCTION: int ubu_drag_syminstance(symptr, tfmat, draginfo, keyptr, 
**								dragonptr,centerview, dsegidptr)
**       This function creates a symbol instance to drag.
**    PARAMETERS   
**       INPUT  : 
**			symptr		pointer to the master symbol record to instance.
**			tfmat			transformation to orient instances; note, the
**							translation portion of the transformation will
**							be changed according to the placing of each
**							instance, it should have no translation component.
**       OUTPUT :  
**			draginfo		pointer to the data structure containing the
**							dragging information.
**			keyptr		pointer to the UNIBASE key of dragging instance.
**			dragonptr	pointer to logical which is UU_TRUE iff a segment
**							is being dragged.
**			centerview	Coordinate of the center of the view to be dragged in.
**			dsegidptr	Pointer to the display segment to drag (if any).
**    RETURNS: UU_SUCCESS if no errors encountered, UB_FAILURE otherwise;
**				 note, a graphics device that does not support dragging is
**				 not considered a reason to return UB_FAILURE.
**    SIDE EFFECTS: creates a segment to be dragged if the graphics
**						device supports dragging.
**    WARNINGS: none
*********************************************************************/
int ubu_drag_syminstance(symptr, tfmat, draginfo, keyptr,dragonptr,
						viewcenter, dsegidptr)
	struct UB_symbol_rec *symptr;
	UU_REAL tfmat[4][3];
	UD_RUBBER *draginfo;
	UU_KEY_ID *keyptr;
	UU_LOGICAL *dragonptr;
	UM_coord viewcenter;
	int *dsegidptr;
{
	struct UB_instance_rec instance_rec;
	struct UB_symattr_rec attr;	/* for retrieving symbol attributes */
	struct UB_inst_rec instdata;
	UV_vport vport;
	UV_view  view;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us,
		"ubu_drag_syminstance(symptr->key:%d,tfmat:%x,?,?,?,?,?)",
					symptr->key, tfmat));

	/* determine if device supports dragging */
	if(UV_act_screen[0].nvports>1)
	{
		*dragonptr = UU_FALSE;
		goto done;
	}

	if (ud_qlocpet(2, 21)) /* 2=device, 21=dragging */
	{  /* supports dragging */
		*dragonptr = UU_TRUE;

		/*****
		 * let user pick view port to drag in *
		if (uvu_pickvp(&vport) != UU_SUCCESS)
			goto failed;

		uv_getvid(vport.cur_view, &view);

		/* set normalization transformation for the view port * 
		gsnormtran(vport.xform);
		*****/

		uv_symbol_draging_view_info(viewcenter);
		/* set the symbol attribute bundle field so that instance is blanked */
/*		UB_symattr_default_bundle.blanked = UU_TRUE; see below two TDG */

		if (ubu_fillin_disp_instance(symptr, tfmat, /*view.cur_ref_pt*/viewcenter,
							UU_TRUE, &instdata) != UU_SUCCESS) goto failed;

		*keyptr = instdata.inst_key; /* pass back key to instance to be dragged */

		/* now reset symbol attribute bundle to attribute defaults */
/* */		if (ub_set_sym_attr(-1, &UB_symattr_default_bundle) != UU_SUCCESS)
			goto failed;
   		
		/* get display segment for instance to be dragged */
		if (ur_retrieve_disp_segid(instdata.inst_key, dsegidptr) != 0)
		{
			uu_uerror2(UB_SYMBOL, 38, instdata.inst_key, "ubu_drag_syminstance");
			/* error message: unable to retrieve display segment id 
			 * for key=%d, (%s) */
			goto failed;
		}
 
#if (TRACE)
		uu_denter2(UU_BTRC,(us, "just before ud_stdrag, draginfo:%x, dsegid:%d",
										draginfo, dsegid));
		uu_dexit;
#endif

		/* now make sure we are in locator mode */
		ud_dtcord(UD_LOCATOR, 1, 1);
	}
	else
		*dragonptr = UU_FALSE;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	


