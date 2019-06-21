
/*********************************************************************
**    NAME         :  m2ulabel.c
**       CONTAINS: user interface label manipulation routines
**			int umu_get_label(rel_num, label)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2ulabel.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:49
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdclass.h"
#include "class.h"
#include "mdrel.h"
#include "mlab.h"
#include "mdcoord.h"
#include "mdebug.h"
#include "nclmodals.h"
#include "nclinp.h"

/*********************************************************************
**    E_FUNCTION     : int umu_get_label(rel_num, label)
**       Return a LABEL for the specified relation (REL_NUM). The
**			label is seven characters plus the null terminator (left
**			justified) and is determined by the value of a global
**			flag UM_label_mode:
**				1. UM_AUTO_LABEL =>	calcuate the next available label
**											for the specified relation
**				2. UM_USER_LABEL =>	prompt the user for a label
**				3. UM_NO_LABEL   =>	do nothing (presumably the label
**											has already been set)
**    PARAMETERS   
**       INPUT  : 
**          rel_num				relation number to get label for
**										(used in UM_AUTO_LABEL mode)
**       OUTPUT :  
**          label					label (size is seven characters 
**										plus null)
**    RETURNS      : 
**			UU_SUCCESS	iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umu_get_label(rel_num, label)
	int rel_num;
	char *label;

	{
	int numint;
	int status;

	uu_denter(UU_MTRC, (us,"umu_get_label(rel_num=%d)", rel_num));

	status = UU_SUCCESS;
	switch (UM_label_mode)
		{
		case UM_AUTO_LABEL:
			status = um_auto_label(rel_num, label);
			break;
		case UM_NO_LABEL:
			break;
		case UM_USER_LABEL:
			ud_ldas(UD_DASSTRING,/* enter drawing name */ UM_MODEL, 227,
				label, 7, &numint, UD_DEFAULT);
			if (numint <= 0) status = UU_FAILURE;
			break;
		default:
			status = UU_FAILURE;
			break;
		}

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : umu_toggle_labels
**       Turn labels on or off.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_toggle_labels()
	{
	int	status;
	int	choice;
	int	numint;
	UU_LOGICAL	initialize;
	UU_LOGICAL	labels_on;
	struct UC_entitydatabag	e;

	uu_denter(UU_MTRC,(us,"umu_toggle_label()"));

	/* save current label on/off status */
	labels_on = NCL_display_label;

	/* inquire user on or off */
	status = ncl_popup(NCL_LABEL_ON_OFF, &choice);
	if (status == NCL_OKINPUT)
		{
		switch (choice)
			{
			case 1:
				NCL_display_label = UU_TRUE;
				break;
			case 2:
				NCL_display_label = UU_FALSE;
				break;
			}

		/* pick entities to change label status */
		ud_ldas(UD_DASSELECT,/*pick entity*/UM_MODEL,223, UU_NULL,1,
			  	&numint,UD_NODEFAULT);
		initialize = UU_TRUE;
		while(ud_gnxt(initialize, UU_NULL, &e.key, 1) == UU_TRUE)
			{
			initialize = UU_FALSE;
			uc_retrieve_data(&e, sizeof(e));
			/* display	*/
			uc_display(&e);
			}
		}

		/* reset the NCL_display_label global */
		NCL_display_label = labels_on;
		uu_dexit;
	}
