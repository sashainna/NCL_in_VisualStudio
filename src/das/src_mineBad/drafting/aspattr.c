#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "class.h"
#include "adrfcom.h"
#include "mdrel.h"
#include "atext.h"
#include "bsym.h"

/*********************************************************************
**    NAME         :  aspattr.c
**       CONTAINS:
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       aspattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:39
*********************************************************************/
extern UA_linear_modal;
#define NO_DEFAULT 0
#define DEFAULT 1
/*********************************************************************
**    E_FUNCTION     : ua_set_sp_attr(formfile)
**       Set dimension attributes using forms package.
**    PARAMETERS   
**       INPUT  : 
**          formfile				form file name
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_sp_attr(formfile)
	char	*formfile;
	
	{
	int status, rel_num;
	UU_LOGICAL init;
	UU_KEY_ID  key;
	struct UC_entitydatabag eptr;
	struct UA_txt_rec *note;         
	struct UB_instance_rec *sym;         

	static int save_ovrd, dims_disp, lin_modal, over_ride;
	static int  *ans[] = {&dims_disp, &over_ride, &lin_modal};

	uu_denter(UU_MTRC,(us,"ua_set_sp_attr()"));

	dims_disp = UA_dims_disp; 
	save_ovrd = UA_text_box_ovrd;
	over_ride = save_ovrd;
	lin_modal = UA_linear_modal;

	status = ud_form(formfile, ans, ans);
	if (status==-1)
	   return -1;

	UA_dims_disp			=		dims_disp;
	UA_text_box_ovrd		=		over_ride;
	UA_linear_modal		=		lin_modal;


/****************************************/
/*   test if text display has changed   */
/****************************************/

	if( save_ovrd != UA_text_box_ovrd )
		{
		/* do dimension text */
		init = UU_TRUE;
		rel_num = UA_LINEAR_DIM;
		while(uv_getalldispinrelation(init, rel_num, &key) == UU_SUCCESS)
			{
			eptr.key = key;
			eptr.rel_num = rel_num;
			status = uc_retrieve_data(&eptr, sizeof(struct UC_entitydatabag));
			if( status == UU_SUCCESS)
				{
				uc_display(&eptr);
				}
			init = UU_FALSE;
			}

		/* now do note text */
		init = UU_TRUE;
		rel_num = UA_TEXT_REL;
		while(uv_getalldispinrelation(init, rel_num, &key) == UU_SUCCESS)
			{
			eptr.key = key;
			eptr.rel_num = rel_num;
			status = uc_retrieve_data(&eptr, sizeof(struct UC_entitydatabag));
			if( status == UU_SUCCESS)
				{
				note = ( struct UA_txt_rec *) &eptr;         
				if(note->arckey == 0) uc_display(&eptr);
				}
			init = UU_FALSE;
			}

		/* now do symbol instances text */
		init = UU_TRUE;
		rel_num = UB_INSTANCE_REL;
		while(uv_getalldispinrelation(init, rel_num, &key) == UU_SUCCESS)
			{
			eptr.key = key;
			eptr.rel_num = rel_num;
			status = uc_retrieve_data(&eptr, sizeof(struct UC_entitydatabag));
			if( status == UU_SUCCESS)
				{
				sym = ( struct UB_instance_rec *) &eptr;         
				if(sym->no_text_nod != 0) uc_display(&eptr);
				}
			init = UU_FALSE;
			}
		}
	uu_dexit;
	}
