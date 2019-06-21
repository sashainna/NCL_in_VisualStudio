/*********************************************************************
**    NAME         :  numotion.c
**       CONTAINS:
**			nclu_cutter
**			nclu_multax
**			nclu_cut
**			nclu_cut_copy
**			nclu_erase_motion
**			nclu_disp_cut
**  		nclu_scrub
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       numotion.c , 25.5
**    DATE AND TIME OF LAST  MODIFICATION
**       01/11/16 , 08:55:08
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "drubber.h"

#include "mdrel.h"
#include "mfort.h"
#include "ncl.h"
#include "nclfc.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"

/*********************************************************************
**    E_FUNCTION     : ncl_multax()
**       Create an NCL MULTAX statement.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_multax(choice)
int choice;
	{
	NCL_cmdbuf cmdbuf;

	uu_denter( UU_MTRC,(us,"ncl_multax()"));

	ncl_init_cmdbuf(&cmdbuf);

	switch (choice)
			{
			case 1:
				ncl_add_token(&cmdbuf, NCL_multax_on, NCL_nocomma);
			break;
	
			case 2:
				ncl_add_token(&cmdbuf, NCL_multax_off, NCL_nocomma);
			break;
			}
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : nclu_contct()
**       Create an NCL CONTCT statement.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_contct(choice)
int choice;
	{
	NCL_cmdbuf cmdbuf;

	ncl_init_cmdbuf(&cmdbuf);

	switch (choice)
			{
			case 1:
				ncl_add_token(&cmdbuf, NCL_contct_on, NCL_nocomma);
			break;
	
			case 2:
				ncl_add_token(&cmdbuf, NCL_contct_off, NCL_nocomma);
			break;
			}
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
	}

/*********************************************************************
**    E_FUNCTION     : ncl_cut(form)
**       Create a cut statement.
**    PARAMETERS   
**       INPUT  : 
**          form						form number
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_cut(form)
	int form;
	{
	NCL_cmdbuf  cmdbuf;
	UU_LOGICAL status;
	char local[20];
	int length;

	uu_denter( UU_MTRC,(us,"ncl_cut()"));

	status = NCL_OKINPUT;
	switch(form)
		{
		case 1:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_cut1, NCL_nocomma);
			break;

		case 2:
			ncl_init_cmdbuf(&cmdbuf);
			strcpy(local, NCL_dntcut);
			length = strlen(local);
			local[length-1] = '\0';
			ncl_add_token(&cmdbuf, local, NCL_nocomma);
			break;

		case 3:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_dntcut, NCL_nocomma);
			ncl_add_token(&cmdbuf, NCL_nomore, NCL_nocomma);
			break;

		case 4:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_tracut, NCL_nocomma);
			status = ncl_add_str(&cmdbuf, 416, NCL_nocomma);
			break;

		case 5:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_tracut, NCL_nocomma);
			ncl_add_token(&cmdbuf, NCL_nomore, NCL_nocomma);
			break;

		case 6:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_index, NCL_nocomma);
			status = ncl_add_str(&cmdbuf, 417, NCL_nocomma);
			break;

		case 7:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_index, NCL_nocomma);
			status = ncl_add_str(&cmdbuf, 417, NCL_comma);
			if(status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_nomore, NCL_nocomma);
			break;
		}
	if(status == NCL_OKINPUT || status == NCL_DONE)
		{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ncl_cut_copy(form)
**       Create a cut copy statement.
**    PARAMETERS   
**       INPUT  : 
**          form						form number
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_cut_copy(form)
	int form;
	{
	NCL_cmdbuf  cmdbuf;
	UU_LOGICAL status;

	uu_denter( UU_MTRC,(us,"ncl_cut_copy()"));
	status = NCL_OKINPUT;
	switch(form)
		{
		case 1:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_copy1, NCL_nocomma);
			status = ncl_add_str(&cmdbuf, 417, NCL_comma);
			if(status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_same, NCL_comma);
			if(status == NCL_OKINPUT)
				status = ncl_add_str(&cmdbuf, 418, NCL_nocomma);
			break;

		case 2:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_copy1, NCL_nocomma);
			status = ncl_add_str(&cmdbuf, 417, NCL_comma);
			if(status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_modify, NCL_comma);
			if(status == NCL_OKINPUT)
				status = ncl_add_str(&cmdbuf, 416, NCL_comma);
			if(status == NCL_OKINPUT)
				status = ncl_add_str(&cmdbuf, 418, NCL_nocomma);
			break;

		case 3:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_copy1, NCL_nocomma);
			status = ncl_add_str(&cmdbuf, 417, NCL_comma);
			if(status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_transl, NCL_comma);
			if(status == NCL_OKINPUT)
				status = ncl_add_str(&cmdbuf, 419, NCL_comma);
			if(status == NCL_OKINPUT)
				status = ncl_add_str(&cmdbuf, 418, NCL_nocomma);
			break;

		case 4:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_copy1, NCL_nocomma);
			status = ncl_add_str(&cmdbuf, 417, NCL_comma);
			if(status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_xyrot, NCL_comma);
			if(status == NCL_OKINPUT)
				status = ncl_add_angle(&cmdbuf, 408, NCL_comma);
			if(status == NCL_OKINPUT)
				status = ncl_add_str(&cmdbuf, 418, NCL_nocomma);
			break;

		case 5:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_copy1, NCL_nocomma);
			status = ncl_add_str(&cmdbuf, 417, NCL_comma);
			if(status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_yzrot, NCL_comma);
			if(status == NCL_OKINPUT)
				status = ncl_add_angle(&cmdbuf, 408, NCL_comma);
			if(status == NCL_OKINPUT)
				status = ncl_add_str(&cmdbuf, 418, NCL_nocomma);
			break;

		case 6:
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf, NCL_copy1, NCL_nocomma);
			status = ncl_add_str(&cmdbuf, 417, NCL_comma);
			if(status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_zxrot, NCL_comma);
			if(status == NCL_OKINPUT)
				status = ncl_add_angle(&cmdbuf, 408, NCL_comma);
			if(status == NCL_OKINPUT)
				status = ncl_add_str(&cmdbuf, 418, NCL_nocomma);
			break;
		}
	if(status == NCL_OKINPUT || status == NCL_DONE)
		{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_erase_motion()
**       Erase motion.
**    PARAMETERS   
**       INPUT  : 
**          none			
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_erase_motion()
	{
	NCL_cmdbuf  cmdbuf;
	UU_LOGICAL edit_mode;
	UU_LOGICAL ncl_get_edit_option();

	uu_denter( UU_MTRC,(us,"ncl_erase_motion()"));
	ncl_init_cmdbuf(&cmdbuf);
	ncl_add_token(&cmdbuf, NCL_erase_motion, NCL_nocomma);
	ncl_add_cmdbuf(&cmdbuf);
	edit_mode = ncl_get_edit_option();
	if(edit_mode) ncl_edit_option();
	ncl_call(&cmdbuf);
	if(edit_mode) ncl_edit_option();
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_disp_cut()
**       Display cutter.
**    PARAMETERS   
**       INPUT  : 
**          none			
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_disp_cut()
	{
	NCL_cmdbuf  cmdbuf;

	uu_denter( UU_MTRC,(us,"ncl_disp_cut()"));
	ncl_init_cmdbuf(&cmdbuf);
	ncl_add_token(&cmdbuf, NCL_disply, NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_show_cutter, NCL_nocomma);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_scrub()
**       Create an NCL scrub statement.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_scrub()
	{
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL status, pt_status;
	int i;

	uu_denter( UU_MTRC,(us,"ncl_scrub()"));
	ncl_init_cmdbuf(&cmdbuf);
	ncl_add_token(&cmdbuf, NCL_scrub, NCL_nocomma);
	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 428, UD_ncl_motion);
	if(status == NCL_OKINPUT)
		status = ncl_add_str(&cmdbuf, 429, NCL_comma);
	if(status == NCL_OKINPUT)
		{
		i=0;
		pt_status = NCL_OKINPUT;
		while(i<4 && pt_status == NCL_OKINPUT)
			{
			pt_status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 402, UD_ncl_pt);
			i++;
			}
		}
	if(status == NCL_OKINPUT || status == NCL_DONE)
		{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		}
	uu_dexit;
	}
