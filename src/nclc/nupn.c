/*********************************************************************
**    NAME         :  nupn.c
**       CONTAINS: User interface routines for patern creation.
**               nclu_pn_pt_ptve
**               nclu_pn_pt_ve_incr
**               nclu_pn_ci
**               nclu_pn_ci_incr
**               nclu_pn_pn_ve
**               nclu_pn_pn_ve_incr
**               nclu_pn_ptpn_ptpn
**     MODULE NAME AND RELEASE LEVEL 
**       nupn.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:11
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "modef.h"

#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"

/*********************************************************************
**    E_FUNCTION     : nclu_pn_pt_ptve()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pn_pt_ptve()
{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_pn_pt_ptve()"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on, if not, prompt user for label.
*/
		if (!NCL_auto_label) status= ncl_add_name(&cmdbuf,1);
/*
.....Put PATERN/LINEAR, into command.
*/
		ncl_add_token(&cmdbuf, NCL_pn, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_linear, NCL_comma);
/*
.....Prompt user for a point or a point-vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 465, UD_ncl_ptpv);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for second point or point vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 457, UD_ncl_ptpve);

/*
.....Prompt user for number of points in patern.
*/
		if (status == NCL_OKINPUT)
			status = ncl_add_length(&cmdbuf, 458);
/*
.....Process command.
*/		
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pn_pt_ve_incr()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pn_pt_ve_incr()
{
	NCL_cmdbuf cmdbuf;
	int status;
	int choice;
	int flag;

	uu_denter(UU_MTRC,(us,"nclu_pn_pt_ve_incr()"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer, status, and flag.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
		flag=0;
/*
.....Check to see if auto label is on, if not, prompt user for label.
*/
		if (!NCL_auto_label) status= ncl_add_name(&cmdbuf,1);
/*
.....Put PATERN/LINEAR, into command.
*/
		ncl_add_token(&cmdbuf, NCL_pn, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_linear, NCL_comma);
/*
.....Prompt user for a point or point-vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 465, UD_ncl_ptpv);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for a vector or point-vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 218, UD_ncl_vepv);

		if (status == NCL_OKINPUT)
		{
			while(status == NCL_OKINPUT)
			{
				status = ncl_popup(NCL_PATERN_INCR,&choice);
				switch (choice)
				{
					case 0:
/*
.....If done was selected and this is the first time being here
.....leave status as is, if not set status to NCL_OKINPUT.
*/
						if (flag !=0)
							status = NCL_OKINPUT;
						else
							status = NCL_NOINPUT;
						goto done1;
						break;
					case 1:
/*
.....This is INCR.
*/
						ncl_add_token(&cmdbuf, NCL_incr, NCL_comma);
						status = ncl_add_str(&cmdbuf, 470, NCL_comma);
						flag =1;
						break;
					case 2:
/*
.....This is INCR ... AT.
*/
						ncl_add_token(&cmdbuf, NCL_incr, NCL_comma);
						status = ncl_add_length(&cmdbuf, 468, NCL_comma);
						if (status == NCL_OKINPUT)
						{
							ncl_add_token(&cmdbuf, NCL_at, NCL_comma);
							status = ncl_add_length(&cmdbuf, 471, NCL_nocomma);
						}
						flag =1;
						break;
					default:
						break;
				}
			}
		}
done1:;
/*
.....Process command.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pn_ci()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pn_ci()
{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_pn_ci()"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on, if not, prompt user for label.
*/
		if (!NCL_auto_label) status= ncl_add_name(&cmdbuf,1);
/*
.....Put PATERN/ARC, into command.
*/
		ncl_add_token(&cmdbuf, NCL_pn, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_arc, NCL_comma);
/*
.....Prompt user for a circle.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 220, UD_ncl_ci);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for starting angle.
*/
		status = ncl_add_length(&cmdbuf, 459);

		if (status == NCL_OKINPUT)
/*
.....Prompt user for ending angle.
*/
			status = ncl_add_length(&cmdbuf, 460);

		if (status == NCL_OKINPUT)
/*
.....Prompt user for CLW or CCLW
*/
			status = ncl_add_modifier(&cmdbuf, NCL_CLWCCW_OP);

/*
.....Prompt for number of points.
*/
		if (status == NCL_OKINPUT)
			status = ncl_add_length(&cmdbuf, 458);
/*
.....Process command.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pn_ci_incr()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pn_ci_incr()
{
	NCL_cmdbuf cmdbuf;
	int status;
	int choice;
	int flag;

	uu_denter(UU_MTRC,(us,"nclu_pn_ci_incr()"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer, status and flag.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
		flag = 0;
/*
.....Check to see if auto label is on, if not, prompt user for label.
*/
		if (!NCL_auto_label) status= ncl_add_name(&cmdbuf,1);
/*
.....Put PATERN/ARC, into command.
*/
		ncl_add_token(&cmdbuf, NCL_pn, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_arc, NCL_comma);
/*
.....Prompt user for a circle.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 220, UD_ncl_ci);
/*
.....If done was selected, exit
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for starting angle.
*/
		status = ncl_add_length(&cmdbuf, 459);

/*
.....Display popup menu for CLW or CCLW
*/
		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_CLWCCW_OP);

		if (status == NCL_OKINPUT)
		{
 			while(status == NCL_OKINPUT)
			{
/*
.....Display popup menu for incrementing.
*/
				status = ncl_popup(NCL_PATERN_INCR,&choice);
				switch (choice)
				{
/*
.....If done was selected and this is the first time we were
.....here, exit with NCL_NOINPUT.  If we have been through here
.....before, then we do have information so status should equal
.....NCL_OKINPUT.
*/
					case 0:
						if (flag !=0)
							status= NCL_OKINPUT;
						else
							status = NCL_NOINPUT;
						goto done1;
						break;
/*
.....INCR was selected.
*/
					case 1:
						ncl_add_token(&cmdbuf, NCL_incr, NCL_comma);
						status = ncl_add_str(&cmdbuf, 467, NCL_comma);
						flag = 1;
						break;
/*
.....INCR ... AT was selected.
*/
					case 2:
						ncl_add_token(&cmdbuf, NCL_incr, NCL_comma);
						status = ncl_add_length(&cmdbuf, 468, NCL_comma);
						flag = 1;
						if (status == NCL_OKINPUT)
						{
							ncl_add_token(&cmdbuf, NCL_at, NCL_comma);
							status = ncl_add_length(&cmdbuf, 469, NCL_nocomma);
						}
						break;
					default:
						break;
				}
			}
		}
done1:;
/*
.....Process command.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pn_pn_ve()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pn_pn_ve()
{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_pn_pn_ve()"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on, if not, prompt user for label.
*/
		if (!NCL_auto_label) status= ncl_add_name(&cmdbuf,1);
/*
.....Put PATERN/PARLEL, into command.
*/
		ncl_add_token(&cmdbuf, NCL_pn, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_parlel, NCL_comma);
/*
.....Prompt user to pick a patern.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 450, UD_ncl_patern);
/*
.....If user selected done, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for a vector or a point-vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 218, UD_ncl_vepv);

/*
.....Get number of points to make.
*/
		if (status == NCL_OKINPUT)
			status = ncl_add_length(&cmdbuf, 464);
/*
.....Process the command.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pn_pn_ve_incr()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pn_pn_ve_incr()
{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_pn_pn_ve_incr()"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on, if not, prompt user for label.
*/
		if (!NCL_auto_label) status= ncl_add_name(&cmdbuf,1);
/*
.....Put PATERN/PARLEL, into command.
*/
		ncl_add_token(&cmdbuf, NCL_pn, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_parlel, NCL_comma);
/*
.....Prompt user to pick a patern.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 450, UD_ncl_patern);
/* 
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for a vector or point-vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 218, UD_ncl_vepv);

		if (status == NCL_OKINPUT)
		{
/*
.....Get either INCR or INCR ... AT.
*/
			status = ncl_add_modifier(&cmdbuf, NCL_PATERN_INCR);
			if (status == NCL_OKINPUT)
			{
				while (ncl_add_modifier(&cmdbuf, NCL_PATERN_INCR) != NCL_DONE);
			}
		}
/*
.....Process command.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pn_ptpn_ptpn()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pn_ptpn_ptpn()
{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_pn_ptpn_ptpn()"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....If auto label is off, prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put PATERN/RANDOM, into command buffer.
*/
		ncl_add_token(&cmdbuf, NCL_pn, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_random, NCL_comma);
/*
.....Prompt for a point, point-vector or a patern
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 451, UD_ncl_ptpvpn);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt for second point, point-vector, or patern.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 451, UD_ncl_ptpvpn);
/*
.....Get additional point, point-vectors, or paterns.
*/
		if (status == NCL_OKINPUT)
		{
			while (status != NCL_NOINPUT && status != NCL_DONE)
			{
				status = ncl_add_label(UD_DASPCKLOC, &cmdbuf,451,UD_ncl_ptpvpn);
			}
			status = NCL_OKINPUT;
		}
/*
.....Process command.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
	uu_dexit;
}
