/*********************************************************************
**    NAME         :  nurefsys.c
**       CONTAINS: User interface routines for refsys manipulation.
**			nclu_refsys()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nurefsys.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:14
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
#include "mdcoord.h"
#include "modef.h"

#include "nccs.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "nclcmd.h"
#include "nclmodals.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : nclu_refsys(option, flag, label)
**       description
**    PARAMETERS   
**       INPUT  : 
**          option					0 => prompt the user for method to
**													set refsys
**											1 => set refsys to wp
**											2 => set refsys to nomore
**			flag:	for opton = 1, if = 1: Original point
**									if = 2, X-axis is changed
**									if = 3, Y-axis is changed
**									if = 4, Z-axis is changed
**			label: PICKING ENTITY label for Orig/Axis changes
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_refsys(option, flag, axis_label)
int option, flag;
char *axis_label;
	{
	NCL_cmdbuf cmdbuf;
	int i, status;
	int choice;
	UM_coord origin;
	UM_vector xaxis;
	UM_vector yaxis;
	UM_vector zaxis;
	char buf[256];

	uu_denter(UU_MTRC,(us,"nclu_refsys(option=%d)", option));
	if (axis_label!=NULL)
	{
		i = strlen(axis_label);
		while ((i>0) && (axis_label[i-1]==' ')) i--;
		axis_label[i] = '\0';
	}
	ncl_init_cmdbuf(&cmdbuf);

	ncl_add_token(&cmdbuf, NCL_refsys, NCL_nocomma);

	if (option == 0)
		status = ncl_popup(NCL_REFSYS, &choice);
	else if (option == 1)
		{
		status = NCL_OKINPUT;
		choice = 3;
		}
	else if (option == 2)
		{
		status = NCL_OKINPUT;
		choice = 2;
		}
	else if (option == 4)
		{
		status = NCL_OKINPUT;
		choice = 4;
		}

	if (status == NCL_OKINPUT) switch (choice)
		{
		case 1:
			status = ncl_add_str(&cmdbuf, 181, NCL_nocomma);
			break;
		case 2:
			status = ncl_add_token(&cmdbuf, NCL_nomore, NCL_nocomma);
			break;
		case 3:
			ncl_add_token(&cmdbuf, "(", NCL_nocomma);
			ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
			um_getcpln(origin, xaxis, yaxis, zaxis);
			ncl_wcstomcs (0, origin, origin);
			ncl_wcstomcs (1, xaxis, xaxis);
			ncl_wcstomcs (1, yaxis, yaxis);
			unitcv(origin);
			if ((axis_label==NULL) || (axis_label[0]=='\0') || (flag!=1))
			{
				ncl_add_token(&cmdbuf, "(PT/", NCL_nocomma);
				ncl_fmtreal (3, origin, buf);
				ncl_add_token(&cmdbuf, buf, NCL_nocomma);
				ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
				ncl_add_cmdbuf(&cmdbuf);
			}
			else
			{
				ncl_add_token(&cmdbuf, axis_label, NCL_comma);
			}
			if ((axis_label==NULL) || (axis_label[0]=='\0') || (flag!=2))
			{
				ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
				ncl_fmtreal (3, xaxis, buf);
				ncl_add_token(&cmdbuf, buf, NCL_nocomma);
				ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
				ncl_add_cmdbuf(&cmdbuf);
			}
			else
			{
				ncl_add_token(&cmdbuf, axis_label, NCL_comma);
			}
			if ((axis_label==NULL) || (axis_label[0]=='\0') || (flag!=3))
			{
				ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
				ncl_fmtreal (3, yaxis, buf);
				ncl_add_token(&cmdbuf, buf, NCL_nocomma);
				ncl_add_token(&cmdbuf, ") )", NCL_nocomma);
			}
			else
			{
				ncl_add_token(&cmdbuf, axis_label, NCL_nocomma);
				ncl_add_token(&cmdbuf, ")", NCL_nocomma);
			}
			break;
		case 4:
			ncl_add_token(&cmdbuf, "(", NCL_nocomma);
			ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
			ncl_add_token(&cmdbuf, "(PT/", NCL_nocomma);
			um_getcpln(origin, xaxis, yaxis, zaxis);
			ncl_wcstomcs (0, origin, origin);
			ncl_wcstomcs (1, xaxis, xaxis);
			ncl_wcstomcs (1, yaxis, yaxis);
      		unitcv(origin);
			ncl_fmtreal (3, origin, buf);
			ncl_add_token(&cmdbuf, buf, NCL_nocomma);
			ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);

			ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
			ncl_fmtreal (3, xaxis, buf);
			ncl_add_token(&cmdbuf, buf, NCL_nocomma);
			ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);

			ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
			ncl_fmtreal (3, yaxis, buf);
			ncl_add_token(&cmdbuf, buf, NCL_nocomma);
			ncl_add_token(&cmdbuf, ") )", NCL_nocomma);
			break;
		default:
			status = NCL_NOINPUT;
			break;
		}

	if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		}

	uu_dexit;
	}
