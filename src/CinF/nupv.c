/*********************************************************************
**    NAME         :  nupv.c
**       CONTAINS: User interface routines for point creation.
**       nclu_pv_xy()
**       nclu_pv_pt_pt()
**       nclu_pv_pt_ve()
**       nclu_pv_ln()
**       nclu_pv_te_tlaxis()
**       nclu_pv_te_fwd()
**       nclu_pv_ce_ci()
**       nclu_pv_io_pl_pl_pl()
**       nclu_pv_cv_dist()
**       nclu_pv_pv_sign_ve()
**       nclu_pv_pv_cross_ve()
**       nclu_pv_unit_pv()
**       nclu_pv_perpto_pl()
**       nclu_pv_perpto_geo()
**       nclu_pv_pv_times_num()
**       nclu_pv_pv_offset_num()
**       nclu_pv_patern()
**		 	nclu_pv_proj_sf()
**		 	nclu_pv_fit()
**		 	nclu_pv_revsf()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nupv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:13
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
#include "mdcoord.h"
#include "mdclass.h"

#include "nccs.h"
#include "nclfc.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "nclcmd.h"
#include "nclmodals.h"

#include "mdunits.h"
#include "ncl.h"

/*********************************************************************
**    E_FUNCTION     : nclu_pv_xy()
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
void nclu_pv_xy(ifl)
int ifl;

{
	NCL_cmdbuf cmdbuf;
	int status,nxy,first,numint,j,i,len;
	char str[80];
	UD_SCA_VALUE zval;
	UD_DASCORD vc;
	UM_coord pt;
	UD_NDCLOCREC tmp;

   uu_denter(UU_MTRC,(us,"nclu_pv_xy()"));

	nxy = 2; if (ifl == 1) nxy = 3;
	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
		{
			status = ncl_add_name(&cmdbuf, 1);
			if (status == NCL_DONE) goto done;
		}
		ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
		first = UU_TRUE;
/*
.....Get Z-level
*/
		if (ifl == 1)
		{
			status = ud_ldas(UD_SCADISTANCE, UA_NCL, 3, &zval, 1,
				&numint, UD_NODEFAULT);
			if (numint <= 0) goto done;
		}
/*
.....Get point coordinates
*/
		while (UU_TRUE)
		{
/*
.....If not autolabel, then get label
*/
			if (!first)
			{
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
				if (status == NCL_DONE) goto done;
				ncl_add_token(&cmdbuf,NCL_pv,NCL_nocomma);
				first = UU_FALSE;
			}

/*			ud_ldas(UD_DASCART,UM_MODEL, 35, &tmp,
				1, &numint, UD_NODEFAULT);
*/
			ud_ldas(UD_SCACART,/*coordinate of point*/UM_MODEL, 35, &tmp, 
				 	1, &numint, UD_NODEFAULT);
			if (numint <= 0) break;
			for(j=0; j<3; j++) pt[j] = tmp.cord[j];
			if (ifl == 1) pt[2] = zval.value;
/*
.....Get vector
*/
			ud_ldas(UD_SCAVEC, UA_NCL, 119, &vc, 1, &numint, UD_NODEFAULT);
/*
.....Place definition into source file
*/
			if (numint > 0)
			{
				ncl_set_cmdmode(UU_TRUE);

				i = 0; len = strlen(tmp.label);
				while (tmp.label[i]==' ') i++;
				if (tmp.label[i]!='\0') 
				{
					strcpy(str, &(tmp.label[i]));
					if (ifl == 1)
					{
/*
.....add or replace the z value
*/
						ncl_replace_cordstr(str, zval.label, 3);
					}
				}
				else
					ncl_cctostr(nxy, pt, str);
				ncl_add_token(&cmdbuf,str,NCL_comma);
		
				i = 0; len = strlen(vc.label);
				while (vc.label[i]==' ') i++;
				if (vc.label[i]!='\0') 
					strcpy(str, &(vc.label[i]));
				else
					ncl_vctostr(vc.cord,str);
				ncl_add_token(&cmdbuf,str,NCL_nocomma);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
/*
.....If not autolabel, then get label
*/
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
				{
					status = ncl_add_name(&cmdbuf,1);
					if (status == NCL_DONE) goto done;
				}
				ncl_add_token(&cmdbuf,NCL_pv,NCL_nocomma);
			}
		}
		if (ifl == 0) goto done;
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_pt_pt()
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
void nclu_pv_pt_pt()

{
NCL_cmdbuf cmdbuf;
int status;

	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
		status = ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
/*
.....Get first point
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 216, UD_ncl_pt);
		if (status != NCL_OKINPUT) goto done;
/*
.....Get second point
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 123, UD_ncl_pt);
/*
.....Process command
*/
		if (status == NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_pt_ve()
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
void nclu_pv_pt_ve()

{
NCL_cmdbuf cmdbuf;
int status;

	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
		status = ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
/*
.....Get first point
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 216, UD_ncl_pt);
		if (status != NCL_OKINPUT) goto done;
/*
.....Get second point
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 218, UD_ncl_vepv);
/*
.....Process command
*/
		if (status == NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_ln()
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
void nclu_pv_ln()
{
	NCL_cmdbuf cmdbuf;
	int status;
	while (UU_TRUE)
	{
/*
.....Initialize command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
		status = ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
/*
.....Get line
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 217, UD_ncl_pvln);
		if (status != NCL_OKINPUT) goto done;
/*
.....Process command
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_te_tlaxis()
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
void nclu_pv_te_tlaxis()
{
	NCL_cmdbuf cmdbuf;
	int status;
/*
.....Initialize the command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
	status = NCL_OKINPUT;
	if (!NCL_auto_label)
		status = ncl_add_name(&cmdbuf, 1);
	if (status == NCL_DONE) goto done;
/*
.....Output the command
*/
	ncl_set_cmdmode(UU_TRUE);
	ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
	status = ncl_add_token(&cmdbuf, NCL_te, NCL_comma);
	status = ncl_add_token(&cmdbuf, NCL_ve_tlaxis, NCL_nocomma);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_te_fwd()
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
void nclu_pv_te_fwd()
{
	NCL_cmdbuf cmdbuf;
	int status;
/*
.....Initialize the command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
	status = NCL_OKINPUT;
	if (!NCL_auto_label)
		status = ncl_add_name(&cmdbuf, 1);
	if (status == NCL_DONE) goto done;
/*
.....Output the command
*/
	ncl_set_cmdmode(UU_TRUE);
	ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
	status = ncl_add_token(&cmdbuf, NCL_te, NCL_comma);
	status = ncl_add_token(&cmdbuf, NCL_fwd, NCL_nocomma);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_ce_ci()
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
void nclu_pv_ce_ci()
{
	int numint;                   /* number of DAS items returned */
	int status;
	UM_PLOCREC pick;
	NCL_cmdbuf cmdbuf;
/*
.....Limit the geometry pick
*/
	ud_lgeo(UU_TRUE, UD_ncl_ci);
	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
/*
.....Get circle
*/
		ua_dl_pldas(UD_DASPCKLOC,/*pick circle*/UA_NCL,17,&pick,1,&numint,1);
		if (numint <= 0) goto done;
/*
.....Place point definition into source file
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_token(&cmdbuf,NCL_pv,NCL_nocomma);
		ncl_add_token(&cmdbuf,NCL_center,NCL_comma);
		ncl_add_token(&cmdbuf,pick.ploc.label,NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_io_pl_pl_pl()
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
void nclu_pv_io_pl_pl_pl()
{
	NCL_cmdbuf cmdbuf;
	int status;
	char str[80],pl1[80],pl2[80],pl3[80];

	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
		if (status == NCL_DONE) goto done;
/*
.....Get first plane
*/
rep1:;
		status = ncl_get_dlabel(UD_DASPCKLOC,pl1,219,UD_ncl_pl);
		if (status == NCL_DONE || status == NCL_NOINPUT) goto done;
/*
.....Get second plane
*/
rep2:;
		status = ncl_get_dlabel(UD_DASPCKLOC,pl2,18,UD_ncl_pl);
		if (status == NCL_DONE || status == NCL_NOINPUT) goto rep1;
/*
.....Get second plane
*/
rep3:;
		status = ncl_get_dlabel(UD_DASPCKLOC,pl3,19,UD_ncl_pl);
		if (status == NCL_DONE || status == NCL_NOINPUT) goto rep2;
/*
.....Get direction
*/
		status = ncl_get_modifier(str, NCL_XYZ_DIRECTION);
		if (status == NCL_DONE || status == NCL_NOINPUT) goto rep3;
/*
.....Place point definition into source file
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_token(&cmdbuf,NCL_pv,NCL_nocomma);
		ncl_add_token(&cmdbuf,NCL_intof,NCL_comma);
		ncl_add_token(&cmdbuf,pl1,NCL_comma);
		ncl_add_token(&cmdbuf,pl2,NCL_comma);
		ncl_add_token(&cmdbuf,pl3,NCL_comma);
		ncl_add_token(&cmdbuf,str,NCL_comma);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_cv_dist()
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
void nclu_pv_cv_dist()
{
	int numint;
	int status;
	UM_PLOCREC pick;
	UU_LOGICAL first;
	NCL_cmdbuf cmdbuf;
	UD_SCA_VALUE u;

	first = UU_TRUE;
	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (first)
		{
			if (!NCL_auto_label)
			{
				status = ncl_add_name(&cmdbuf,1);
				if (status != NCL_OKINPUT) goto done;
			}
		}
/*
.....Get curve
*/
		ud_lgeo(UU_TRUE, UD_ncl_allcv);
		um_dl_pldas(UD_DASPCKLOC, /*pick curve to evaluate */ UM_MODEL, 326,
			&pick, 1, &numint, 2);
		if (numint <= 0) goto done;

		while (UU_TRUE)
		{
/*
.....If not autolabel, then get label
*/
			if (!first)
			{
				if (!NCL_auto_label)
				{
					status = ncl_add_name(&cmdbuf,1);
					if (status != NCL_OKINPUT) break;
				}
			}
			first = UU_FALSE;
/*
.....Get distance along curve
*/
			ud_ldas(UD_SCADISTANCE, UA_NCL, 224, &u, 1, &numint, 2);
			if (numint <= 0) break;
/*
....Create command
*/
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_token(&cmdbuf,NCL_pv,NCL_nocomma);
			ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
/*			ncl_sprintf(buf,&u,1); */
			ncl_add_token(&cmdbuf,u.label,NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_pv_sign_ve()
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
void nclu_pv_pv_xxx_vesca(ifl)
int ifl;
{
	NCL_cmdbuf cmdbuf;
	int status;
	static int ipr[5] = {610, 610, 612, 612, 612};
	static char *imod[5] = {NCL_plus, NCL_minus, NCL_cross, NCL_times, NCL_offset};

	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
		status = ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
/*
.....Get first pointvector
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, ipr[ifl-1], UD_ncl_pv);
		if (status != NCL_OKINPUT) goto done;
/*
.....Add modifier
*/
		ncl_add_token(&cmdbuf,imod[ifl-1],NCL_comma);
/*
.....Get second point
*/
		if (ifl < 4)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 218, UD_ncl_vepv);
		else
			status = ncl_add_length(&cmdbuf, 223);
/*
.....Process command
*/
		if (status == NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_unit_pv()
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
void nclu_pv_unit_pv()
{
	NCL_cmdbuf cmdbuf;
	int status;
	while (UU_TRUE)
	{
/*
.....Initialize command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
		status = ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_unit, NCL_comma);
/*
.....Get pointvector
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 612, UD_ncl_pv);
		if (status != NCL_OKINPUT) goto done;
/*
.....Process command
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_perpto_pl()
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
void nclu_pv_perpto_pl(option)
int option;
{
	NCL_cmdbuf cmdbuf;
	int status;
	while (UU_TRUE)
	{
/*
.....Initialize command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
		status = ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
/*
.....If option = 1 then the command is PV/pt,perpto,pl.
.....Otherwise the command is PV/perpto,pl.  JLS 4/14/99
*/
		if (option ==1)
		{
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 216, UD_ncl_ptpv);
			if (status != NCL_OKINPUT) goto done;
		}
		status = ncl_add_token(&cmdbuf, NCL_perpto, NCL_comma);
/*
.....Get plane
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 219, UD_ncl_pl);
		if ((status != NCL_OKINPUT)&&(option ==0)) goto done;
/*
.....Process command
.....Since adding the option, make sure that input is okay before
.....procession the command.  JLS 4/14/99
*/
		if (status ==NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_perpto_geo()
**       User interface for PV/{pt1|pv1},{TANTO|PERPTO}[,ON|OFF],geo
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pv_perpto_geo()
{
	NCL_cmdbuf cmdbuf;
	int status, choice, relnum;
	char elabel[256], nlabel[256];

	while (UU_TRUE)
	{
/*
.....Initialize command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
		ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
/*
.....Get point or pointvector
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 216, UD_ncl_ptpv);
		if (status != NCL_OKINPUT) goto done;
/*
.....Get TANTO or PERPTO
*/
		status = ncl_popup(NCL_PV_TANTO_PERPTO,&choice);
		if (status == NCL_OKINPUT)
		{
			if (choice == 1)
				status = ncl_add_token(&cmdbuf, NCL_tanto, NCL_comma);
			else
				status = ncl_add_token(&cmdbuf, NCL_perpto, NCL_comma);
		}
/*
.....Get ON or OFF
*/
		if (status == NCL_OKINPUT)
			status = ncl_popup(NCL_PV_ON_OFF,&choice);

		if (status == NCL_OKINPUT)
		{
			if (choice == 1)
				status = ncl_add_token(&cmdbuf, NCL_on, NCL_comma);
			else
				status = ncl_add_token(&cmdbuf, NCL_off, NCL_comma);
		}
/*
.....Get geometry
*/
		if (status == NCL_OKINPUT)
			status = ncl_add_label_nrpt(238, UD_ncl_pvttpe, elabel,
			         nlabel, &relnum);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, elabel, NCL_comma);

		if (status == NCL_OKINPUT && relnum != NCL_PLN_REL)
			status = ncl_add_token(&cmdbuf, nlabel, NCL_nocomma);
/*
.....Process command
*/
		if (status ==NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_patern()
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
void nclu_pv_patern()
{
	NCL_cmdbuf cmdbuf;
	int status,num,numint;
	char str[80];
	UM_PLOCREC pick;
/*
....Limit geometry picks
*/
	ud_lgeo(UU_TRUE, UD_ncl_patern);
	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....If not autolabel, then get label
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
		if (status == NCL_DONE) goto done;
/*
.....Get Patern
*/
		ua_dl_pldas(UD_DASPCKLOC,/*pick circle*/UA_NCL,450,&pick,1,&numint,1);
		if (numint <= 0) goto done;
		num = um_get_pickkey(&pick.pent,2);
		sprintf(str,"%d",num);
/*
.....Place point definition into source file
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_token(&cmdbuf,NCL_pv,NCL_nocomma);
		ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
		ncl_add_token(&cmdbuf,str,NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_proj_sf()
**      Make a point vector by projecting a point onto a surface.
**      The vector part is the surface normal at that point.  It 
**      is on the same side of the surface as the projecting pt.
**      The user can supply an optional projection vector.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pv_proj_sf()
{
	int status, prompt, projvec, relnum, i;
	NCL_cmdbuf cmdbuf;
	char pt_label[80], surf_label[80], nrpt_str[80];
	static char projct_lab [7] = "PROJCT";
	UM_PLOCREC pick;
	int pick_mask[UD_NMENTWD], *mask;

	
	for (i=0; i<UD_NMENTWD; i++)
	{
		pick_mask[i] = 0;
		pick_mask[i] = pick_mask[i] | UD_solid[i];
		pick_mask[i] = pick_mask[i] | UD_ncl_allsf[i];
	}
	mask = (int *)pick_mask;
	while (UU_TRUE)
	{
/*
..... Have user pick surface to project onto.  Get sf and near pt.
*/
		prompt = 712;   /* Pick a surface */
		status = ncl_add_label_nrpt(prompt, mask, surf_label, nrpt_str, &relnum);
		if (status != NCL_OKINPUT) break;
/*
..... Have user pick projection vector (optional).  Get label.
*/
		prompt = 61;    /* Enter vector to project along (optional). */
		ud_lgeo (UU_TRUE, UD_ncl_vepv);
		um_dl_pldas(UD_DASPCKLOC, UM_APPGEO, prompt, &pick, 1, &projvec, 2);
		while (UU_TRUE)
		{
/*
.....Initialize the NCL command buffer
*/
			ncl_init_cmdbuf(&cmdbuf);
/*
..... Have user pick a point/pointvector.  Get label. If none, stop loop.
*/
			prompt = 489;    /* Pick a point or point-vector */
			status = ncl_get_dlabel(UD_DASPCKLOC, pt_label, prompt, UD_ncl_ptpv);
			if (status != NCL_OKINPUT) break;
/*
.....Place point definition into source file
.....Format is   pt/projct,point[,projection vector],surface,(nearpt)
*/
			ncl_set_cmdmode (UU_TRUE);
			ncl_add_token(&cmdbuf,NCL_pv,NCL_nocomma);
			ncl_add_token(&cmdbuf,projct_lab,NCL_comma);
			ncl_add_token(&cmdbuf, pt_label ,NCL_comma);
			if(projvec == 1)
				ncl_add_token (&cmdbuf, pick.ploc.label, NCL_comma);
			ncl_add_token(&cmdbuf, surf_label ,NCL_comma);
			ncl_add_token(&cmdbuf, nrpt_str ,NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_fit()
**       create apoint vector at the center of a  cv/sf
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pv_fit()
{
	NCL_cmdbuf cmdbuf;
	int numint,status;
	UM_PLOCREC pick;
	UU_KEY_ID um_get_pickkey();

	uu_denter(UU_MTRC,(us,"nclu_pv_fit"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on and if not prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
		if (status == NCL_DONE) goto done;
/*
.....Prompt user for cv/sf.
*/
		ud_lgeo(UU_TRUE,UD_cvsf);
		um_dl_pldas(UD_DASPCKLOC, /*pick cv/sf */ UA_NCL, 248,
			&pick, 1, &numint, 2);
		if (numint <= 0) goto done;
/*
.....Put PV/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
/*
.....Put CENTER into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_center, NCL_comma);
/*
.....Put picked entity into command.
*/
		ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);

		if ((status == NCL_NOINPUT)) goto done;
/*
.....Prompt user to enter a scalar variable.
*/
		status = ncl_add_str(&cmdbuf, 234, NCL_nocomma);
/*
.....Process command.
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
   uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pv_revsf()
**      Make a point vector along the axis of a revsurf

**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pv_revsf()
{
	int status;
	NCL_cmdbuf cmdbuf;
	char str[256];
	struct NCL_fixed_databag sf;
	struct NCL_trimsf_rec surf;
	UM_int2 primtyp;
	int rel_num = 0;
	char ptlab[256];
	UU_KEY_ID skey;
		
	while (UU_TRUE)
	{
prompt:
/*
.....Initialize commadn buffer and status.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....If auto label is off, prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);

		ncl_add_token(&cmdbuf, NCL_pv, NCL_nocomma);
		
/*
.....Select surface.
*/
		status = ncl_get_dlabel_rel(str,ptlab,UU_FALSE,205,UD_ncl_netentity,&skey,
			&rel_num);
		sf.key = skey;
		ncl_retrieve_data_fixed (&sf);
		if (rel_num == NCL_TRIMSF_REL)
		{
			surf.key = skey;
			status = ncl_retrieve_data_fixed(&surf);
			sf.key = surf.bs_key;
			status = ncl_retrieve_data_fixed(&sf);
		 }

		 ncl_get_sf_primtyp(&sf.key,&primtyp);
		 if(primtyp != NCLSF_CYLINDER && primtyp !=NCLSF_CONE &&
			sf.rel_num != NCL_REVSURF_REL)
		 {
			 ud_wrerr("The entity is not a cone/cylinder or a surface of revolution. Retry. [More info in HELP.]");
			 goto prompt;
		 }

/*
.....If done is selected, exit
*/
		if (status != NCL_OKINPUT) goto done;
/*
......Put the surface into the command buffer.
*/
		ncl_add_token(&cmdbuf,str,NCL_comma);

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
done:
	uu_dexit;
}

