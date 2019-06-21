/*********************************************************************
**
**    NAME         :  netoler.c
**
**       CONTAINS:
**				nclc_selectsystem()
**				nclc_selectunits()
**				nclc_input_mode()
**				nclc_cam_thick()
**				nclc_cam_toler()
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			netoler.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:08:54
*********************************************************************/
#include "usysdef.h"
#include "lcom.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nclcmd.h"
#include "udforms.h"
#include "nclinp.h"
#include "mdattr.h"
#include "mattr.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mdraw.h"

/*********************************************************************
**    E_FUNCTION : nclc_selectsystem
**		Will display a popup mmenu with choices of 
**			"NCLCAM" and "DRAWING", choose to enter different
**			mode													
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclc_selectsystem()
{
	int choice, status;
/*
.....popup an internal menu with "NCLCAM" & "DRAWING"
*/
	status = ncl_popup(NCL_SYSTEM_CHOICE, &choice);
      
	if(status == NCL_OKINPUT)
	{
		if (choice==1)
/*
......NCLCAM
*/
		{
			if (UM_2d3d_mode == UM_2D)
			{	
				udm_exit_drawing();
			}
		}
		else if (choice==2)
		{
			if (UM_2d3d_mode != UM_2D)					
				udm_enter_drawing();
		}		
	}
/*
.....display in status area
*/
	uz_actsystem();
}

/*********************************************************************
**    E_FUNCTION : nclc_selectunits
**		Will display a popup mmenu with choices of 
**			"Inches" and "Millimeters", choose to set different
**			units													
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclc_selectunits()
{
	int choice, status;

	status = ncl_popup(NCL_SELECT_UNITS, &choice);
      
	if(status == NCL_OKINPUT)
	{
		if (choice==1)
/*
......INCHES
*/
		{
			if (UM_cpln.length_unit==UM_MM)
				nclu_units(0);
		}
		else if (choice==2)
		{
			if (UM_cpln.length_unit==UM_INCH)
				nclu_units(1);
		}		
	}
}

/*********************************************************************
**    E_FUNCTION : nclc_input_mode
**		Will display a popup mmenu with choices of 
**			"Insert" and "Overwrite", choose to set different
**			text input mode													
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclc_input_mode()
{
	int choice, status;

	status = ncl_popup(NCL_TEXT_MODE, &choice);
      
	if(status == NCL_OKINPUT)
	{
		if (choice==1)
/*
......Insert
*/
		{
			nclu_cmd_edit(5);
		}
		else if (choice==2)
/*
......Overstrike
*/
		{
			nclu_cmd_edit(6);
		}	
	}
}

/*********************************************************************
**    E_FUNCTION : nclc_cam_toler
**		Output TOLER command												
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclc_cam_toler()
{
	int status;
	char lstr[80];
	UM_int2 sc;
	static UM_real8 rval[2];
	static int *ans[2];
	NCL_cmdbuf cmdbuf;
	UU_REAL anyreal;
	char ct_scalar[65], pt_scalar[65];
/*
.....Set up default answers
*/
	sc = 27;
	getsc(&sc,&(rval[0]));
	sc = 168;
	getsc(&sc,&(rval[1]));

	anyreal = rval[0];
	ncl_sprintf(ct_scalar, &anyreal,1);
	anyreal = rval[1];
	ncl_sprintf(pt_scalar, &anyreal,1);
	ans[0] = (int*)ct_scalar;
	ans[1] = (int*)pt_scalar;
/*
.....Get the Form input
*/
	status = ud_form("ntoler.frm",ans,ans);
	if (status==-1)
		return -1;
/*
.....Initialize NCL command buffers
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lstr,"TOLER/");
/*
//	anyreal =rval[0];
//	ncl_sprintf(ostr,&anyreal,1);
//	strcat(lstr,ostr);
//	strcat(lstr,",");
//	anyreal = rval[1];
//	ncl_sprintf(ostr,&anyreal,1);
//	strcat(lstr,ostr);
*/
	strcat(lstr, ct_scalar);
	strcat(lstr,",");
	strcat(lstr, pt_scalar);
/*
.....Output the TOLER command
*/
	ncl_add_token(&cmdbuf,lstr,UU_FALSE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....End of routine
*/
	return 0;
}

/*********************************************************************
**    E_FUNCTION : nclc_cam_thick
**		Output THICK command												
**    PARAMETERS   
**       INPUT  :  None
**       OUTPUT :  None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclc_cam_thick()
{
	int i, status;
	UM_int2 sc;
	char lstr[80];
	static UM_real8 rval[7];
	static int *ans[7];
	NCL_cmdbuf cmdbuf;
	char thk_scalar[7][65];
/*
.....Set up default answers
*/
/*
.....Set up default answers
*/
	sc = 23;
	getsc(&sc,&(rval[0]));
	sc = 24;
	getsc(&sc,&(rval[1]));
	sc = 25;
	getsc(&sc,&(rval[2]));
	sc = 177;
	getsc(&sc,&(rval[3]));
	sc = 178;
	getsc(&sc,&(rval[4]));
	sc = 179;
	getsc(&sc,&(rval[5]));
	sc = 180;
	getsc(&sc,&(rval[6]));

	sprintf(thk_scalar[0], "%f", rval[0]);
	ans[0]  = (int *)thk_scalar[0];
	sprintf(thk_scalar[1],"%f",rval[1]);
	ans[1]  = (int *)thk_scalar[1];
	sprintf(thk_scalar[2], "%f", rval[2]);
	ans[2]  = (int *)thk_scalar[2];
	sprintf(thk_scalar[3],"%f",rval[3]);
	ans[3]  = (int *)thk_scalar[3];
	sprintf(thk_scalar[4], "%f", rval[4]);
	ans[4]  = (int *)thk_scalar[4];
	sprintf(thk_scalar[5],"%f",rval[5]);
	ans[5]  = (int *)thk_scalar[5];
	sprintf(thk_scalar[6],"%f",rval[6]);
	ans[6]  = (int *)thk_scalar[6];
/*
.....Get the Form input
*/
	status = ud_form("nthick.frm",ans,ans);
	if (status==-1)
		return -1;
/*
.....Initialize NCL command buffers
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lstr,"THICK/");
/*
	anyreal =rval[0];
	ncl_sprintf(ostr,&anyreal,1);
	strcat(lstr,ostr);
	strcat(lstr,",");
	anyreal = rval[1];
	ncl_sprintf(ostr,&anyreal,1);
	strcat(lstr,ostr);
	strcat(lstr,",");
	anyreal = rval[2];
	ncl_sprintf(ostr,&anyreal,1);
	strcat(lstr,ostr);

	strcat(lstr,",");
	anyreal = rval[3];
	ncl_sprintf(ostr,&anyreal,1);
	strcat(lstr,ostr);
	strcat(lstr,",");
	anyreal = rval[4];
	ncl_sprintf(ostr,&anyreal,1);
	strcat(lstr,ostr);
	strcat(lstr,",");
	anyreal = rval[5];
	ncl_sprintf(ostr,&anyreal,1);
	strcat(lstr,ostr);
	strcat(lstr,",");
	anyreal = rval[6];
	ncl_sprintf(ostr,&anyreal,1);
	strcat(lstr,ostr);
*/
	for (i=0; i<6;i++)
	{
		strcat(lstr,thk_scalar[i]);
		strcat(lstr,",");
	}
	strcat(lstr,thk_scalar[6]);
/*
.....Output the THICK command
*/
	ncl_add_token(&cmdbuf,lstr,UU_FALSE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....End of routine
*/
	return 0;
}

