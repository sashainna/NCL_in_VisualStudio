/*********************************************************************
**    NAME         :  lipvcolor.c
**       CONTAINS:
**				ul_ipv_tool_color()
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvcolor.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:12
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "lcom.h"
#include "lipv.h"
#include "mfort.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nclfile.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"

static int mainmarkval=0;

#define FCUT 0
#define FCTR 1
#define FHLD 2
#define FSHK 3
#define FFXT 4
#define FHCL 5
#define FRCL 6
#define FAUT 7
#define FUST 8
#define FUFX 9

/*********************************************************************
**    S_FUNCTION     :  OnColor(filedno, val, stat)
**       Method called at when Cut Color toggle is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnColor(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	if (*fieldno!=0) return(UD_FLDOK);
/*
.....Set field traversal flags
.....Based on cut color
*/
	i = *(val->frmint);
	if (i == -1)
	{
		ud_set_traverse_mask(FAUT,UU_TRUE);
		ud_set_traverse_mask(FUST,UU_TRUE);
		ud_set_traverse_mask(FUFX,UU_TRUE);
	}
	else
	{
		ud_set_traverse_mask(FAUT,UU_FALSE);
		ud_set_traverse_mask(FUST,UU_FALSE);
		ud_set_traverse_mask(FUFX,UU_FALSE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_tool_color()
**       Processes the NCLIPV Simulation colors form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ul_ipv_tool_color()
{
	int status;
/*
.....Set up form fields
*/
	static int tog[10];
	static char traverse[]     = {1,1,1,1, 1,1,1, 1,1,1};
	static UD_METHOD methods[] = {OnColor,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL, UU_NULL};
	static char called[]       = {6,6,6,6, 6,6,6, 6,6,6};
	static char display[] = {1,1,1,1, 1,1,1, 1,1,1};
	static int *ans[] = {&tog[0],&tog[1],&tog[2],&tog[3],&tog[4],&tog[5],
		&tog[6],&tog[7],&tog[8],&tog[9]};
/*
.....Command Reject
*/
	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		UD_UNMARK (mainmarkval);
		return(UU_SUCCESS);
	}
/*
.....Setup form defaults
*/
	tog[FCUT] = LW_default_tool.cut_color;
	tog[FCTR] = LW_default_tool.color;
	tog[FHLD] = LW_default_tool.hold_color;
	tog[FSHK] = LW_default_tool.shank_color;
	tog[FFXT] = LW_clash_material.fixture;
	tog[FHCL] = LW_clash_material.holder;
	tog[FRCL] = LW_clash_material.rapid;
	tog[FAUT] = LW_default_tool.initial;
	tog[FUST] = LW_default_tool.use_stock[0];
	tog[FUFX] = LW_default_tool.use_stock[1];
/*
.....Setup field traversal
*/
	if (LW_default_tool.cut_color == -1)
	{
		traverse[FAUT] = 1;
		traverse[FUST] = 1;
		traverse[FUFX] = 1;
	}
	else
	{
		traverse[FAUT] = 0;
		traverse[FUST] = 0;
		traverse[FUFX] = 0;
	}
/*
.....Get the Form input
*/
form:;
	status = ud_form1("ipvcolors.frm", ans, ans, methods, called, display, traverse);
	if (status==-1)
		goto done;
/*
.....Save the form data
*/
	LW_default_tool.cut_color = tog[FCUT];
	LW_default_tool.color = tog[FCTR];
	LW_default_tool.hold_color = tog[FHLD];
	LW_default_tool.shank_color = tog[FSHK];
	LW_clash_material.fixture = tog[FFXT];
	LW_clash_material.holder = tog[FHCL];
	LW_clash_material.rapid = tog[FRCL];
	LW_default_tool.initial = tog[FAUT];;
	LW_default_tool.use_stock[0] = tog[FUST];
	LW_default_tool.use_stock[1] = tog[FUFX];
/*
.....Setup the initial color index
*/
	if (LW_default_tool.cut_color == 0)
	{
		LW_cutcolor_index = -1;
		ul_ipv_set_colors();
	}
		
/*
.....End of routine
*/
done:;
	UD_UNMARK(mainmarkval);
	return(UU_SUCCESS);
}
