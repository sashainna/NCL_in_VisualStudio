/*********************************************************************
**    NAME         : nemotstk1.c
**       CONTAINS:
**          ul_ipv_mot_stack_modals()
**
**    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvmotstk1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:15
*********************************************************************/
#include "usysdef.h"
#include "stdio.h"
#include "lcom.h"
#include "nclmplay.h"
#include "lipv.h"
#include "lipvstack.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"

static int Ssize;
static UU_LOGICAL Sactive,Sfixture;

static UD_FSTAT OnTog();
static void S_save_modals();

/*********************************************************************
**    E_FUNCTION    : int ul_ipv_mot_stack_modals()
**       Navigate the NCLIPV Undo Stack Modals form.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      :  none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mot_stack_modals()
{
	char buf[80],tmp[80];
	int i, status, stat,activ;
	char *ux_getenv();
	UX_pathname fname;
	FILE *fptr;

	static int *ans[] = {&Sactive, &Ssize, &Sfixture};
	static UD_METHOD methods[] = {OnTog, UU_NULL, UU_NULL};
	static char called[] = {6, 6, 6};
	static char traverse[] = {1, 1, 1};
/*
.....Set up default values
*/
	Sactive = activ = LW_mot_stack_active;
	Ssize = LW_mot_stack_size;
	Sfixture = LW_mot_stack_fixture;
/*
.....Set up field traverse flags
*/
	if (Sactive)
		traverse[1] = traverse[2] = 1;
	else
		traverse[1] = traverse[2] = 0;
/*
.....Traverse form:
*/
	status = ud_form1("ipvstack.frm",ans,ans,methods,called,UU_NULL,traverse);
	if (status==-1)
		return ;
/*
.....Save modals file
*/
	S_save_modals();
/* 
.....Propogate changes
*/
	LW_mot_stack_active = Sactive;
	LW_mot_stack_size = Ssize;
	LW_mot_stack_fixture = Sfixture;
/*
.....Reset/start the stack
.....if the active state has changed
*/
	if (Sactive != activ) ul_ipv_mot_stack_init();
}

/*********************************************************************
**    I_FUNCTION     :  OnTog(fieldno, val, stat)
**       Method called when a toggle field is changed.
**    PARAMETERS   
**       INPUT  : 
**          fieldno	Field number being changed.
**          val		Current field value.
**          stat		Field status.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form traverse mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/* 
.....Set appropriate field traversals
*/

	switch(*fieldno) 
	{
	case 0:
		if (Sactive)
		{
			ud_set_traverse_mask(1, UU_TRUE);
			ud_set_traverse_mask(2, UU_TRUE);
		}
		else
		{
			ud_set_traverse_mask(1, UU_FALSE);
			ud_set_traverse_mask(2, UU_FALSE);
		}
		break;
	}
	ud_default_method(fieldno, val, stat);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  S_save_modals()
**       Method called at each change/nochange toggle field
**			in the pick modals form. (pikmod.frm)
**    PARAMETERS   
**       INPUT  : 
**          fieldno	Field number being changed.
**          val		Current field value.
**          stat		Field status.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form traverse mask.
**    WARNINGS     : none
*********************************************************************/
static void S_save_modals()
{
	int stat;
	char buf[80];
	UX_pathname fname;
	FILE *fptr;
/*
.....Open modals file
*/
	strcpy(fname,"nclipv_undo.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,
		fname,3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) return;
/*
.....Store modals
*/
	ux_fputs0("#UNDO#\n", fptr);

	if (Sactive)
		ux_fputs0("/ACTIVE/ *YES\n", fptr);
	else
		ux_fputs0("/ACTIVE/ *NO\n", fptr);

	sprintf(buf,"/SIZE/ %d\n",Ssize);
	ux_fputs0(buf, fptr);

	if (Sfixture)
		ux_fputs0("/FIXTURE/ *YES\n", fptr);
	else
		ux_fputs0("/FIXTURE/ *NO\n", fptr);
/*
.....Close the modals file
*/	
	ux_fclose0 (fptr);
}
