/*********************************************************************
**	FILENAME: lipvlight.c
**	CONTAINS:
**				ul_ipv_light_form
**				ul_ipv_init_lights
**				ul_ipv_set_lights
**				ul_ipv_destroy_lights
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvlight.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:13
*********************************************************************/

#include "usysdef.h"
#include "lipv.h"
#include "lumb.h"
#include "mdcpln.h"
#include "ustdio.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "udfdata.h"
#include "udforms.h"

#define FLGT 0
#define FACT 1
#define FTYP 2
#define FCOL 3
#define FRGB 4
#define FINT 5
#define FPOS 6
#define FPOX 7
#define FDIR 8
#define FAPY 9

void ul_ipv_set_lights();

static UD_FSTAT OnLight();
static UD_FSTAT OnActive();
static UD_FSTAT OnType();
static UD_FSTAT OnColor();
static UD_FSTAT OnPos();
static UD_FSTAT OnApply();

static void S_copy_lights();
static UU_LOGICAL S_compare_lights();
static void S_set_traversal();
static int S_save_modfile();

static int Slight,Sactlight=0;
static LW_light_struc Slgt,Slights[LW_MAX_LIGHT];
static LtSref Slight_list=0;

extern char uw_color_name[64][96];
/*********************************************************************
**	 E_FUNCTION : ul_ipv_light_form()
**			This function handles the NCLIPV Lights form.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS:    none.
**	 SIDE EFFECTS:  none.
**	 WARNINGS: none.
*********************************************************************/
void ul_ipv_light_form()
{
	int i,j,status,idum;
	static int *ans[10];
	static UD_METHOD methods[] = {OnLight,OnActive,OnType,OnColor,UU_NULL,
		UU_NULL,OnPos,UU_NULL,UU_NULL,OnApply};
	static char called[] = {6,6,6, 6,6,6, 6,6,6, 6};
	char traverse[10];
	static char display[] = {1,1,1, 1,1,1, 1,1,1, 1};
/*
.....Save current light settings
*/
	Slight = Sactlight;
	S_copy_lights(LW_lights,Slights,LW_MAX_LIGHT);
	S_copy_lights(&Slights[Slight],&Slgt,1);
/*
.....Initialize fields
*/
	UM_cc_exttoint(Slgt.rgb,Slgt.rgb);
	UM_cc_exttoint(Slgt.direction,Slgt.direction);
/*
.....Setup the default answers
*/
	ans[FLGT] = &Slight;
	ans[FACT] = &Slgt.active;
	ans[FTYP] = (int *)&Slgt.type;
	ans[FCOL] = &Slgt.color;
	ans[FRGB] = (int *)&Slgt.rgb;
	ans[FINT] = (int *)&Slgt.intensity;
	ans[FPOS] = &idum;
	ans[FPOX] = (int *)&Slgt.position;
	ans[FDIR] = (int *)&Slgt.direction;
	ans[FAPY] = &idum;
/*
.....Set traverse flags
*/
	for (i=0;i<10;i++) traverse[i] = 1;
	traverse[FPOS] = LW_active;
	if (!Slgt.active)
	{
		for (i=FTYP;i<=FDIR;i++) traverse[i] = 0;
	}
	else if (Slgt.type == LW_LIGHT_AMBIENT || Slgt.type == LW_LIGHT_EYE)
		traverse[FPOS] = traverse[FPOX] = traverse[FDIR] = 0;
	else if (Slgt.type == LW_LIGHT_POINT)
		traverse[FDIR] = 0;
	if (Slgt.color != -1) traverse[FRGB] = 0;
/*
.....Get the Form input
*/
	status = ud_form1("ipvlight.frm",ans,ans,methods,called,display,traverse);
	if (status == -1) goto done;
/*
.....Use the new lights if they have changed
*/
	UM_cc_inttoext(Slgt.rgb,Slgt.rgb);
	UM_cc_inttoext(Slgt.direction,Slgt.direction);
	S_copy_lights(&Slgt,&Slights[Slight],1);
	for (i=0;i<LW_MAX_LIGHT;i++)
	{
		for (j=0;j<3;j++)
		{
			if (Slights[i].rgb[j] < 0.) Slights[j].rgb[j] = 0.;
			if (Slights[i].rgb[j] > 1.) Slights[j].rgb[j] = 1.;
		}
	}
	if (!S_compare_lights(Slights,LW_lights,LW_MAX_LIGHT))
	{
		S_copy_lights(Slights,LW_lights,LW_MAX_LIGHT);
		ul_ipv_set_lights(LW_lights);
	}
	Sactlight = Slight;
/*
.....Save the Display modals file
*/
	S_save_modfile();
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    S_FUNCTION     :  OnLight(filedno, val, stat)
**       Method called when the active Light is changed.
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
static UD_FSTAT OnLight(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Copy the current form fields
.....into the current light structure
*/
	UM_cc_inttoext(Slgt.rgb,Slgt.rgb);
	UM_cc_inttoext(Slgt.direction,Slgt.direction);
	S_copy_lights(&Slgt,&Slights[Sactlight],1);
	Slight = Sactlight = *(val->frmint);
	S_copy_lights(&Slights[Slight],&Slgt,1);
	UM_cc_exttoint(Slgt.rgb,Slgt.rgb);
	UM_cc_exttoint(Slgt.direction,Slgt.direction);
/*
.....Set the field traversals
*/
	S_set_traversal();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnActive(filedno, val, stat)
**       Method called when the Active field is changed.
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
static UD_FSTAT OnActive(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Set the field traversals
*/
	Slgt.active = *(val->frmint);
	S_set_traversal();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnType(filedno, val, stat)
**       Method called when the Type field is changed.
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
static UD_FSTAT OnType(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Set the field traversals
*/
	Slgt.type = (LW_light_type)*(val->frmint);
	S_set_traversal();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnColor(filedno, val, stat)
**       Method called when the Color field is changed.
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
/*
.....Set the field traversals
*/
	Slgt.color = *(val->frmint);
	S_set_traversal();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnPos(filedno, val, stat)
**       Method called when the Position button is pressed.
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
static UD_FSTAT OnPos(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int button,event,status;
	LtPoint pos,pt;
	LtVector norm;
	LtPickedEntityList entlist;
	LW_rv_picked_entity rvent;
/*
.....Take down the form
*/
	ud_form_invis();
/*
.....Let the user pick a location
*/
	do
	{
		event = ul_ipv_pick_entity("Select a location on the solid",
			LI_ENTITY_TYPE_FACE,&entlist,&rvent,pos,norm,&button);
		status = ul_ipv_pick_point(entlist,&rvent,pos,norm,pt);
		if (status != UU_SUCCESS) ud_wrerr("You did not pick a valid entity.");
	} while (status != UU_SUCCESS);
/*
.....Store the response
*/
	if (event == 3 && button == 1)
	{
		um_vctovc(pt,Slgt.position);
		ud_update_answer(FPOX,(int *)Slgt.position);
	}
/*
.....Display the form
*/
	ud_form_vis();
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnApply(filedno, val, stat)
**       Method called when the Apply button is pressed.  Appies the
**       current form Light settings.
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
static UD_FSTAT OnApply(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Use the new lights if they have changed
*/
	UM_cc_inttoext(Slgt.rgb,Slgt.rgb);
	UM_cc_inttoext(Slgt.direction,Slgt.direction);
	S_copy_lights(&Slgt,&Slights[Slight],1);
	ul_ipv_set_lights(Slights);
	UM_cc_exttoint(Slgt.rgb,Slgt.rgb);
	UM_cc_exttoint(Slgt.direction,Slgt.direction);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  S_set_traversal()
**       Sets the field traversals when a light setting has changed.
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
static void S_set_traversal()
{
	int i;
/*
.....Set the field traversals
*/
	if (!Slgt.active)
	{
		for (i=FTYP;i<=FDIR;i++) ud_set_traverse_mask(i,UU_FALSE);
	}
	else
	{
		ud_set_traverse_mask(FTYP,UU_TRUE);
		ud_set_traverse_mask(FCOL,UU_TRUE);
		if (Slgt.color == -1) ud_set_traverse_mask(FRGB,UU_TRUE);
		else ud_set_traverse_mask(FRGB,UU_FALSE);
		ud_set_traverse_mask(FINT,UU_TRUE);
		if (Slgt.type == LW_LIGHT_POINT || Slgt.type == LW_LIGHT_DISTANT)
		{
			ud_set_traverse_mask(FPOS,LW_active);
			ud_set_traverse_mask(FPOX,UU_TRUE);
		}
		else
		{
			ud_set_traverse_mask(FPOS,UU_FALSE);
			ud_set_traverse_mask(FPOX,UU_FALSE);
		}
		if (Slgt.type == LW_LIGHT_DISTANT)
			ud_set_traverse_mask(FDIR,UU_TRUE);
		else
			ud_set_traverse_mask(FDIR,UU_FALSE);
	}
/*
.....Update the form answers
*/
	ud_update_answer(FRGB,(int *)&Slgt.rgb);
	ud_update_answer(FINT,&Slgt.intensity);
	ud_update_answer(FPOX,(int *)&Slgt.position);
	ud_update_answer(FDIR,(int *)&Slgt.direction);
}

/*********************************************************************
**    I_FUNCTION     : S_copy_lights(inlgt,outlgt,nlgt)
**       Copies an array of light strucutures.
**    PARAMETERS   
**       INPUT  : 
**          inlgt   = Input array of light structures.
**          nlgt    = Number of light structures to copy.
**       OUTPUT :  
**          outlgt  = Copied array of light structures.
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static void S_copy_lights(inlgt,outlgt,nlgt)
LW_light_struc *inlgt,*outlgt;
int nlgt;
{
	int i;
	for (i=0;i<nlgt;i++) outlgt[i] = inlgt[i];
}

/*********************************************************************
**    I_FUNCTION     : S_compare_lights(lgt1,lgt2,nlgt)
**       Compares an array of light strucutures.
**    PARAMETERS   
**       INPUT  : 
**          lgt1    = First array of light structures.
**          lgt2    = Second array of light structures.
**          nlgt    = Number of light structures to compare.
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if the arrays are identical, UU_FALSE otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static UU_LOGICAL S_compare_lights(lgt1,lgt2,nlgt)
LW_light_struc *lgt1,*lgt2;
int nlgt;
{
	int i;
	UU_LOGICAL iret;
/*
.....Initialize routine
*/
	iret = UU_FALSE;
/*
.....Compare the structures
*/
	for (i=0;i<nlgt;i++)
	{
		if (lgt1[i].active != lgt2[i].active || lgt1[i].type != lgt2[i].type ||
			lgt1[i].color != lgt2[i].color ||
			!um_cceqcc(lgt1[i].rgb,lgt2[i].rgb) ||
			lgt1[i].intensity != lgt2[i].intensity ||
			!um_cceqcc(lgt1[i].position,lgt2[i].position) ||
			!um_cceqcc(lgt1[i].direction,lgt2[i].direction)) goto done;
	}
	iret = UU_TRUE;
/*
.....End of routine
*/
done:;
	return(iret);
}

/*********************************************************************
**    I_FUNCTION     : S_save_modfile
**       Save the NCLIPV display properties into modals file.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_modfile()
{
	int stat,i;
	char msg[80];
	UU_REAL rval[3];
	FILE *fptr;
	UX_pathname fname;
	static char yesno[2][10] = {"*NO","*YES"};
	static char ctype[4][10] = {"*AMBIENT","*DISTANT","*EYE","*POINT"};
	static char bcolor[65][96];
/*
.....Initialize routine
*/
	strcpy(bcolor[0], "*RGB");
	for (i=0; i<64;i++)
	{
		sprintf(bcolor[i+1], "*%s", uw_color_name[i]);
	}
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy (fname, "nclipv_lights.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) goto done;
/*
.....Store light modals
*/
	ux_fputs0("#LIGHTS#\n", fptr);
	for (i=0;i<LW_MAX_LIGHT;i++)
	{
		sprintf(msg,"\n/LIGHT/ %d\n",i+1);
		ux_fputs0(msg, fptr);

		sprintf(msg,"/ACTIVE/ %s\n",yesno[LW_lights[i].active]);
		ux_fputs0(msg, fptr);

		sprintf(msg,"/TYPE/ %s\n",ctype[LW_lights[i].type]);
		ux_fputs0(msg,fptr);

		sprintf(msg,"/COLOR/ %s\n",bcolor[LW_lights[i].color+1]);
		ux_fputs0(msg,fptr);

		sprintf(msg,"/RGB/ %g,%g,%g\n",LW_lights[i].rgb[0],
			LW_lights[i].rgb[1],LW_lights[i].rgb[2]);
		ux_fputs0(msg,fptr);

		sprintf(msg,"/INTENSITY/ %d\n",LW_lights[i].intensity);
		ux_fputs0(msg, fptr);

		UM_cc_inttoext(LW_lights[i].position,rval);
		sprintf(msg,"/POSITION/ %g,%g,%g\n",rval[0],rval[1],rval[2]);
		ux_fputs0(msg,fptr);

		sprintf(msg,"/DIRECTION/ %g,%g,%g\n",LW_lights[i].direction[0],
			LW_lights[i].direction[1],LW_lights[i].direction[2]);
		ux_fputs0(msg,fptr);
	}
/*
.....Close modals file
*/
	ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
	return(stat);
}
