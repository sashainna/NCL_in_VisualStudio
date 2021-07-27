/*********************************************************************
**	FILENAME: m2ulight.c
**	CONTAINS:
**				um_lights
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m2ulight.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:50
*********************************************************************/

#include "usysdef.h"
#include "lumb.h"
#include "mattr.h"
#include "mconst.h"
#include "mdcpln.h"
#include "mdrel.h"
#include "mdclass.h"
#include "mdcoord.h"
#include "mdattr.h"
#include "mrenddl.h"
#include "ustdio.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "udfdata.h"
#include "udforms.h"

#define FLGT 0
#define FACT 1
#define FTYP 2
#define FSPC 3
#define FCOL 4
#define FINT 5
#define FPOS 6
#define FPOM 7
#define FPON 8
#define FANG 9
#define FDIR 10
#define FAMB 11
#define FAPY 12

#define MAXLIGHTS 5

extern int UM_light_keys[];

typedef struct
{
	int active;
	int type;
	int space;
	int color;
	int intensity;
	UM_coord mpos;
	UM_vector npos;
	UM_vector direction;
	UU_REAL cone_angle;
	UU_REAL ambient;
} S_light_struc;
	
static UD_FSTAT OnLight();
static UD_FSTAT OnActive();
static UD_FSTAT OnType();
static UD_FSTAT OnSpace();
static UD_FSTAT OnPos();
static UD_FSTAT OnApply();

static void S_copy_lights();
static void S_load_lights();
static void S_save_lights();
static void S_set_traversal();
static int S_save_modfile();

static int Slight,Sactlight=0;
static UU_LOGICAL Slight_reset;
static S_light_struc Slgt,Slights[MAXLIGHTS];
extern char uw_color_name[64][96];
/*********************************************************************
**	 E_FUNCTION : um_lights()
**			This function handles the NCL Lights form.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS:    none.
**	 SIDE EFFECTS:  none.
**	 WARNINGS: none.
*********************************************************************/
void um_lights()
{
	int i,status,idum;
	static int *ans[FAPY+1];
	static UD_METHOD methods[] = {OnLight,OnActive,OnType,OnSpace,UU_NULL,
		UU_NULL,OnPos,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,OnApply};
	static char called[] = {6,6,6, 6,6,6, 6,6,6,6, 6,6, 6};
	char traverse[FAPY+1];
	static char display[] = {1,1,1, 1,1,1, 1,1,1,1, 1,1, 1};
/*
.....Save current light settings
*/
	Slight = Sactlight;
	S_load_lights(Slights);
	S_copy_lights(&Slights[Slight],&Slgt,1);
/*
.....Initialize fields
*/
	UM_cc_exttoint(Slgt.npos, Slgt.npos);
	UM_cc_exttoint(Slgt.direction,Slgt.direction);
/*
.....Setup the default answers
*/
	ans[FLGT] = &Slight;
	ans[FACT] = &Slgt.active;
	ans[FTYP] = &Slgt.type;
	ans[FSPC] = &Slgt.space;
	ans[FCOL] = &Slgt.color;
	ans[FINT] = &Slgt.intensity;
	ans[FPOS] = &idum;
	ans[FPOM] = (int *)&Slgt.mpos;
	ans[FPON] = (int *)&Slgt.npos;
	ans[FANG] = (int *)&Slgt.cone_angle;
	ans[FDIR] = (int *)&Slgt.direction;
	ans[FAMB] = (int *)&Slgt.ambient;
	ans[FAPY] = &idum;
/*
.....Set traverse flags
*/
	for (i=0;i<FAPY+1;i++) traverse[i] = 1;
	traverse[FPOS] = 1;
	if (!Slgt.active)
	{
		for (i=FTYP;i<=FAMB;i++) traverse[i] = 0;
	}
	else if (Slgt.type == UM_EYE_LIGHT-1)
	{
		Slgt.space = UM_SCREEN_POS;
		traverse[FSPC] = 0;
		traverse[FPOS] = 0;
		traverse[FPOM] = 0;
		traverse[FPON] = 0;
		traverse[FANG] = 0;
	}
	else if (Slgt.type == UM_POINT_LIGHT-1)
	{
		traverse[FANG] = 0;
		traverse[FDIR] = 0;
	}
/*
.....Set postion display flags
*/
	if (Slgt.space == UM_WORLD_POS)
	{
		display[FPOM] = 1;
		display[FPON] = 0;
	}
	else
	{
		display[FPOM] = 0;
		display[FPON] = 1;
	}
/*
.....Get the Form input
*/
	Slight_reset = UU_FALSE;
	status = ud_form1("mlight.frm",ans,ans,methods,called,display,traverse);
	if (status == -1) goto done;
	Slight_reset = UU_TRUE;
/*
.....Store the new lights
*/
	UM_cc_inttoext(Slgt.npos,Slgt.npos);
	UM_cc_inttoext(Slgt.direction,Slgt.direction);
	S_copy_lights(&Slgt,&Slights[Slight],1);
	S_save_lights(Slights);
/*
.....Save the Display modals file
*/
	S_save_modfile();
/*
.....End of routine
*/
done:;
	if (Slight_reset)
	{
		uw_gllight_define();
		uz_repaint(0);
	}
	return;
}

/*********************************************************************
**    S_FUNCTION     :  OnLight(fieldno, val, stat)
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
	UM_cc_inttoext(Slgt.npos,Slgt.npos);
	UM_cc_inttoext(Slgt.direction,Slgt.direction);
	S_copy_lights(&Slgt,&Slights[Sactlight],1);
	Slight = Sactlight = *(val->frmint);
	S_copy_lights(&Slights[Slight],&Slgt,1);
	UM_cc_exttoint(Slgt.npos,Slgt.npos);
	UM_cc_exttoint(Slgt.direction,Slgt.direction);
/*
.....Update the form answers
*/
	ud_update_answer(FACT,&Slgt.active);
	ud_update_answer(FTYP,&Slgt.type);
	ud_update_answer(FSPC,&Slgt.space);
	ud_update_answer(FCOL,&Slgt.color);
	ud_update_answer(FINT,&Slgt.intensity);
	ud_update_answer(FPOM,(int *)Slgt.mpos);
	ud_update_answer(FPON,(int *)Slgt.npos);
	ud_update_answer(FANG,(int *)&Slgt.cone_angle);
	ud_update_answer(FDIR,(int *)Slgt.direction);
	ud_update_answer(FAMB,(int *)&Slgt.ambient);
/*
.....Set the field traversals
*/
	S_set_traversal();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnActive(fieldno, val, stat)
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
**    S_FUNCTION     :  OnType(fieldno, val, stat)
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
	Slgt.type = *(val->frmint);
	S_set_traversal();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSpace(fieldno, val, stat)
**       Method called when the Space field is changed.
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
static UD_FSTAT OnSpace(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Set the field traversals
*/
	Slgt.space = *(val->frmint);
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
	int markval,numint;
	UD_NDCLOCREC pos;
	S_light_struc lights[MAXLIGHTS];
	struct UM_light_rec e;
/*
.....Take down the form
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK(markval,UU_TRUE);
	if (markval != 0) goto done;
/*
.....Modelling space
.....Just get a position
*/
	if (Slgt.space == UM_WORLD_POS)
	{
		ud_ldas(UD_DASCART,UM_MODEL,66,&pos,1,&numint,UD_NODEFAULT);
		um_vctovc(pos.cord,Slgt.mpos);
		ud_update_answer(FPOM,(int *)&Slgt.mpos);
	}
/*
.....Screen space
.....Drag light
*/
	else
	{
		S_load_lights(lights);
		OnApply(fieldno,val,stat);
		uw_gldynlight(Sactlight);
		e.key = UM_light_keys[Sactlight];
		ur_retrieve_data(&e,sizeof(struct UM_light_rec));
		Slgt.npos[0] = e.position[0];
		Slgt.npos[1] = e.position[1];
		Slgt.npos[2] = e.position[2];
		ud_update_answer(FPON,(int *)Slgt.npos);
		Slight_reset = UU_TRUE;
		S_save_lights(lights);
	}
/*
.....Display the form
*/
done:;
	ud_form_vis();
	UD_UNMARK(markval);
/*
.....End of routine
*/
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
.....Store the new lights
*/
	UM_cc_inttoext(Slgt.npos,Slgt.npos);
	UM_cc_inttoext(Slgt.direction,Slgt.direction);
	S_copy_lights(&Slgt,&Slights[Slight],1);
	S_save_lights(Slights);
	UM_cc_exttoint(Slgt.npos,Slgt.npos);
	UM_cc_exttoint(Slgt.direction,Slgt.direction);
/*
.....Display the new lights
*/
	uw_gllight_define();
	uz_repaint(0);
/*
.....End of routine
*/
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
.....Light is not active
*/
	if (!Slgt.active)
	{
		for (i=FTYP;i<=FAMB;i++) ud_set_traverse_mask(i,UU_FALSE);
/*
........Modelling space
*/
		if (Slgt.space == UM_WORLD_POS)
		{
			ud_set_display_mask(UD_INPUTF,FPON,UU_FALSE);
			ud_set_display_mask(UD_INPUTF,FPOM,UU_TRUE);
		}
/*
........Screen space
*/
		else
		{
			ud_set_display_mask(UD_INPUTF,FPOM,UU_FALSE);
			ud_set_display_mask(UD_INPUTF,FPON,UU_TRUE);
		}
	}
/*
.....Light is active
*/
	else
	{
		ud_set_traverse_mask(FTYP,UU_TRUE);
		ud_set_traverse_mask(FCOL,UU_TRUE);
		ud_set_traverse_mask(FINT,UU_TRUE);
		ud_set_traverse_mask(FAMB,UU_TRUE);
/*
........Eye light
*/

		if (Slgt.type == UM_EYE_LIGHT-1)
		{
			ud_set_traverse_mask(FSPC,UU_FALSE);
			ud_set_traverse_mask(FPOS,UU_FALSE);
			ud_set_traverse_mask(FPOM,UU_FALSE);
			ud_set_traverse_mask(FPON,UU_FALSE);
			ud_set_traverse_mask(FANG,UU_FALSE);
			ud_set_traverse_mask(FDIR,UU_TRUE);
		}
/*
........Point light
*/
		else
		{
			if (Slgt.type == UM_POINT_LIGHT-1)
			{
				ud_set_traverse_mask(FSPC,UU_TRUE);
				ud_set_traverse_mask(FPOS,UU_TRUE);
				ud_set_traverse_mask(FANG,UU_FALSE);
				ud_set_traverse_mask(FDIR,UU_FALSE);
			}
/*
........Spot light
*/
			else
			{
				ud_set_traverse_mask(FSPC,UU_TRUE);
				ud_set_traverse_mask(FPOS,UU_TRUE);
				ud_set_traverse_mask(FANG,UU_TRUE);
				ud_set_traverse_mask(FDIR,UU_TRUE);
			}
/*
........Modelling space
*/
			if (Slgt.space == UM_WORLD_POS)
			{
				ud_set_display_mask(UD_INPUTF,FPON,UU_FALSE);
				ud_set_display_mask(UD_INPUTF,FPOM,UU_TRUE);
				ud_set_traverse_mask(FPOM,UU_TRUE);
			}
/*
........Screen space
*/
			else
			{
				ud_set_display_mask(UD_INPUTF,FPOM,UU_FALSE);
				ud_set_display_mask(UD_INPUTF,FPON,UU_TRUE);
				ud_set_traverse_mask(FPON,UU_TRUE);
			}
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : S_copy_lights(inlgt,outlgt,nlgt)
**       Copies an array of light structures.
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
S_light_struc *inlgt,*outlgt;
int nlgt;
{
	int i;
	for (i=0;i<nlgt;i++) outlgt[i] = inlgt[i];
}

/*********************************************************************
**    I_FUNCTION     : S_load_lights(lights)
**       Copies an array of light strucutures.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  
**          outlgt  = Light definitions from Unibase.
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static void S_load_lights(lights)
S_light_struc *lights;
{
	int i,blanked;
	struct UM_light_rec e;
	struct UM_attrdata_rec attr;
/*
.....Loop through lights
*/
	for (i=0; i<MAXLIGHTS; i++)
	{
/*
........Get the Light record
*/
		e.key = UM_light_keys[i];
		e.rel_num = UM_LIGHT_REL;
		uc_retrieve_data(&e, sizeof(struct UM_light_rec));
		um_get_disp_attr(e.key, &attr);
		ur_retrieve_blanked(e.key, &blanked);
/*
........Store the light data
*/
		lights[i].type = e.type - 1;
		lights[i].active = !blanked;
		lights[i].color = attr.color;
		lights[i].intensity = e.intens;
		lights[i].space = e.space;
		if (e.space == UM_WORLD_POS)
		{
			um_vctovc(e.position,lights[i].mpos);
			um_nullvc(lights[i].npos);
		}
		else 
		{
			um_vctovc(e.position,lights[i].npos);
			um_nullvc(lights[i].mpos);
		}
		um_vctovc(e.direction,lights[i].direction);
		lights[i].cone_angle = e.cone_angle;
		lights[i].ambient = e.ambient[0];
	}
}

/*********************************************************************
**    I_FUNCTION     : S_save_lights(lights)
**       Copies an array of light strucutures.
**    PARAMETERS   
**       INPUT  :
**          lights  = Light definitions to store in Unibase.
**       OUTPUT : none
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static void S_save_lights(lights)
S_light_struc *lights;
{
	int i,blanked;
	struct UM_light_rec e;
	struct UM_attrdata_rec attr;
/*
.....Loop through lights
*/
	for (i=0; i<MAXLIGHTS; i++)
	{
/*
........Get the Light record
*/
		e.key = UM_light_keys[i];
		e.rel_num = UM_LIGHT_REL;
		uc_retrieve_data(&e, sizeof(struct UM_light_rec));
		um_get_disp_attr(e.key, &attr);
		ur_retrieve_blanked(e.key, &blanked);
/*
........Store the light data
*/
		e.type = lights[i].type + 1;
		e.intens = lights[i].intensity;
		e.space = lights[i].space;
		if (e.space == UM_WORLD_POS)
		{
			um_vctovc(lights[i].mpos,e.position);
		}
		else 
		{
			um_vctovc(lights[i].npos,e.position);
		}
		um_vctovc(lights[i].direction,e.direction);
		e.cone_angle = lights[i].cone_angle;
		e.ambient[0] = e.ambient[1] = e.ambient[2] = lights[i].ambient;
		e.ambient[3] = 1.;
		ur_update_data_fixed(&e);
/*
........Store the light attributes
*/
		attr.color = lights[i].color;
		ur_update_attr(&attr);
		blanked = !lights[i].active;
		ur_update_blanked(e.key,blanked);
	}
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
	int i, stat;
	char  msg[256];
	UM_coord pos;
	UX_pathname fname;
	FILE *fptr;
	char color[64][96];
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy(fname,"ncl_lights.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,fname,
		3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;
	for (i=0; i<64;i++)
	{
		sprintf(color[i], "*%s", uw_color_name[i]);
	}
/*
.....Store modals file
*/
	ux_fputs0("#LIGHTS#\n", fptr);

	for (i=0; i<MAXLIGHTS; i++)
	{
		sprintf(msg, "/LIGHT/ %d\n", i+1);
		ux_fputs0(msg, fptr);
		if (Slights[i].active)
			strcpy(msg, "/ACTIVE/ *Yes\n");
		else
			strcpy(msg, "/ACTIVE/ *No\n");
		ux_fputs0(msg, fptr);

		if (Slights[i].type == UM_POINT_LIGHT-1)
			strcpy(msg, "/TYPE/ *Point\n");
		else if (Slights[i].type == UM_SPOT_LIGHT-1)
			strcpy(msg, "/TYPE/ *Spot\n");
		else
			strcpy(msg, "/TYPE/ *Direct\n");
		ux_fputs0(msg, fptr);
	
		if (Slights[i].space == UM_SCREEN_POS)
			strcpy(msg, "/SPACE/ *Screen\n");
		else
			strcpy(msg, "/SPACE/ *Model\n");
		ux_fputs0(msg, fptr);

		if (Slights[i].color==-1)
		{
			strcpy(msg, "/COLOR/ *DEFAULT\n");
			ud_winerror("Should not go to here, suppose no default option.");
		}
		else
			sprintf(msg, "/COLOR/ %s\n", color[Slights[i].color]);
		ux_fputs0(msg, fptr);

		sprintf(msg, "/INTENSITY/ %d\n", Slights[i].intensity);
		ux_fputs0(msg, fptr);

		if (Slights[i].space == UM_WORLD_POS)
		{
			UM_cc_inttoext(Slights[i].mpos,pos);
			sprintf(msg, "/POSITION/ %3.4f, %3.4f, %3.4f\n", pos[0],pos[1],pos[2]);
		}
		else
		{
			sprintf(msg, "/POSITION/ %3.4f, %3.4f, %3.4f\n", Slights[i].npos[0],
				Slights[i].npos[1],Slights[i].npos[2]);
		}
		ux_fputs0(msg, fptr);

		sprintf(msg, "/CONE/ %3.1f\n", Slights[i].cone_angle);
		ux_fputs0(msg, fptr);

		sprintf(msg, "/DIRECTION/ %3.4f, %3.4f, %3.4f\n", Slights[i].direction[0],
						Slights[i].direction[1],Slights[i].direction[2]);
		ux_fputs0(msg, fptr);

		sprintf(msg, "/AMBIENT/ %3.4f\n\n", Slights[i].ambient);
		ux_fputs0(msg, fptr);
	}
	ux_fclose0 (fptr);
done:
	return 0;
}
