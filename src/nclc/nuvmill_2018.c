/********************************************************************* 
**    NAME         :  nuvmill.c
**       CONTAINS:
**          nclu_vmill()
**
**    COPYRIGHT 2011 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nuvmill.c , 26.2
**     DATE AND TIME OF LAST  MODIFICATION
**       04/10/18 , 16:05:52
**       
*********************************************************************/
#include <string.h>
#include "nclmplay.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "ncluvmp.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mattr.h"

extern int NCL_preview_mot;
enum
{
/*
.....Boundaries
*/
	BOUNDRG1,BOUNDRG2,BOUNDRG3,BOUNDRG4,BOUNDRG5,BOUNDRG6,
	BOUNDRG7, BOUNDRG8, BOUNDRG9, BOUNDRG10, BOUNDRG11,
/*
.....Levels
*/
	LEVELRG1,LEVELRG2,LEVELRG3,LEVELRG4,LEVELRG5,LEVELRG6,
/*
.....Options
*/
	OPTRG1,OPTRG2,
/*
.....Colors
*/
	CLRRG1,CLRRG2,CLRRG3,CLRRG4,CLRRG5,
	CLRRG6, CLRRG7, CLRRG8, CLRRG9,
/*
.....ALL
*/
	FMOD, 
/*
.....Action item fields
*/
   FAPV, FAPY, FARS, FAPB, FAVE, FAGE, FAVW,FVIDEO
};
#define HIDE_KEY 0
#define CONTOUR_KEY 1

#define DIS 0
#define PLN 1

#define IN 0
#define OFFST 1

typedef struct
{
	UU_KEY_ID key;
	char label[NCL_MAX_LABEL];
	int start;
	int end;
	int total;
	int *colors;
	int *lintyp;
} Sopen_struc;

extern int NVMILL;

static UU_LOGICAL Sgeo_init[3]; /* for Sperim, Sholes, Sisles */
static UU_LOGICAL Spocfrm_init = UU_FALSE;
static UU_LOGICAL SOpenSides = UU_FALSE;
/*
......Boundaries
*/
static int Sdirmodp,Sislperim,Sbotperim;
static char Sds_thk[65],Sopen_thk[65];
static int Tdirmodp,Tislperim,Tbotperim;
static char Tds_thk[65],Topen_thk[65];
/*
.....Levels
*/
static int Stoptype, Ttoptype;
static char Spoc_bot[VSTRL], Sps_thk[65], Spoc_top[VSTRL];
static char Tpoc_bot[VSTRL], Tps_thk[65], Tpoc_top[VSTRL];
/*
.....Colors
*/
static int Scolorp,Scolori,Scolorop,Slintyp,Scolorb,Scolort,Scolorh;
static int Tcolorp,Tcolori,Tcolorop,Tlintyp,Tcolorb,Tcolort,Tcolorh;
static int Tgeotog,Tgeocol;
/*
.....value used
*/
static char Scmd_str[5][VSTRL];
static int Spsthk,Sdsthk;
static int Sopenthk;
static UU_LOGICAL Sgeo_redraw;
/*
.....Global variables
*/
extern int NAUTIPV;
extern UD_METHOD UD_initfrm_intry;

static int SfrmPlay=0;
static int Serrflg;
static UU_LOGICAL Spreviewflg,Schgmade;

static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;
/*
.....Section definitions
*/
enum {Boundaries, Levels, Options, Colors};
static char *Smode[]={"Boundaries", "Levels", "Options", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
static int *Ssecol[]={Sblack,Sblack,Sblack,Sblack,Sblack,Sblack};
static int Sacc[4];
/*
.....Main variables
*/
static UU_LOGICAL Sform_init = UU_FALSE;
static UN_motseg *Smptr=0,Smotatt;
static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];
/*
.....Geometry variables
*/
static NCLU_vmpmod Smodals;
static UM_sgeo Sgprm, Sgisl, Sghol; 
static UM_sgeo Sptop,Spbot;
static UU_LOGICAL Sgeo_apply = UU_FALSE;
static UU_LIST Sperim, Sholes;
static UU_LIST *Sisles = UU_NULL;
static int nperim = 0, nisles = 0,Snholes = 0;

static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];

static UU_LOGICAL allsf = UU_FALSE;
static UU_LOGICAL Sclick_pokmod;

static UD_FSTAT OnModals();
static UD_FSTAT OnOpen(),OnOpenDesel();
static int S_build_command();
static void S_init_form();
static void S_hilite_entity (), S_add_key(), S_section_changed();
static UD_FSTAT OnModals(), OnAction(), OnColors(),
	OnButtons(), OnCheck(), OnEditText(), OnChoices(),
	S_enter_form(),OnVideo();

static UD_FSTAT OnEditTxt();
static int Serrflg,Spreviewflg,Schgmade,Stog_ch[3],Smenu_ch;
static int Sopentyp;
UU_KEY_ID Sopen_key;
static int *Sanswers[] = {
/*
......Boundaries
*/
	UU_NULL,&Tdirmodp,UU_NULL,UU_NULL, UU_NULL,
	&Tislperim, &Tbotperim, UU_NULL, UU_NULL,
	(int *)Tds_thk, (int *)Topen_thk,
/*
.....Levels
*/
	UU_NULL, (int *)Tpoc_bot, (int *)Tps_thk, &Ttoptype, 
	(int *)Tpoc_top, UU_NULL,
/*
......Options
*/
	UU_NULL, UU_NULL,
/*
.....Colors
*/
	&Tcolorp, &Tcolori, &Tcolorop, &Tlintyp,
	&Tcolorb, &Tcolort, &Tcolorh,
	&Tgeotog,&Tgeocol,
/*
.....ALL
*/
	UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL
};

/*********************************************************************
**    I_FUNCTION     : S_enter_form(fieldno,val,stat)
**       Method called when the Form is first displayed.
**    PARAMETERS
**       INPUT  :
**          fieldno  -1 if routine is called from a form field.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT S_enter_form(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_dispfrm_set_attribs(0,CLRRG1,UM_BLACK,Tcolorp);
	ud_dispfrm_set_attribs(0,CLRRG2,UM_BLACK,Tcolori);
	ud_dispfrm_set_attribs(0,CLRRG3,UM_BLACK,Tcolorop);
	ud_dispfrm_set_attribs(0,CLRRG5,UM_BLACK,Tcolorb);
	ud_dispfrm_set_attribs(0,CLRRG6,UM_BLACK,Tcolort);
	ud_dispfrm_set_attribs(0,CLRRG7,UM_BLACK,Tcolorh);
	ud_dispfrm_set_attribs(0,CLRRG9,UM_BLACK,Tgeocol);
	S_enable_buttons();
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : S_enable_buttons()
**			Determines which fields should be enabled and which buttons
**       marked as necessary, but unfulfilled based on the Part, Drive,
**       and Check surface selections.
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_TRUE if Check Surface is satisfied.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_enable_buttons()
{
	int nc1, nc2;
	UU_LOGICAL sfpk1,sfpk2;
	UU_LOGICAL ifl;

	sfpk1 = UU_TRUE;
	nperim = UU_LIST_LENGTH(&Sperim);
	if (nperim>0)
	{
		ud_set_traverse_mask(BOUNDRG3,UU_TRUE);
		ud_dispfrm_set_attribs(0, BOUNDRG1, UM_BLACK, Tcolorp);
	}
	else
	{
		ud_set_traverse_mask(BOUNDRG3,UU_FALSE);
		ud_dispfrm_set_attribs(0, BOUNDRG1, UM_BLACK, UM_RED);
		sfpk1 = UU_FALSE;
	}
	if (SOpenSides)
		ud_set_traverse_mask(BOUNDRG5,UU_TRUE);
	else
		ud_set_traverse_mask(BOUNDRG5,UU_FALSE);
	ud_dispfrm_set_attribs(0, BOUNDRG4, UM_BLACK, Tcolorop);
	if (Sisles==NULL)
		nisles = 0;
	else
	{
		nisles = UU_LIST_LENGTH(Sisles);
	}
	if (nisles>0)
	{
		ud_set_traverse_mask(BOUNDRG9,UU_TRUE);
	}
	else
	{
		ud_set_traverse_mask(BOUNDRG9,UU_FALSE);
	}
	ud_dispfrm_set_attribs(0, BOUNDRG8, UM_BLACK, Tcolori);
	if (sfpk1)
	{
		if (Sacc[Boundaries]==0)
		{
			Ssecol[Boundaries] = Sblack;
			S_section_changed(Boundaries,UU_FALSE);
		}
		else
		{	
			Ssecol[Boundaries] = Sgreen; 
			S_section_changed(Boundaries,UU_TRUE);
		}
	}
	else
	{
		Ssecol[Boundaries] = Sred; 
		S_section_changed(Boundaries,UU_FALSE);
	}
	nc2 = (int)strlen(Tpoc_bot);
	ul_strip_blanks(Tpoc_bot, &nc2);
	ud_dispfrm_set_attribs(0, LEVELRG1, UM_BLACK, Tcolorb);
	ud_dispfrm_set_attribs(0, LEVELRG6, UM_BLACK, Tcolort);
	sfpk2 = UU_TRUE;
	if (Ttoptype == PLN)
	{
		nc1 = (int)strlen(Tpoc_top);
		ul_strip_blanks(Tpoc_top, &nc1);
		if (nc1>0)
		{
			sfpk2 = UU_TRUE;
			ud_dispfrm_set_attribs(0, LEVELRG6, UM_BLACK, Tcolort);
		}
		else
		{
			ud_dispfrm_set_attribs(0, LEVELRG6, UM_BLACK, UM_RED);
			sfpk2 = UU_FALSE;
		}
	}
	if (sfpk2)
	{
		if (Sacc[Levels]==0)
		{
			Ssecol[Levels] = Sblack;
			S_section_changed(Levels,UU_FALSE);
		}
		else
		{	
			Ssecol[Levels] = Sgreen; 
			S_section_changed(Levels,UU_TRUE);
		}
	}
	else
	{
		Ssecol[Levels] = Sred; 
		S_section_changed(Levels,UU_FALSE);
	}
	ud_dispfrm_set_attribs(0, OPTRG1, UM_BLACK, Tcolorh);

	Snholes  = UU_LIST_LENGTH(&Sholes);
	if (Snholes >0)
	{
		ud_set_traverse_mask(OPTRG2,UU_TRUE);
	}
	else
	{
		ud_set_traverse_mask(OPTRG2,UU_FALSE);
	}
/*
.....Set Action Buttons
*/
	ifl = sfpk1 && sfpk2;
	ud_frm_enable_ok(ifl);
	ud_set_traverse_mask(FAPV,ifl);
	ud_set_traverse_mask(FAPY,ifl);

	ifl = Sacc[Boundaries] + Sacc[Levels] + Sacc[Options] + Sacc[Colors]; 
	ud_set_traverse_mask(FARS,ifl);
	if ((nc1>0)||(nc2>0) ||(Snholes>0)||(nisles>0)||(nperim>0))
	{
		ud_set_traverse_mask(FAGE,1);
	}
	else
		ud_set_traverse_mask(FAGE,0);
	return UU_TRUE;
}
/*********************************************************************
**    S_FUNCTION     :  S_unhilite_entity(sfpt)
**       Unhighlights the selected geometry.
**    PARAMETERS
**       INPUT  :
**          sfpt     Pointer to selected geometry structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_unhilite_entity(sfpt)
UM_sgeo *sfpt;
{
	struct NCL_fixed_databag e;
	if (sfpt==NULL)
		return;
/*
.....Save geometry color
*/
	if (sfpt->color != -1)
	{
		if (sfpt->key != 0)
		{
			e.key = sfpt->key;
			if (ncl_retrieve_data_fixed(&e) != 0) return;
			if (sfpt->color == -1) ncl_get_geo_color (e.key,&sfpt->color);
/*
.....Highlight entity
*/
			if (Sgeo_redraw) ncl_remove_override_geo(e.key);
			ncl_update_geo_color(e.key,sfpt->color,UU_FALSE);
			uc_display(&e);
			ug_wflush();
		}
	}
/*
.....Reset geometry structure
*/
	sfpt->key = 0;
	sfpt->color = -1;
	sfpt->label[0] = '\0';
}

/*********************************************************************
**    S_FUNCTION     :  S_unhilite_entities(sflst)
**       Unhighlights the selected geometry entities.
**    PARAMETERS
**       INPUT  :
**          sflst    Pointer to list of selected geometry structures.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_unhilite_entities(sflst, geo_init)
UU_LIST *sflst;
UU_LOGICAL *geo_init;
{
	int i;
	UM_sgeo *geo;
/*
.....Loop through list
*/
	if (*geo_init == UU_TRUE)
	{
		geo = (UM_sgeo *)UU_LIST_ARRAY(sflst);
		for (i=0;i<UU_LIST_LENGTH(sflst);i++) S_unhilite_entity(&geo[i]);
		uu_list_free(sflst);
		*geo_init = UU_FALSE;
		sflst = UU_NULL;
	}
}

/*********************************************************************
**    I_FUNCTION     : S_form_vis()
**       Redisplays all Rmill forms.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_form_vis()
{
	ud_form_vis();
}
/*********************************************************************
**    I_FUNCTION     : S_form_invis()
**       Takes down all Rmill forms.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_form_invis()
{
	ud_form_invis();
}
/*********************************************************************
**    I_FUNCTION     :  S_init_geo(sfpt,label,color)
**       Initializes a picked geometry structure using a user entered
**       label.
**    PARAMETERS
**       INPUT  :
**          sfpt     Pointer to selected geometry structure.
**          label    Label of entity to store.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_init_geo (sfpt,label,color)
UM_sgeo *sfpt;
char *label;
int color;
{
	int nc;
	UM_f77_str f77_str;
/*
.....Same geometry as picked before
*/
	nc = (int)strlen(label);
	ul_strip_blanks(label,&nc);
	if (strcmp(label,sfpt->label) == 0) return;
/*
.....Unhilite the previous entity
*/
	S_unhilite_entity(sfpt);
/*
.....Store and hilite the new entity
*/
	if (nc != 0)
	{
		UM_init_f77_str(f77_str,label,nc);
		getkey(UM_addr_of_f77_str(f77_str),&sfpt->key);
		strcpy(sfpt->label,label);
		S_hilite_entity(sfpt,color);
	}
}


/*********************************************************************
**    S_FUNCTION     :  S_deselect_all()
**       Deselects all geometry in the Geomtry section.  The associated
**       text fields will be cleared and the geometry will be unhighlighted.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_deselect_all()
{
	int fno;
	UD_DDATA val;
	UD_FSTAT stat = UD_FLDOK;
	
	S_unhilite_entity(&Sptop);
	S_unhilite_entity(&Spbot);
	fno = BOUNDRG3;
	OnDeleteAll(&fno, &val, stat);
	fno = BOUNDRG9;
	OnDeleteAll(&fno, &val, stat);
	fno = OPTRG2;
	OnDeleteAll(&fno, &val, stat);
/*
.....Initialize geomtry variables
*/
	Spbot.key = 0; Spbot.relnum = 0; Spbot.color = -1; Spbot.label[0] = '\0';
	Sptop.key = 0; Sptop.relnum = 0; Sptop.color = -1; Sptop.label[0] = '\0';
	Tpoc_bot[0] = '\0';
	ud_update_answer(LEVELRG2, (int *)Tpoc_bot);
	if (Ttoptype == PLN)
	{
		Tpoc_top[0] = '\0';
		ud_update_answer(LEVELRG5,(int *)Tpoc_top);
	}
}
/*********************************************************************
**    S_FUNCTION     :  S_unhilite_all()
**       Unhighlights all geometry entities used in this form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_unhilite_all()
{
	S_unhilite_entity(&Spbot);
	S_unhilite_entity(&Sptop);

	S_unhilite_entities(&Sperim, &(Sgeo_init[0]));
	nperim = 0;

	S_unhilite_entities(&Sholes, &(Sgeo_init[1]));
	Snholes = 0;
	if (Sisles!=UU_NULL)
	{
		S_unhilite_entities(Sisles, &(Sgeo_init[2]));
	}
	nisles = 0;
}
/*********************************************************************
**    S_FUNCTION     :  S_hilite_entity(sfpt,color)
**       Highlights the selected geometry.
**    PARAMETERS
**       INPUT  :
**          sfpt     Pointer to selected geometry structure.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_hilite_entity (sfpt,color)
UM_sgeo *sfpt;
int color;
{
	struct NCL_fixed_databag e;
/*
.....Save geometry color
*/
	if (color != -1)
	{
		if (sfpt->key != 0)
		{
			e.key = sfpt->key;
			if (ncl_retrieve_data_fixed(&e) != 0) return;
			if (sfpt->color == -1) ncl_get_geo_color (e.key,&sfpt->color);
/*
.....Highlight entity
*/
			if (Sgeo_redraw) ncl_add_override_geo(e.key);
			ncl_update_geo_color(e.key,color,UU_TRUE);
			uc_display(&e);
			ug_wflush();
		}
	}
}
/*********************************************************************
**    I_FUNCTION     : S_section_changed(section, reset)
**			Updates all answers in the form.
**    PARAMETERS   
**       INPUT  :
**          section   = Label of section that changed.
**          reset     = UU_TRUE = Enable Reset button.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_section_changed(section,reset)
int section;
UU_LOGICAL reset;
{
	UU_LOGICAL bold;
/*
.....Change color of section label
*/
	bold = Ssecol[section] != Sblack;
	ud_form_section_color(0, Smode[section], Ssecol[section], bold);
/*
.....Enable Reset button
*/
	if (reset) ud_set_traverse_mask(FARS,UU_TRUE);
}
/*********************************************************************
**    I_FUNCTION     : S_create_key_list(which)
**			Creates the key lists of geometry used in this form.
**    PARAMETERS   
**       INPUT  :
**          which     = HIDE_KEY = Create Override geometry list.  Used
**                      for hiding geometry.
**                      CONTOUR_KEY = Create Contour Stock (local) list.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_create_key_list(which)
int which;
{
	int i;
	UM_sgeo *mgeo;
/*
.....Free local key list
*/
	if (which == CONTOUR_KEY && Skey_init)
	{
		UU_LIST_EMPTY(&Skey_list);
		Snkeys = 0;
	}
/*
.....Add all geometry to the list
*/
	S_add_key(which, Sptop.key);
	S_add_key(which, Spbot.key);

	nperim = UU_LIST_LENGTH(&Sperim);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sperim);
	for (i=0;i<nperim;i++) S_add_key(which, mgeo[i].key);

	Snholes = UU_LIST_LENGTH(&Sholes);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sholes);
	for (i=0;i<Snholes;i++) S_add_key(which,mgeo[i].key);

	if (Sisles !=UU_NULL)
	{
		nisles = UU_LIST_LENGTH(Sisles);
		mgeo = (UM_sgeo *)UU_LIST_ARRAY(Sisles);
		for (i=0;i<nisles;i++) S_add_key(which,mgeo[i].key);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_add_key(which,key)
**			Adds a key to either the Override_Geo key list or Contour Stock
**       key list.
**    PARAMETERS   
**       INPUT  :
**          which     = HIDE_KEY = Create Override geometry list.  Used
**                      for hiding geometry.
**                      CONTOUR_KEY = Create Contour Stock (local) list.
**          key       = Key to add to the list.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_add_key(which,key)
UU_KEY_ID key;
int which;
{
	int rel;
	if (key != 0)
	{
		if (which == HIDE_KEY) ncl_add_override_geo(key);
		else
		{
			if (!Skey_init)
			{
        		uu_list_init(&Skey_list,sizeof(UU_KEY_ID),50,50);
				Skey_init = UU_TRUE;
			}
			ur_retrieve_data_relnum(key,&rel);
			if (uc_super_class(rel) == UC_SURFACE_CLASS)
			{
				uu_list_push(&Skey_list,&key);
				Snkeys++;
			}
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : S_update_answers()
**			Updates all answers in the form.
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_update_answers()
{
	int i;
	for (i=0;i<sizeof(Sanswers)/sizeof(int *);i++)
		if (Sanswers[i] != UU_NULL) ud_update_answer(i,Sanswers[i]);
}

/*********************************************************************
**    S_FUNCTION     :  OnEditText(fieldno, val, stat)
**       callback to editbox on the form.
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
static UD_FSTAT OnEditText(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_REAL thk;
	switch(*fieldno)
	{
	case LEVELRG2:
		if (strcmp(Scmd_str[0],Tpoc_bot) != 0)
		{
			strcpy(Scmd_str[0],Tpoc_bot);
			if (Spreviewflg) Schgmade = 1;
			Sacc[Levels] = 1;
		}
		break;

	case LEVELRG3:
		if (strcmp(Scmd_str[3],Tps_thk) != 0)
		{
			strcpy(Scmd_str[3],Tps_thk);
			if (Spreviewflg) Schgmade = 1;
			Sacc[Levels] = 1;
		}
		break;
	case LEVELRG4:
		if (strcmp(Scmd_str[1],Tpoc_top) != 0)
		{
			strcpy(Scmd_str[1],Tpoc_top);
			if (Spreviewflg) Schgmade = 1;
			Sacc[Levels] = 1;
			Spsthk = UU_TRUE;
		}
		break;
	case BOUNDRG11:
		if (strcmp(Scmd_str[4],Topen_thk) != 0)
		{
			strcpy(Scmd_str[4],Topen_thk);
			if (Spreviewflg) Schgmade = 1;
			Sacc[Boundaries] = 1;
			ncl_get_scalar_value(Topen_thk,&thk);
			if (thk != 0.) Sopenthk = UU_TRUE;
			else Sopenthk = UU_FALSE;
		}
		break;
	case BOUNDRG10:
		if (strcmp(Scmd_str[2],Tds_thk) != 0)
		{
			strcpy(Scmd_str[2],Tps_thk);
			if (Spreviewflg) Schgmade = 1;
			Sacc[Boundaries] = 1;
			Sdsthk = UU_TRUE;
		}
		break;
	}
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnChoices(fieldno, val, stat)
**       Routine to choicebox on the form.
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
static UD_FSTAT OnChoices(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
	case LEVELRG4:
		if (Ttoptype == DIS)
		{
			ud_set_traverse_mask(LEVELRG6,UU_FALSE);
			ud_set_traverse_mask(CLRRG6,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(LEVELRG6,UU_TRUE);
			ud_set_traverse_mask(CLRRG6,UU_TRUE);
		}
		if (Smenu_ch != Ttoptype)
		{
			if (Spreviewflg) Schgmade = 1;
			Smenu_ch = Ttoptype;
		}
		break;
	case BOUNDRG2:
		if (Tdirmodp == OFFST)
			ud_set_traverse_mask(BOUNDRG11,UU_TRUE);
		else
			ud_set_traverse_mask(BOUNDRG11,UU_FALSE);
		break;
	default:
		break;
	}
	S_enable_buttons();
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     : S_finditem(sfpt, sflst, rm_flag)
**       find the sfpt item from sflst list
**    PARAMETERS
**       INPUT  :
**          sfpt:   item to be find
**          sflst      geometry list.
**          rm_flag:    if remove item if find. 1: yes, 0: No
**       OUTPUT :
**          none
**    RETURNS      : TRUE if find, otherwise False
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_finditem(sfpt, sflst, rm_flag)
UM_sgeo *sfpt;
UU_LIST *sflst;
int rm_flag;
{
	int i;
	UM_sgeo *mgeo;
	UU_LOGICAL match = UU_FALSE;

	int num_item = UU_LIST_LENGTH(sflst);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(sflst);
	for (i=0;i<num_item;i++) 
	{
		if (mgeo[i].key==sfpt->key)
/*
.....match
*/
		{
			if (rm_flag)
			{
/*
......remove this item from sflst
*/
				S_unhilite_entity(&mgeo[i]);
				uu_list_delete (sflst,i,1);
			}
			match = UU_TRUE;
		}
	}
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(sflst);
	return match;
}
/*********************************************************************
**    I_FUNCTION     :  S_select_geo(sfpt,sflst,mask,multi,prmno,color,frm,
**                                   fieldno,label)
**       Routine to select geometry for the form.  The associated
**       text field will be updated with the geometry label and
**       the geometry will be highlighted.
**    PARAMETERS
**       INPUT  :
**          mask     Picking mask.
**          multi    0 = Single selection, 1 = multiple selection,
**                   2 = Single selection (calling routine handles invisibling
**                   of form).
**					3: multiple selection, not erase the old selection, but add into it.
**					4: multiple selection, not erase the old selection, but add into it.
**						But when select the item is from the sflst, remove it 
**          prmno    Prompt number to use while in pick mode.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**          frm      Form number that the field resides on. 0 = Main form.
**          fieldno  Text field to update with label of selected entity.
**                   -1 = No field to updated.
**          label    Text buffer that belongs to 'fieldno'.
**       OUTPUT :
**          sfpt     Pointer to selected geometry structure for single entity.
**          sflst    Pointer to selected geometry structure list when
**                   multiple entities can be picked (multi).
**    RETURNS      : UU_SUCCESS if an entity is picked.  UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_select_geo(sfpt,sflst,mask,multi,prmno,color,frm,fieldno,label,
	geo_init)
UM_sgeo *sfpt;
UU_LIST *sflst;
unsigned int *mask;
UU_LOGICAL multi;
int prmno,color,fieldno,frm;
char *label;
UU_LOGICAL *geo_init;
{
	int numint,iret,nc;
	unsigned int *imask;
	UU_LOGICAL init;
	struct NCL_fixed_databag e;
	UM_PLOCREC pick;
	UU_LOGICAL cmdreject,doend;
	UM_int4 sfkey;
	UM_int2 primtyp;
/*
.....Take down form
*/
	iret = UU_SUCCESS;
	if (multi != 2) S_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_TRUE);
	if (cmdreject != 0) goto failed;
/*
.....Set the appropriate selection mask
*/
	imask = mask;
	if (imask == UD_ncl_pl) imask = UD_ncl_allsfpl;
/*
.....Unhighlight the previous selection
*/
	if ((multi != 2)&&(multi != 3)&&(multi != 4))
	{
		if (sfpt != UU_NULL) S_unhilite_entity(sfpt);
		if (sflst != UU_NULL) S_unhilite_entities(sflst, geo_init);
	}
	else if ((multi!=3)&&(multi != 4))
	{
		sfpt->key = 0;
		sfpt->color = -1;
		sfpt->label[0] = '\0';
	}
/*
.....Get the next geometry selection
*/
repeat:;
	do
	{
		doend = UU_TRUE;
		ud_lgeo(UU_TRUE,imask);
/*
.....Single selection
*/
		if ((multi != 1)&&(multi != 3)&&(multi != 4))
		{
			ua_dl_pldas(UD_DASPCKLOC,UA_NCL,prmno,&pick,1,&numint,1);
			if (numint == 0) goto failed;
			e.key = um_get_pickkey(&(pick.pent),1);
/*
........Screen location picked
*/
			if (e.key == 0)
				strcpy(label,pick.ploc.label);
/*
........Check for planar surface
*/
			else
			{
				if (ncl_retrieve_data_fixed(&e) != 0) goto done;
				if (mask == UD_ncl_pl && e.rel_num != NCL_PLN_REL)
				{
					sfkey = e.key;
					ncl_get_sf_primtyp (&sfkey,&primtyp);
					if (primtyp != NCLSF_PLANE)
					{
						ud_wrerr("Entity Picked is not Planar.");
						doend = UU_FALSE;
					}
				}
			}
		}
/*
.....Multiple selection
*/
		else
		{
			ud_ldas(UD_DASSELECT,UA_NCL,prmno,NULL,1,&numint,UD_NODEFAULT);
			if (numint == 0) goto failed;
			doend = UU_TRUE;
		}
	} while (!doend);
/*
.....Single selection
*/
	if ((multi != 1)&&(multi != 3)&&(multi != 4))
	{
/*
........Store the entity's data
*/
		sfpt->key = e.key;
		sfpt->relnum = e.rel_num;
		sfpt->inout = Tdirmodp;
		if (sfpt->key != 0) ncl_get_label(&e,sfpt->label);
		else strcpy(sfpt->label,label);
/*
........Highlight the selected entity
*/
		S_hilite_entity(sfpt,color);
/*
........Update the text field with the entity label
*/
		if (fieldno != -1)
		{
			strcpy(label,sfpt->label);
			if (frm == 0) ud_update_answer(fieldno,(int *)label);
			else ud_dispfrm_update_answer(frm,fieldno,(int *)label);
		}
	}
/*
.....Multiple selection
*/
	else
	{
/*
........Unhighlight the previous selections
*/
		if ((multi != 3)&&(multi != 4))
		{
			S_unhilite_entities(sflst, geo_init);
		}
/*
........Store the entities' data
*/
		if(sflst->data==UU_NULL)
			uu_list_init(sflst,sizeof(UM_sgeo),numint,10);
		*geo_init = UU_TRUE;
		init = UU_TRUE;
		while (ud_gnxt(init,UU_NULL,&e.key,1))
		{
			init = UU_FALSE;
			if (e.key != 0)
			{
				if (ncl_retrieve_data_fixed(&e) != 0) goto done;
				sfpt->key = e.key;
				sfpt->relnum = e.rel_num;
				sfpt->color = -1;
				ncl_get_label(&e,sfpt->label);
/*
........CADD Text is not allowed
*/
				nc = (int)strlen(sfpt->label);
				ul_strip_blanks(sfpt->label,&nc);
/*
........Highlight the selected entity
*/
				if (nc != 0)
				{
					sfpt->inout = Tdirmodp;
/*
......check if this item is in the sflst already, if No,
......highlist and add into the sflst. 
......If Yes, remove it from sflst and de-highlighted
*/
					if (multi != 4)
					{
						S_hilite_entity(sfpt,color);
/*
........Push the entity onto the stack
*/
						uu_list_push(sflst,sfpt);
					}
					else if (S_finditem(sfpt, sflst,1)==UU_FALSE)
					{
						S_hilite_entity(sfpt,color);
/*
........Push the entity onto the stack
*/
						uu_list_push(sflst,sfpt);
					}
				}
			}
		}
/*
........No entities selected
*/
		if (UU_LIST_LENGTH(sflst) == 0)
		{
			ud_wrerr("No CAM entities selected.");
			uu_list_free(sflst);
			goto repeat;
		}
/*
........Update the text field with the entity label
*/
		else if (fieldno != -1)
		{
			sfpt = (UM_sgeo *)UU_LIST_ARRAY(sflst);
			strcpy(label,sfpt->label);
			if (UU_LIST_LENGTH(sflst) > 1) strcat(label,",...");
			if (frm == 0) ud_update_answer(fieldno,(int *)label);
			else ud_dispfrm_update_answer(frm,fieldno,(int *)label);
		}
	}
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_unlimit();
	if (multi != 2) S_form_vis();
	UD_UNMARK(cmdreject);
	return(iret);
/*
.....User did not select anything
*/
failed:
	iret = UU_FAILURE;
	goto done;
}
/*********************************************************************
**    I_FUNCTION     : OnDeleteAll(fieldno, val, stat)
**			Displays all surfaces in the layer.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnDeleteAll(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (*fieldno==BOUNDRG3)
	{
/*
......delete All
*/
		nperim = UU_LIST_LENGTH(&Sperim);
		if (nperim > 0)
		{
			S_unhilite_entities(&Sperim, &(Sgeo_init[0]));
			nperim = 0;
			if (Spreviewflg) Schgmade = 1;
		}
		ud_set_traverse_mask(BOUNDRG8,UU_TRUE);
		ud_set_traverse_mask(BOUNDRG9,UU_TRUE);
		ud_set_traverse_mask(CLRRG2,UU_TRUE);
		ud_set_traverse_mask(BOUNDRG3,0);
	}
	else if (*fieldno==BOUNDRG9)
	{
		if (Sisles)
		{
			nisles = UU_LIST_LENGTH(Sisles);
			if (nisles > 0)
			{
				S_unhilite_entities(Sisles, &(Sgeo_init[2]));
				nisles = 0;
				if (Spreviewflg) Schgmade = 1;
			}
		}
	}
	if (*fieldno==OPTRG2)
	{
		Snholes = UU_LIST_LENGTH(&Sholes);
		if (Snholes > 0)
		{
			S_unhilite_entities(&Sholes, &(Sgeo_init[1]));
			Snholes = 0;
			if (Spreviewflg) Schgmade = 1;
		}
	}
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  OnButtons(fieldno, val, stat)
**       Routine to button click on the form.
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
static UD_FSTAT OnButtons(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i, pr, namfld, *mask, color;
	int j1 = 0;
	char *namp;
	int status;
	struct NCL_fixed_databag e;
/*
	int numint,init,color,csav,pr,i,inout;
	struct NCL_fixed_databag e;
	UU_LOGICAL cmdreject;
	UM_sgeo geo;
	UU_LIST *sfpt;
*/
	if (*fieldno == BOUNDRG1)
	{
		UU_LOGICAL nsf,ntrim,cvpt;
		UM_sgeo *geo1;

		mask = (int *)UD_pocket_perimeter;
		pr = 495;
		status = S_select_geo(&Sgprm,&Sperim,mask,4,pr,Tcolorp,0,-1,NULL, &(Sgeo_init[0]));
		allsf = UU_FALSE;
		nperim = Sperim.cur_cnt;
		if (nperim > 0)
			ud_set_traverse_mask(BOUNDRG3,1);
		if (nperim > 1)
		{
			ud_set_traverse_mask(BOUNDRG8,UU_FALSE);
			ud_set_traverse_mask(BOUNDRG9,UU_FALSE);
			ud_set_traverse_mask(CLRRG2,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(BOUNDRG8,UU_TRUE);
			ud_set_traverse_mask(BOUNDRG9,UU_TRUE);
			ud_set_traverse_mask(CLRRG2,UU_TRUE);
		}		
		ud_set_traverse_mask(BOUNDRG6,UU_FALSE);
		ud_set_traverse_mask(BOUNDRG7,UU_FALSE);
		if (nperim > 0)
		{	
			nsf = ntrim = UU_FALSE;
			allsf = UU_TRUE;
			cvpt = UU_FALSE;
			geo1 = (UM_sgeo *) UU_LIST_ARRAY(&Sperim);
			for (i = 0; i < nperim; i++)
			{
				if (geo1[i].relnum == UM_POINT_REL ||
					geo1[i].relnum == NCL_POINT_REL || geo1[i].relnum == UM_COMPCRV_REL)
				{
					ncl_get_geo_color(geo1[i].key, &color);
					if (color!=-1)
					{
						Sopentyp++; Sopen_key = geo1[i].key;
						cvpt = UU_TRUE;
						ud_set_traverse_mask(BOUNDRG4,UU_TRUE);
						ud_set_traverse_mask(BOUNDRG5,UU_TRUE);
						ud_set_traverse_mask(CLRRG3,UU_TRUE);
						ud_set_traverse_mask(CLRRG4,UU_TRUE);
					}
					else
					{
						Sopentyp--;
						if (Sopentyp == 0)
						{
							ud_set_traverse_mask(BOUNDRG4,UU_FALSE);
							ud_set_traverse_mask(BOUNDRG5,UU_FALSE);
							ud_set_traverse_mask(CLRRG3,UU_FALSE);
							ud_set_traverse_mask(CLRRG4,UU_FALSE);
						}
					}
				}
			}
			for (i = 0; i < nperim && (!ntrim || allsf); i++)
			{
				if (i > 0 && ntrim)
				{
					allsf = (geo1[i].relnum == NCL_TRIMSF_REL ||
						geo1[i].relnum == NCL_SURF_REL || 
						geo1[i].relnum == UM_RBSPLSRF_REL || 
						geo1[i].relnum ==  NCL_REVSURF_REL);
					continue;
				}
				ntrim = (geo1[i].relnum == NCL_TRIMSF_REL);
				nsf = (ntrim || geo1[i].relnum == NCL_SURF_REL || 
						geo1[i].relnum == UM_RBSPLSRF_REL || 
						geo1[i].relnum ==  NCL_REVSURF_REL);
				if (i == 0) allsf = nsf;
				if (ntrim)
				{
					struct NCL_trimsf_rec *eptr;
					e.key = geo1[i].key;
					if (ncl_retrieve_data_fixed(&e) != 0) goto done;
					eptr = (struct NCL_trimsf_rec *) &e;
					ntrim = (eptr->no_ibndykey > 0);
				}
			}
			if (ntrim)
				ud_set_traverse_mask(BOUNDRG6,UU_TRUE);
			if (nsf)
			{
				ud_set_traverse_mask(BOUNDRG7,UU_TRUE);
				if (Tdirmodp == OFFST)
				{
					ud_set_traverse_mask(BOUNDRG11,UU_TRUE);
					ud_set_traverse_mask(BOUNDRG7,UU_FALSE);
					i = 1; ud_update_answer(BOUNDRG7,(int *)&i);
				}
			}
		}
		if (Tbotperim == UU_TRUE && allsf)
		{
			ud_set_traverse_mask(LEVELRG1,UU_FALSE);
			ud_set_traverse_mask(LEVELRG2,UU_FALSE);
			ud_set_traverse_mask(CLRRG5,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(LEVELRG1,UU_TRUE);
			ud_set_traverse_mask(LEVELRG2,UU_TRUE);
			ud_set_traverse_mask(CLRRG5,UU_TRUE);
		}
	}
	if (*fieldno == OPTRG1)
	{
		mask = (int *)UD_ncl_ptpv;
		pr = 503;
		status = S_select_geo(&Sghol, &Sholes, mask,4, pr,Tcolorh,0,-1,NULL, &(Sgeo_init[1]));
		Snholes = UU_LIST_LENGTH(&Sholes);
		if (Snholes > 0)
			ud_set_traverse_mask(OPTRG2,1);
	}
	if (*fieldno == BOUNDRG8)
	{
		if (!Sisles)
		{
			Sisles = (UU_LIST *) uu_malloc (sizeof (UU_LIST));
			uu_list_init(Sisles,sizeof(UM_sgeo),10,10); 
		}
		mask = (int *)UD_pocket_perimeter;
		pr = 496;
		status = S_select_geo(&Sgisl, Sisles, mask,4, pr,Tcolori,0,-1,NULL, &(Sgeo_init[2]));
		nisles = UU_LIST_LENGTH(Sisles);
		if (nisles > 0)
			ud_set_traverse_mask(BOUNDRG9,1);
	}
	if (*fieldno == LEVELRG1)
	{
		pr = 493;
		mask = UD_ncl_allsfpl;
		namp = Tpoc_bot;
		namfld = LEVELRG2;
		status = S_select_geo(&Spbot,UU_NULL,mask,0,pr,Tcolorb,0,namfld,namp,NULL);
	}
	if (*fieldno==LEVELRG6)
	{
		pr = 494;
		mask = UD_ncl_allsfpl;
		namp = Tpoc_top;
		namfld = LEVELRG5;
		status = S_select_geo(&Sptop,UU_NULL,mask,0,pr,Tcolort,0,namfld,namp,NULL);
	}
	if ((*fieldno == BOUNDRG3)
		|| (*fieldno == BOUNDRG9)
		|| (*fieldno == OPTRG2))
	{
		OnDeleteAll(fieldno, val, stat);
		status = 0;
	}
done:;
	if (status!=-1)
	{
		if (Spreviewflg) Schgmade = 1;
		if ((*fieldno>=BOUNDRG1)&&(*fieldno<=BOUNDRG11))
			Sacc[Boundaries] = 1;
		if ((*fieldno>=LEVELRG1)&&(*fieldno<=LEVELRG6))
			Sacc[Levels] = 1;
		if ((*fieldno==OPTRG2)||(*fieldno==OPTRG2))
			Sacc[Options] = 1;
		S_enable_buttons();
	}
/*
.....Check for change since preview
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnCheck(fieldno, val, stat)
**       Routine to checkbox on the form.
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
static UD_FSTAT OnCheck(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
	case BOUNDRG6:
		if (Tislperim == UU_TRUE)
		{
			ud_set_traverse_mask(BOUNDRG8,UU_FALSE);
			ud_set_traverse_mask(BOUNDRG9,UU_FALSE);
			ud_set_traverse_mask(CLRRG2,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(BOUNDRG8,UU_TRUE);
			ud_set_traverse_mask(BOUNDRG9,UU_TRUE);
			ud_set_traverse_mask(CLRRG2,UU_TRUE);
		}
		if (Stog_ch[0] != Tislperim)
		{
			if (Spreviewflg) Schgmade = 1;
			Stog_ch[0] = Tislperim;
			Sacc[Boundaries] = 1;
		}
		break;
	case BOUNDRG7:
		if (Tbotperim == UU_TRUE && allsf)
		{
			ud_set_traverse_mask(LEVELRG1,UU_FALSE);
			ud_set_traverse_mask(LEVELRG2,UU_FALSE);
			ud_set_traverse_mask(CLRRG5,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(LEVELRG1,UU_TRUE);
			ud_set_traverse_mask(LEVELRG2,UU_TRUE);
			ud_set_traverse_mask(CLRRG5,UU_TRUE);
		}
		if (Stog_ch[1] != Tbotperim)
		{
			if (Spreviewflg) Schgmade = 1;
			Stog_ch[1] = Tbotperim;
			Sacc[Boundaries] = 1;
		}
		break;
	default:
		break;
	}
	S_enable_buttons();
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     :  S_hilite_entities(sflst,color)
**       Highlights the selected geometry entities.
**    PARAMETERS
**       INPUT  :
**          sflst    Pointer to list of selected geometry structures.
**          color    Color to highlight entities.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_hilite_entities(sflst,color)
UU_LIST *sflst;
{
	int i;
	UM_sgeo *geo;
/*
.....Loop through list
*/
	geo = (UM_sgeo *)UU_LIST_ARRAY(sflst);
	for (i=0;i<UU_LIST_LENGTH(sflst);i++) S_hilite_entity(&geo[i],color);
}


/*********************************************************************
**    I_FUNCTION     : OnColors(fieldno,val,stat)
**       Method called when a Geometry toggle field is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnColors(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;

	switch (*fieldno)
	{
	case CLRRG1:
		S_hilite_entities(&Sperim, Tcolorp);
		break;
	case CLRRG2:
		S_hilite_entities(Sisles, Tcolori);
		break;
	case CLRRG3:
		ncl_set_open_att(Tcolorop,Tlintyp);
		ncl_pocket_draw_open(UU_FALSE);
		break;
	case CLRRG4:
		ncl_set_open_att(Tcolorop,Tlintyp);
		ncl_pocket_draw_open(UU_FALSE);
		break;
	case CLRRG5:
		S_hilite_entity(&Spbot,Tcolorb);
		break;
	case CLRRG6:
		S_hilite_entity(&Sptop,Tcolort);
		break;
	case CLRRG7:
		S_hilite_entities(&Sholes, Tcolorh);
		break;
	case CLRRG8:
		Tgeotog = val->frmint[0];
	case CLRRG9:
		if (Sgeo_redraw)
		{
			Sgeo_redraw = UU_FALSE;
			fno = FAGE;
			OnAction(&fno,val,stat);
		}
		break;
	}
	Sacc[Colors] = 1;
	Ssecol[Colors] = Sgreen; 
	S_section_changed(Colors,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : OnVideo(fieldno,val,stat)
**       Method called when an Video button is pressed.
**		Video button will play default video file, but we can change the file
**		doing nothing now exepect play default file, but put here in case we need it later
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnVideo(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnAction(fieldno,val,stat)
**       Method called when the View button is pressed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnAction(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status,i,flag= 0;
	switch (*fieldno)
	{
/*
.....Preview command
*/
	case FAPV:
/*
........Reset motion settings
*/
		if (Spreviewflg) 
		{
			moinfr(&flag);
			if (Sclick_pokmod) vmprst();
		}
		Spreviewflg = UU_FALSE;
		Schgmade = UU_FALSE;
		Serrflg = 0;
/*
........Erase last previewed motion
*/
		if (Smptr != UU_NULL) 
			ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
		Smptr = UU_NULL;
/*
........Save motion settings
*/
		moinfs();
		ncl_mark_mdisplay(&Smptr,&Smotatt,Smvpbox,UU_TRUE);

/*
.....Output *VMPMOD command
*/
		if (Sclick_pokmod)
		{
			vmpsav();
			nclu_vmpmod_cmd (&Smodals,UU_FALSE);
		}
/*
........Output command
*/
		status = S_build_command(UU_FALSE);
		if (status != UU_SUCCESS) 
			moinfr(&flag);
		else
		{
			Spreviewflg = UU_TRUE;
			ud_set_traverse_mask(FAPB,1);
			ud_set_traverse_mask(FAVE,NAUTIPV);
		}
		break;
/*
.....APPLY
*/
	case FAPY:
		if (Spreviewflg)
		{
			if (!Schgmade && Serrflg == 0) flag = UU_TRUE;
			else flag = UU_FALSE;
			moinfr(&flag);
			if (Sclick_pokmod && !flag) 
				vmprst();
			Sclick_pokmod = UU_FALSE;
		}
/*
.....Output the command
*/
		if (Sclick_pokmod) 
			nclu_vmpmod_cmd (&Smodals,UU_TRUE);
		status = S_build_command(UU_TRUE);
	    Smptr = UU_NULL;
		Spreviewflg = UU_FALSE;
		if (status != UU_SUCCESS) 
		{
			break;
		}
/*
.....reset all geometries fields per Ken
*/
		S_deselect_all();
		Sgeo_apply = UU_TRUE;
		for (i=FAPV;i<FAVW;i++) 
			ud_set_traverse_mask(i,0);
		i = -1;
		S_enter_form(&i, val, stat);
		break;
/*
.....Reset form
.....Unselect all geometry
.....Erase Preview motion
*/
	case FARS:
		if (Sgeo_redraw)
			ncl_hide_geo(-1,-1,-1,-1,UU_TRUE,UU_NULL,0);
		S_unhilite_all();
		if (Smptr != UU_NULL) 
			ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
		Smptr = UU_NULL;
		if (Spreviewflg) 
		{
			moinfr(&flag);
			if (Sclick_pokmod) vmprst();
		}
		S_init_form();
		S_enter_form(fieldno,val,stat);
		for (i=FAPV;i<FAVW;i++) 
			ud_set_traverse_mask(i,0);
		S_update_answers();
		Schgmade = Spreviewflg = UU_FALSE;
		Sacc[Boundaries] = Sacc[Levels] = Sacc[Options] = Sacc[Colors] = 0;
		break;
/*
.....Playback motion
*/
	case FAPB:
		SfrmPlay = nclu_playback_preview();
		if (SfrmPlay>0)
		{
			S_form_invis();
			uw_ntdspfrm_vis(SfrmPlay);
		}
		break;
/*
.....Verify motion
*/
	case FAVE:
		S_create_key_list(CONTOUR_KEY);
		SfrmPlay = ul_ipv_playback_preview(&Skey_list,Snkeys);
		if (SfrmPlay>0)
		{
			S_form_invis();
			uw_ntdspfrm_vis(SfrmPlay);
		}
		break;
/*
.....Toggle geometry display
*/
	case FAGE:
		if (!Sgeo_redraw)
		{
			S_create_key_list(HIDE_KEY);
			ncl_hide_geo(Tgeotog,Tgeocol,UM_DASHSPC_LINE,20,UU_TRUE,UU_NULL,0);
			Sgeo_redraw = UU_TRUE;
		}
		else
		{
			ncl_hide_geo(-1,-1,-1,-1,UU_TRUE,UU_NULL,0);
			Sgeo_redraw = UU_FALSE;
		}
		break;
/*
.....Enter viewing mode
*/
	case FAVW:
		S_form_invis();
		uz_dyn_mouse();
		S_form_vis();
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_init_form()
**       Initializes the Regional Milling form variables.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_init_form()
{
	UU_REAL rnum;
	UM_int2 ifl;
	UM_real8 thk;
/*
.....Get VMPMOD settings
*/	
	nclu_vmpmod_parm(&Smodals);
/*,
..... Initialize Boundaries
*/
	Sopentyp = 0;
	Sopen_key = -1;
	SOpenSides = UU_FALSE;
	if (!Spocfrm_init)
	{
		Spocfrm_init = UU_TRUE;
		Scolorp = 8; Scolori = 9; Scolorb = 10; Scolort = 11; Scolorop = 1;
		Scolorh = 14;
		Slintyp = 7;
		Sislperim = Sbotperim = UU_FALSE;
		Sdirmodp = 0;
	}
	ncl_set_open_att(Scolorop,Slintyp);
	strcpy(Spoc_bot,Smodals.botpln);
	Stoptype = Smodals.ttype;
	strcpy(Spoc_top,Smodals.toppln);
	Spsthk = Sdsthk = UU_FALSE;
	ifl = 23; getsc(&ifl,&thk);
	rnum = thk;
	ncl_sprintf (Sps_thk,&rnum,1);
	ifl = 24; getsc(&ifl,&thk);
	rnum = thk;
	ncl_sprintf (Sds_thk,&rnum,1);
	strcpy(Sopen_thk,"0.0");
	uu_list_init(&Sperim,sizeof(UM_sgeo),10,10);
	Sisles = UU_NULL;
	Spbot.key = 0; Spbot.relnum = 0; Spbot.color = -1; Spbot.label[0] = '\0';
	Sptop.key = 0; Sptop.relnum = 0; Sptop.color = -1; Sptop.label[0] = '\0';
	nperim = nisles = 0;
/*
.....Initialize predrilled holes
*/
	uu_list_init(&Sholes,sizeof(UM_sgeo),10,10);
	Snholes = 0;
/*
.....Store values to check for changes
*/
	strcpy(Scmd_str[0],Spoc_bot);
	strcpy(Scmd_str[1],Spoc_top);
	strcpy(Scmd_str[2],Sds_thk);
	strcpy(Scmd_str[3],Sps_thk);
	strcpy(Scmd_str[4],Sopen_thk);

	Tdirmodp = Sdirmodp;
	Tislperim = Sislperim;
	Tbotperim = Sbotperim;
	strcpy(Tds_thk, Sds_thk);
	strcpy(Topen_thk, Sopen_thk);
	Ttoptype = Stoptype;
	strcpy(Tpoc_bot, Spoc_bot);
	strcpy(Tps_thk, Sps_thk);
	strcpy(Tpoc_top, Spoc_top);
	Tcolorp = Scolorp;
	Tcolori = Scolori;
	Tcolorop = Scolorop;
	Tlintyp = Slintyp;
	Tcolorb = Scolorb;
	Tcolort = Scolort;
	Tcolorh = Scolorh;
	Tgeotog = UN_unused_geo_flag;
	Tgeocol = UN_unused_geo_color;
}
static void S_save_form()
{
	Sdirmodp = Tdirmodp;
	Sislperim = Tislperim;
	Sbotperim = Tbotperim;
	strcpy(Sds_thk, Tds_thk);
	strcpy(Sopen_thk, Topen_thk);
	Stoptype = Ttoptype;
	strcpy(Spoc_bot, Tpoc_bot);
	strcpy(Sps_thk, Tps_thk);
	strcpy(Spoc_top, Tpoc_top);
	Scolorp = Tcolorp;
	Scolori = Tcolori;
	Scolorop = Tcolorop;
	Slintyp = Tlintyp;
	Scolorb = Tcolorb;
	Scolort = Tcolort;
	Scolorh = Tcolorh;
	UN_unused_geo_flag = Tgeotog;
	UN_unused_geo_color = Tgeocol;
}
/*********************************************************************
**    E_FUNCTION     : nclu_vmill()
**       Controlling routine for the VoluMill Pocket form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_vmill()
{
	UU_LOGICAL cmdreject;
	int status,flag;
	UD_METHOD save_entry;
/*
.....Set up form fields
*/
	static char traverse[] = {
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,
		1,1,
		1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1 };
	static char display[] = {
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,
		1,1,
		1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1};
	static char called[] = {
		6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,
		6,6,
		6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6};
	static UD_METHOD methods[] = {
/*
		OnPerim,OnEnable,OnColor,OnDesel,OnOpen,OnEnable,OnColor,OnOpenDesel,
			OnEnable,OnEnable,OnPerim,OnColor,OnDesel,OnThick,OnThick,
		OnEditTxt,OnGeoSel,OnColor,OnThick,OnEnable,
		OnEditTxt,OnGeoSel,OnColor,
		OnPerim,OnColor,OnDesel,OnModals,
		OnView,OnPreview,OnMoters,OnDispCut};
*/
/*
.....Boundaries
*/
		OnButtons, OnChoices, OnButtons, 
		OnOpen, OnOpenDesel, 
		OnCheck, OnCheck, OnButtons, OnButtons,
		OnEditText, OnEditText,
/*
.....Levels
*/
		OnButtons, OnEditText, OnEditText, OnChoices, OnEditText, OnButtons, 
/*
.....Options
*/
		OnButtons, OnButtons,
/*
.....Colors
*/
		OnColors, OnColors, OnColors, OnColors, OnColors, 
		OnColors, OnColors, OnColors, OnColors, 
/*
.....All
*/
		OnModals, OnAction, OnAction, OnAction,
		OnAction, OnAction, OnAction, OnAction,OnVideo
	};
/*
.....Initialize routine
*/
	if (NVMILL == 0)
	{
		ud_wrerr("You are not authorized to run this product.");
		return;
	}
	Sclick_pokmod = UU_FALSE;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0) goto fini;

	S_init_form();

	Serrflg = 0;
	Spreviewflg = UU_FALSE;
	Schgmade = UU_FALSE;
	save_entry = UD_initfrm_intry;
	SfrmPlay = 0;
	Skey_init = UU_FALSE;
	Snkeys = 0;
	Sacc[Boundaries] = Sacc[Levels] = Sacc[Options] = Sacc[Colors] = 0;

	traverse[BOUNDRG6] = UU_FALSE;
	traverse[BOUNDRG7] = UU_FALSE;
	traverse[LEVELRG6] = traverse[CLRRG6] = (Ttoptype == DIS)? UU_FALSE: UU_TRUE;

	traverse[BOUNDRG4] = traverse[BOUNDRG5] = traverse[CLRRG3] = UU_FALSE;
	traverse[CLRRG4] = UU_FALSE;

	if (Tislperim == UU_TRUE)
	{
		traverse[BOUNDRG8] = UU_FALSE;
		traverse[BOUNDRG9] = UU_FALSE;
		traverse[CLRRG2] = UU_FALSE;
	}
	else
	{
		traverse[BOUNDRG8] = UU_TRUE;
		traverse[BOUNDRG9] = UU_TRUE;
		traverse[CLRRG2] = UU_TRUE;
	}
	if (Tdirmodp == OFFST)
	{
		traverse[BOUNDRG11] = UU_TRUE;
/* original code assign this but why?
/*		traverse[LEVELRG1] = UU_FALSE;
		traverse[LEVELRG2] = UU_FALSE;
		traverse[CLRRG5] = UU_FALSE;
*/	}
	else
	{
/*		traverse[BOUNDRG11] = UU_FALSE;
//?		traverse[LEVELRG1] = UU_TRUE;
//?		traverse[LEVELRG2] = UU_TRUE;
//?		traverse[CLRRG5] = UU_TRUE;
*/	}
	if (Tbotperim == UU_TRUE && allsf)
	{
		traverse[LEVELRG1] = UU_FALSE;
		traverse[LEVELRG2] = UU_FALSE;
		traverse[CLRRG5] = UU_FALSE;
	}
	else
	{
		traverse[LEVELRG1] = UU_TRUE;
		traverse[LEVELRG2] = UU_TRUE;
		traverse[CLRRG5] = UU_TRUE;
	}
	NCL_preview_mot = 1;
/*
.....Get the Form input
*/
	UD_initfrm_intry = S_enter_form;
	status = ud_form1("nvmill.frm",Sanswers,Sanswers,methods,called,display,traverse);
	UD_initfrm_intry = save_entry;
/*
.....Erase last previewed motion
*/
	if (Smptr != UU_NULL && (!Spreviewflg || Schgmade || status == -1)) 
		ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
	else if (Spreviewflg)
		ncl_step_displayed(2,0,UU_NULL);
	Smptr = UU_NULL;
/*
.....Free the motion settings created during preview if a change has been
.....made and ok was pressed or if cancel was pressed
*/
	if (Spreviewflg)
	{
		if (!Schgmade && status != -1 && Serrflg == 0) flag = UU_TRUE;
		else flag = UU_FALSE;
		moinfr(&flag);
		if (Sclick_pokmod && !flag) 
			vmprst();
/*
.....add the nclu_vmpmod_cmd call here since we will set Sclick_pokmod = UU_FALSE here
*/
		if ((Sclick_pokmod)&&(status!=-1)) 
			nclu_vmpmod_cmd (&Smodals,UU_TRUE);
		Sclick_pokmod = UU_FALSE;
	}
	if (status == -1) goto done;
/*
.....Output the command
*/
	if (Sclick_pokmod) 
		nclu_vmpmod_cmd (&Smodals,UU_TRUE);
	
	S_save_form();
	S_build_command(UU_TRUE);

done:;
	ncl_pocket_draw_open(UU_TRUE);
	SOpenSides = UU_FALSE;
	if (Sgeo_redraw)
		ncl_hide_geo(-1,-1,-1,-1,UU_TRUE,UU_NULL,0);
	if (Skey_init)
		uu_list_free(&Skey_list);
	Skey_init = UU_FALSE;
	S_unhilite_all();
	if (Sisles!=UU_NULL)
		uu_free (Sisles);
fini:;
	ncl_pocket_draw_open(UU_TRUE);
	SOpenSides = UU_FALSE;
	Sopentyp = 0;
	NCL_preview_mot = 0;
	UD_UNMARK(cmdreject);
	return;
}

/*********************************************************************
**    S_FUNCTION     :  OnOpen(fieldno, val, stat)
**       Routine to select open regions of perimeter geometry.
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
static UD_FSTAT OnOpen(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int numint,i,start,end,irtn,status,npts,sub,rel;
	int save_ptype,limit_key = -1;
	int total,pick_mask[UD_NMENTWD],*mask;
	struct NCL_fixed_databag e,e2;
	struct UM_compcrv_rec *ccv;
	struct UM_crvdatabag crv;
	struct NCL_nclpt_rec *ptt,pt1,pt2;
	UM_transf tfmat;
	UM_vector fwd,bwd;
	UU_REAL u,thk;
	UU_KEY_ID key,nclkey;
	UU_LOGICAL found,cmdreject;
	struct UM_evcrvout evout;
	Gwpoint3 pt, ve;
	UM_PLOCREC pick;
	Sopen_struc open;
	char ncllabel[NCL_MAX_LABEL];
	UM_f77_str_ptr flab;
	UM_int4 ipg, iel;
	UM_int2 nwds, ietype;
/*
.....Take down form
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_TRUE);
	if (cmdreject != 0)
	{
		ud_setpick_type(save_ptype);
		if (limit_key != -1) ud_limit_entity(UU_FALSE, limit_key);
		limit_key = -1;
		goto done;
	}
/*
.....Set the appropriate selection mask
*/
	for (i=0;i<UD_NMENTWD; i++) pick_mask[i] = UD_ncl_pt[i] | UD_compcrv[i];
	mask = (int *)pick_mask;
	ud_lgeo(UU_TRUE,mask);
	limit_key = -1;
	if (Sopentyp == 1)
	{
		limit_key = Sopen_key;
		ud_limit_entity(UU_TRUE, limit_key);
		if (limit_key != -1) ud_limit_entity(UU_FALSE, limit_key);
	}
	ua_dl_pldas(UD_DASPCKLOC,UA_NCL,495,&pick,1,&numint,1);
	if (numint > 0)
	{
		e.key = um_get_pickkey(&pick.pent, 1);
		open.key = e.key;
		ncl_retrieve_data_fixed(&e);
		rel = e.rel_num;
		if (e.rel_num == UM_COMPCRV_REL)
		{
/*
.....Get component picked.
*/
			ccv = (struct UM_compcrv_rec *)&e;
			key = um_get_pickkey(&pick.pent, 2);
			found = UU_FALSE;
			for (i = 0; i < ccv->no_cid && !found; i++)
				found = (key == ccv->cid[i].crvid);
			if (found) start = end = i;
/*
.....Get order to use for components.
*/
			u = 0.5;
			uc_retrieve_transf(key,tfmat);
			crv.key = key; ncl_retrieve_data_fixed(&crv);
			uc_init_evcrvout (&crv,&evout);
			uc_evcrv(UM_FRSTDERIV,u,&crv,tfmat,&evout);
			um_unitvc(evout.dcdu,evout.dcdu);
			if (ccv->cid[i-1].reverse) um_negvc(evout.dcdu,fwd);
			else um_vctovc(evout.dcdu,fwd);
			um_negvc(fwd,bwd);
			pt.x = evout.cp[0]; pt.y = evout.cp[1]; pt.z = evout.cp[2];
			ve.x = fwd[0]; ve.y = fwd[1]; ve.z = fwd[2];
			ud_assist_vector(pt,ve);
			ve.x = bwd[0]; ve.y = bwd[1]; ve.z = bwd[2];
			ud_assist_vector(pt,ve);

			save_ptype = ud_getpick_type();
			ud_unlimit ();
			irtn = ud_pick_assist_seg("Pick the direction to chain components:");
			ud_setpick_type(save_ptype);
/*
.....Pick ending component.
*/
			if (irtn != 0)
			{
				limit_key = e.key;
				ud_limit_entity(UU_TRUE, limit_key);
				ua_dl_pldas(UD_DASPCKLOC,UA_NCL,714,&pick,1,&numint,1);
/*
.....Get component picked.
*/
				if (numint > 0)
				{					
					key = um_get_pickkey(&pick.pent, 2);
					found = UU_FALSE;
					for (i = 0; i < ccv->no_cid && !found; i++)
						found = (key == ccv->cid[i].crvid);
					if (found) end = i;
				}
			}
/*
.....Removed so user can pick single component without having to 
.....pick direction - ASF 1/21/14
				else
				goto done;
*/
		}
		else
		{
/*
.....Get number of points using the given label.
*/
			ptt = (struct NCL_nclpt_rec *)&e;
			strcpy(ncllabel,e.label);
			for (i=strlen(ncllabel);i<NCL_MAX_LABEL;i++) ncllabel[i] = ' ';
			UM_init_f77_str(flab,ncllabel,NCL_MAX_LABEL);
			sub = 0;
			status = vxchk(UM_addr_of_f77_str(flab),&sub,&nclkey,&ipg,&iel,
				&nwds,&ietype);
			npts = ipg;
			start = e.subscr;
			sub = e.subscr + 1;
			if (sub > npts) sub = 1;
			status = vxchk(UM_addr_of_f77_str(flab),&sub,&pt1.key,&ipg,&iel,
				&nwds,&ietype);
			sub = e.subscr - 1;
			if (sub < 1) sub = npts;
			status = vxchk(UM_addr_of_f77_str(flab),&sub,&pt2.key,&ipg,&iel,
				&nwds,&ietype);
			ncl_retrieve_data_fixed(&pt1); ncl_retrieve_data_fixed(&pt2);
/*
.....Get direction for index.
*/
			pt.x = ptt->pt[0]; pt.y = ptt->pt[1]; pt.z = ptt->pt[2];
			um_vcmnvc(pt1.pt,ptt->pt,fwd); um_unitvc(fwd,fwd);
			um_vcmnvc(pt2.pt,ptt->pt,bwd); um_unitvc(bwd,bwd);
			ve.x = fwd[0]; ve.y = fwd[1]; ve.z = fwd[2];
			ud_assist_vector(pt,ve);
			ve.x = bwd[0]; ve.y = bwd[1]; ve.z = bwd[2];
			ud_assist_vector(pt,ve);
			irtn = ud_pick_assist_seg("Pick the direction to chain points:");
			if (irtn == 0) goto done;
/*
.....Set the appropriate selection mask
*/
			for (i=0;i<UD_NMENTWD; i++)
				pick_mask[i] = UD_points[i] & UD_pocket_perimeter[i];
			mask = (int *)pick_mask;
			ud_lgeo(UU_TRUE,mask);
			ua_dl_pldas(UD_DASPCKLOC,UA_NCL,216,&pick,1,&numint,1);
			e2.key = um_get_pickkey(&pick.pent, 1);
			ncl_retrieve_data_fixed(&e2);
			end = e2.subscr;
			if (strcmp(e.label,e2.label) != 0)
			{
				ud_wrerr("Invalid Point. Not a member of the point array.");
				goto done;
			}
			strcpy(open.label,e.label);
		}
/*
.....Fix order if necessary.
*/
		if (irtn != 1)
		{
			i = start;
			start = end;
			end = i;
		}
		open.start = start;
		open.end = end;
/*
.....Store the open region.
*/
		open.total = npts;
		if (rel != UM_COMPCRV_REL)
		{
			if (end < start) total = npts - start + end + 1;
			else total = end - start + 1;
			open.colors = (int *)uu_malloc(total*sizeof(int));
			for (i=0;i<total;i++)
			{
				sub = start + i;
				if (sub > npts) sub -= npts;
				status = vxchk(UM_addr_of_f77_str(flab),&sub,&nclkey,&ipg,&iel,
					&nwds,&ietype);
				ncl_get_geo_color(nclkey,&open.colors[i]);
			}
		}
		ncl_push_indlst(&open);
		if (Spreviewflg) Schgmade = 1;
		ud_set_traverse_mask(BOUNDRG11, UU_TRUE);
		ncl_get_scalar_value(Topen_thk, &thk);
		if (thk != 0) Sopenthk = UU_TRUE;
		else Sopenthk = UU_FALSE;
		ncl_pocket_draw_open(UU_FALSE);
		SOpenSides = UU_TRUE;
		Sacc[Boundaries] = 1;
	}
done:
	if (limit_key != -1) ud_limit_entity(UU_FALSE, limit_key);
	ud_form_vis();
	ud_unlimit();
	S_enable_buttons();

	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnOpenDesel(fieldno, val, stat)
**			Deselects all regions from the list.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Not used, form initiated through pushbutton.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnOpenDesel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{	
	ncl_pocket_draw_open(UU_TRUE);
	SOpenSides = UU_FALSE;
	if (Spreviewflg) Schgmade = 1;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnModals()
**			Displays the VoluMill Modals form.
**    PARAMETERS   
**       INPUT  : fieldno = Not used.
**			         val     = Not used, form initiated through pushbutton.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnModals(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	Sclick_pokmod = UU_TRUE;
	nclu_vmpmod(UU_FALSE);
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : S_build_command(flag)
**			Builds and outputs the VMPOCK/ command.
**    PARAMETERS   
**       INPUT  : flag    = UU_TRUE = output command to source file.
**			                UU_FALSE = Preview command only.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			Saves and restores motion parameters when previewing the command.
**			The motion will remain displayed though.
**    WARNINGS     : none
*********************************************************************/
static int S_build_command(flag)
UU_LOGICAL flag;
{
	int i,j,dirmodp;
	NCL_cmdbuf cmdbuf;
	UM_sgeo *geop,*geoi,*geoh;
	char endbuf[96],tstr[8];
	UU_LOGICAL cmdreject,nsf,ntrim,found;
	UU_LIST *openlst;
	Sopen_struc *open;

	if (nperim == 0) return (0);
	
	if (flag)
		NCL_preview_mot = 0;
	else
		NCL_preview_mot = 1;

	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto fini;

	geop = (UM_sgeo *) UU_LIST_ARRAY (&Sperim);
	if (nisles > 0) geoi = (UM_sgeo *) UU_LIST_ARRAY (Sisles);
	geoh = (UM_sgeo *)UU_LIST_ARRAY(&Sholes);
	for (i = 0; i < nperim; i++)
	{
		ncl_set_cmdmode(UU_TRUE);
/*
.....Initialize command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);

		if (!flag)
			ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		ntrim = (Tislperim && geop[i].relnum == NCL_TRIMSF_REL);
		nsf = (Tbotperim && (geop[i].relnum == NCL_TRIMSF_REL || 
						geop[i].relnum == NCL_SURF_REL || 
						geop[i].relnum == UM_RBSPLSRF_REL));
		if (ntrim)
		{
			struct NCL_trimsf_rec eptr;
			eptr.key = geop[i].key;
			ntrim = (ncl_retrieve_data_fixed (&eptr) == UU_SUCCESS && 
				eptr.no_ibndykey > 0);
		}

		if (i < nperim-1)
			dirmodp = geop[i].inout;
		else
			dirmodp = Tdirmodp;

		ncl_add_token(&cmdbuf, NCL_vmpock, NCL_nocomma);
		if (Tbotperim && nsf)
			ncl_add_token(&cmdbuf,geop[i].label,NCL_comma);
		else
			ncl_add_token(&cmdbuf,Spoc_bot,NCL_comma);

		ncl_add_token(&cmdbuf,Tpoc_top,NCL_comma);

		if (dirmodp == OFFST)
			ncl_add_token(&cmdbuf,NCL_offset,NCL_comma);

		if (geop[i].relnum == NCL_POINT_REL || geop[i].relnum == UM_POINT_REL)
		{
			nclu_unsubscript (geop[i].label,tstr);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
		}
		else
			ncl_add_token(&cmdbuf,geop[i].label,NCL_nocomma);

		for (j = 0; i == 0 && !ntrim && j < nisles; j++)
		{
			ncl_add_token(&cmdbuf, ",", NCL_nocomma);

			if (geoi[j].relnum == NCL_POINT_REL || geoi[j].relnum == UM_POINT_REL)
			{
				nclu_unsubscript (geoi[j].label,tstr);
				ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			}
			else
				ncl_add_token(&cmdbuf,geoi[j].label,NCL_nocomma);
		}
		if (ntrim)
		{
			ncl_add_token(&cmdbuf, ",", NCL_nocomma);
			ncl_add_token(&cmdbuf,geop[i].label,NCL_nocomma);
		}
		endbuf[0] = '\0';

		if (Spsthk)
		{
			sprintf (endbuf,",PS,THICK,%s",Tps_thk);
			ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
		}
		if (Sdsthk || Sopenthk)
		{
			if (Sopenthk)
				sprintf(endbuf,",DS,THICK,%s,%s",Tds_thk,Topen_thk);
			else
				sprintf (endbuf,",DS,THICK,%s",Tds_thk);
			ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
		}
/*
.....Added open boundary regions.
*/
		if (Sopentyp > 0)
		{
			ncl_get_indlst(&openlst);
			open = (Sopen_struc *) UU_LIST_ARRAY(openlst);
			found = UU_FALSE;
			for (j=0;j<openlst->cur_cnt;j++)
			{
				if (geop[i].key == open[j].key)
				{
					if (!found)
					{
						found = UU_TRUE;
						sprintf (endbuf,",OPEN");
						ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
					}
					if (open[j].start != open[j].end)
						sprintf (endbuf,",%d,THRU,%d",open[j].start,open[j].end);
					else
						sprintf (endbuf,",%d",open[j].start);
					ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
				}
			}
		}
/*
.....Add POSITN holes
*/
		if (Snholes > 0)
		{
			ncl_add_token(&cmdbuf,",POSITN",NCL_nocomma);
			for (j=0;j<Snholes;j++)
			{
				ncl_add_token(&cmdbuf, ",", NCL_nocomma);
				ncl_add_token(&cmdbuf,geoh[j].label,NCL_nocomma);
			}
		}
		
		ncl_add_cmdbuf(&cmdbuf);
/*
.....Insert the command at the current line, but do not execute
.......Only need to write the command if Preview was already pressed
.......and no changes were made since the command was already executed
*/
		if (Spreviewflg && !Schgmade) ncl_call_input(&cmdbuf,UU_TRUE);
/*
.....Execute command if a change has been made since the last preview or no
.....preview was created
*/
		else Serrflg = ncl_call(&cmdbuf);
	}
	
fini:
	NCL_preview_mot = 1;
	UD_UNMARK(cmdreject);
	return(UU_SUCCESS);
}
