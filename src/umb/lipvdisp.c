/*********************************************************************
**	FILENAME: ldisplay.c
**	CONTAINS:
**				ul_ipv_display_mod
**				ul_ipv_swap_screen
**          ul_ipv_display_obstruct
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvdisp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:13
*********************************************************************/

#include "usysdef.h"
#include "gtbl.h"
#include "lipv.h"
#include "lipvmach.h"
#include "lumb.h"
#include "mdcpln.h"
#include "ustdio.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "udfdata.h"
#include "udforms.h"

#define NLAB 5
#define FLCOL 0
#define FLTOP 1
#define FLULF 2
#define FLBOT 3
#define FLURT 4

#define FMOD 0
#define FAXS 1
#define FBUF 2
#define FWIN 3
#define FMIN 4
#define FHID 5
#define FTRA 6
#define FEDG 7
#define FSHD 8
#define FCOL1 9
#define FRGB1 10
#define FCOL2 11
#define FRGB2 12
#define FCOL3 13
#define FRGB3 14
#define FCOL4 15
#define FRGB4 16
#define FIMG 17
#define FIMT 18
#define FROT 19
#define FSTR 20

int NCL_swap_changed = 0;

extern int UM_swap_ipv;
extern int PKx,PKy;
extern UG_wdt glwdt;

void ul_ipv_swap_screen();
void ul_ipv_display_obstruct();

static UD_FSTAT OnWin();
static UD_FSTAT OnShader();
static UD_FSTAT OnToggle();
static UD_FSTAT OnColor();
static UD_FSTAT OnBrowse();
static int S_save_modfile();

static int Scolor[4],Sshadfl;
static UM_vector Srgb[4];
static UX_pathname Sfile;

extern char uw_color_name[64][96];
/*********************************************************************
**	 E_FUNCTION : ul_ipv_display_mod()
**			This function handles the NCLIPV Display Properties form.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS:    none.
**	 SIDE EFFECTS:  none.
**	 WARNINGS: none.
*********************************************************************/
void ul_ipv_display_mod()
{
	int i,j,status,axes,dwin,dmin,mode,inc,shad,brot,bstr,idum,ncol,buf;
	int shadsav,colsav[4],rotsav,strsav,hauto,htrans,hedge;
	UU_LOGICAL ifl,ifl1;
	UX_pathname filsav;
	UM_vector rgbsav[4];
	int *ans[22];
	static UD_METHOD methods[] = {UU_NULL,UU_NULL,UU_NULL,OnWin,UU_NULL,
		OnToggle,UU_NULL,UU_NULL, OnShader,
		OnColor,UU_NULL, OnColor,UU_NULL, OnColor,UU_NULL, OnColor,UU_NULL,
		OnBrowse,UU_NULL,UU_NULL,UU_NULL};
	static char called[] = {6,6,6,6,6, 6,6,6, 6, 6,6, 6,6, 6,6, 6,6, 6,6,6,6};
	static char traverse[] = {1,1,1,1,1, 1,1,1, 1, 1,1, 1,1, 1,1, 1,1, 1,1,1,1};
	static char display[] = {1,1,1,1,1,1, 1,1,1, 1,1,1,1, 1, 1,1, 1,1, 1,1, 1,1,
		1,1,1,1};
/*
.....Initialize fields
*/
	mode = LW_display_prop.mode;
	axes = LW_display_prop.axes;
	buf = LW_display_prop.buf;
	hauto = LW_display_prop.hide_auto;
	htrans = LW_display_prop.hide_lucency;
	hedge = LW_display_prop.hide_edge;
	dwin = LW_display_prop.swap;
	dmin = LW_display_prop.minimize;
	shad = LW_display_prop.shader;
	strcpy(Sfile,LW_display_prop.bgfile);
	brot = LW_display_prop.rotate;
	bstr = LW_display_prop.stretch;
/*
.....Setup traverse masks
*/
	traverse[FTRA] = traverse[FEDG] = hauto;
/*
.....Set up default answers &
.....Display fields
*/
	ans[0] = &mode;
	ans[1] = &axes;
	ans[2] = &buf;
	ans[3] = &dwin;
	ans[FMIN] = &dmin;
	ans[FHID] = &hauto;
	ans[FTRA] = &htrans;
	ans[FEDG] = &hedge;
	ans[FSHD] = &shad;
	inc = 0;
	for (i=0;i<4;i++)
	{
		ans[FCOL1+inc] = &Scolor[i];
		ans[FRGB1+inc] = (int *)Srgb[i];
		inc += 2;
	}
	for (i=0;i<NLAB;i++) display[i] = 0;
	for (i=FCOL1;i<=FSTR;i++) display[i+NLAB] = traverse[i] = 0;
	Sshadfl = shad;
	switch (shad)
	{
/*
........Solid background
*/
	case 0:
		Scolor[0] = LW_display_prop.bgcolor;
		UM_cc_exttoint(LW_display_prop.bgrgb,Srgb[0]);
		display[FCOL1+NLAB] = 1;
		display[FRGB1+NLAB] = 1;
		traverse[FCOL1] = 1;
		traverse[FRGB1] = 1;
		display[FLCOL] = 1;
		ncol = 1;
		break;
/*
........Graduated background
*/
	case 1:
		Scolor[0] = LW_display_prop.grad[0];
		UM_cc_exttoint(LW_display_prop.grgb[0],Srgb[0]);
		Scolor[1] = LW_display_prop.grad[1];
		UM_cc_exttoint(LW_display_prop.grgb[1],Srgb[1]);
		display[FCOL1+NLAB] = 1;
		display[FRGB1+NLAB] = 1;
		display[FCOL2+NLAB] = 1;
		display[FRGB2+NLAB] = 1;
		traverse[FCOL1] = 1;
		traverse[FRGB1] = 1;
		traverse[FCOL2] = 1;
		traverse[FRGB2] = 1;
		display[FLTOP] = 1;
		display[FLBOT] = 1;
		ncol = 2;
		break;
/*
........4-corner background
*/
	case 2:
		inc = 0;
		for (i=0;i<4;i++)
		{
			Scolor[i] = LW_display_prop.fcolor[i];
			UM_cc_exttoint(LW_display_prop.frgb[i],Srgb[i]);
			display[FCOL1+inc+NLAB] = 1;
			traverse[FCOL1+inc] = 1;
			display[FRGB1+inc+NLAB] = 1;
			traverse[FRGB1+inc] = 1;
			inc += 2;
		}
		display[FLULF] = 1;
		display[FLURT] = 1;
		ncol = 4;
		break;
/*
........Image file
*/
	case 3:
		for (i=0;i<4;i++)
		{
			display[FIMG+i+NLAB] = 1;
			traverse[FIMG+i] = 1;
		}
		ncol = 0;
		break;
	}
	ans[FIMG] = &idum;
	ans[FIMT] = (int *)&Sfile;
	ans[FROT] = &brot;
	ans[FSTR] = &bstr;
	inc = 0;
	for (i=0;i<ncol;i++)
	{
		if (Scolor[i] != -1)
		{
			traverse[FRGB1+inc] = 0;
		}
		inc += 2;
	}
/*
.....Save background settings
*/
	shadsav = shad;
	for (i=0;i<4;i++)
	{
		colsav[i] = Scolor[i];
		um_vctovc(Srgb[i],rgbsav[i]);
	}
	strcpy(filsav,Sfile);
	rotsav = brot;
	strsav = bstr;
/*
.....Set traverse flags
*/
	traverse[FMIN] = LW_display_prop.swap;
	if (LW_nclipv==LW_STANDALONE)
	{
		display[3+NLAB] = display[4+NLAB] = 0;
	}
/*
.....Get the Form input
*/
	status = ud_form1("ipvdisplay.frm",ans,ans,methods,called,display,traverse);
	if (status == -1) goto done;
/*
.....Determine if background has changed
*/
	if (shad != shadsav) ifl = UU_TRUE;
	else
	{
		ifl = UU_FALSE;
		ncol = 1;
		if (shad == 1) ncol = 2;
		else if (shad == 2) ncol = 4;
		else if (shad == 3)
		{
			if (strcmp(Sfile,filsav) != 0) ifl = UU_TRUE;
			if (brot != rotsav || bstr != strsav) ifl = UU_TRUE;
			ncol = 0;
		}
		for (i=0;i<ncol;i++)
		{
			if (Scolor[i] != colsav[i]) ifl = 1;
			else if (Scolor[i] == -1)
			{
				if (!um_cceqcc(Srgb[i],rgbsav[i])) ifl = 1;
			}
		}
	}
/*
.....Store modals
*/
	LW_display_prop.mode = mode;
	LW_display_prop.axes = axes;
	LW_display_prop.buf = buf;
	LW_display_prop.swap = dwin;
	LW_display_prop.minimize = dmin;
/*
.....Store Obstruction attributes
*/
	ifl1 = UU_FALSE;
	if (hauto != LW_display_prop.hide_auto ||
		htrans != LW_display_prop.hide_lucency||
		hedge != LW_display_prop.hide_edge) ifl1 = UU_TRUE;
	LW_display_prop.hide_auto = hauto;
	LW_display_prop.hide_lucency = htrans;
	LW_display_prop.hide_edge = hedge;

/*
.....Store Background Shader parameters
*/
	LW_display_prop.shader = shad;
	ncol = 1;
	if (shad == 1) ncol = 2;
	else if (shad == 2) ncol = 4;
	else if (shad == 3) ncol = 0;
	for (i=0;i<ncol;i++)
	{
		if (Scolor[i] == -1)
		{
			for (j=0;i<3;i++)
			{
				if (Srgb[i][j] < 0.) Srgb[i][j] = 0.;
				if (Srgb[i][j] > 1.) Srgb[i][j] = 1.;
			}
		}
	}
	switch (shad)
	{
/*
........Solid background
*/
	case 0:
		if (Scolor[0] != -1)
		{
			LW_display_prop.bgcolor = Scolor[0];
		}
		else
		{
			LW_display_prop.bgcolor = Scolor[0];
			UM_cc_inttoext(Srgb[0],LW_display_prop.bgrgb);
		}
		break;
/*
........Graduated background
*/
	case 1:
		for (i=0;i<2;i++)
		{
			if (Scolor[i] != -1)
			{
				LW_display_prop.grad[i] = Scolor[i];
			}
			else
			{
				LW_display_prop.grad[i] = Scolor[i];
				UM_cc_inttoext(Srgb[i],LW_display_prop.grgb[i]);
			}
		}
		break;
/*
........4-corner background
*/
	case 2:
		for (i=0;i<4;i++)
		{
			if (Scolor[i] != -1)
			{
				LW_display_prop.fcolor[i] = Scolor[i];
			}
			else
			{
				LW_display_prop.fcolor[i] = Scolor[i];
				UM_cc_inttoext(Srgb[i],LW_display_prop.frgb[i]);
			}
		}
		break;
/*
........Image file
*/
	case 3:
		strcpy(LW_display_prop.bgfile,Sfile);
		LW_display_prop.rotate = brot;
		LW_display_prop.stretch = bstr;
		break;
	}
/*
.....Set the obstruction settings
*/
	if (ifl1) ul_ipv_display_obstruct(UU_TRUE);
/*
.....Set the background shader
*/
	if (ifl) ul_ipv_set_background();
/*
.....Swap screen if setting changed
*/
	if (LW_display_prop.swap != UM_swap_ipv) ul_ipv_swap_screen();
/*
.....Save the Display modals file
*/
	S_save_modfile();
/*
.....Update the NCLIPV display
.....with the new properties
*/
	if (LW_active) ul_ipv_view_same(LW_vport.xform);
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    S_FUNCTION     :  OnWin(filedno, val, stat)
**       Method called when Swap Window toggle is changed.
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
static UD_FSTAT OnWin(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
   if (*(val->frmint) == 1)
      ud_set_traverse_mask(FMIN,UU_TRUE);
	else
      ud_set_traverse_mask(FMIN,UU_FALSE);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnShader(filedno, val, stat)
**       Method called when the Background Shader toggle is changed.
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
static UD_FSTAT OnShader(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,shad,inc,ncol,nfld;
/*
.....Value did not change
*/
	shad = *(val->frmint);
	if (shad == Sshadfl) goto done;
/*
.....Change previous shader fields
*/
	switch (Sshadfl)
	{
/*
........Solid background
*/
	case 0:
      ud_set_display_mask(UD_DISPLAYF,FLCOL,UU_FALSE);
		ncol = 1;
		break;
/*
........Graduated background
*/
	case 1:
      ud_set_display_mask(UD_DISPLAYF,FLTOP,UU_FALSE);
      ud_set_display_mask(UD_DISPLAYF,FLBOT,UU_FALSE);
		ncol = 2;
		break;
/*
........4-corner background
*/
	case 2:
      ud_set_display_mask(UD_DISPLAYF,FLULF,UU_FALSE);
      ud_set_display_mask(UD_DISPLAYF,FLURT,UU_FALSE);
		ncol = 4;
		break;
/*
........Image file
*/
	case 3:
      ud_set_display_mask(UD_INPUTF,FIMG,UU_FALSE);
      ud_set_display_mask(UD_INPUTF,FIMT,UU_FALSE);
      ud_set_display_mask(UD_INPUTF,FROT,UU_FALSE);
      ud_set_display_mask(UD_INPUTF,FSTR,UU_FALSE);
		ncol = 0;
		break;
	}
/*
.....Display current shader fields
*/
	switch (shad)
	{
/*
........Solid background
*/
	case 0:
      ud_set_display_mask(UD_DISPLAYF,FLCOL,UU_TRUE);
		Scolor[0] = LW_display_prop.bgcolor;
		UM_cc_exttoint(LW_display_prop.bgrgb,Srgb[0]);
		nfld = 1;
		break;
/*
........Graduated background
*/
	case 1:
      ud_set_display_mask(UD_DISPLAYF,FLTOP,UU_TRUE);
      ud_set_display_mask(UD_DISPLAYF,FLBOT,UU_TRUE);
		Scolor[0] = LW_display_prop.grad[0];
		Scolor[1] = LW_display_prop.grad[1];
		UM_cc_exttoint(LW_display_prop.grgb[0],Srgb[0]);
		UM_cc_exttoint(LW_display_prop.grgb[1],Srgb[1]);
		nfld = 2;
		break;
/*
........4-corner background
*/
	case 2:
      ud_set_display_mask(UD_DISPLAYF,FLULF,UU_TRUE);
      ud_set_display_mask(UD_DISPLAYF,FLURT,UU_TRUE);
		inc = 0;
		for (i=0;i<4;i++)
		{
			Scolor[i] = LW_display_prop.fcolor[i];
			UM_cc_exttoint(LW_display_prop.frgb[i],Srgb[i]);
			inc += 2;
		}
		nfld = 4;
		break;
/*
........Image file
*/
	case 3:
      ud_set_display_mask(UD_INPUTF,FIMG,UU_TRUE);
      ud_set_display_mask(UD_INPUTF,FIMT,UU_TRUE);
      ud_set_display_mask(UD_INPUTF,FROT,UU_TRUE);
      ud_set_display_mask(UD_INPUTF,FSTR,UU_TRUE);
      ud_set_traverse_mask(FIMG,UU_TRUE);
      ud_set_traverse_mask(FIMT,UU_TRUE);
      ud_set_traverse_mask(FROT,UU_TRUE);
      ud_set_traverse_mask(FSTR,UU_TRUE);
		nfld = 0;
		break;
	}
/*
.....Too many color fields active
.....disable some
*/
	if (ncol > nfld)
	{
		inc = nfld * 2;
		for (i=nfld;i<ncol;i++)
		{
     	 	ud_set_display_mask(UD_INPUTF,FCOL1+inc,UU_FALSE);
     	 	ud_set_display_mask(UD_INPUTF,FRGB1+inc,UU_FALSE);
     	 	ud_set_traverse_mask(FCOL1+inc,UU_FALSE);
			inc += 2;
		}
	}
/*
.....Not enough color fields active
.....enable them
*/
	if (nfld > ncol)
	{
		inc = ncol * 2;
		for (i=ncol;i<nfld;i++)
		{
     	 	ud_set_display_mask(UD_INPUTF,FCOL1+inc,UU_TRUE);
     	 	ud_set_display_mask(UD_INPUTF,FRGB1+inc,UU_TRUE);
     	 	ud_set_traverse_mask(FCOL1+inc,UU_TRUE);
			inc += 2;
		}
	}
/*
.....Set traversal masks for RGB fields
*/
	inc = 0;
	for (i=0;i<nfld;i++)
	{
		if (Scolor[i] == -1)
			ud_set_traverse_mask(FRGB1+inc,UU_TRUE);
		else
		{
			ud_set_traverse_mask(FRGB1+inc,UU_FALSE);
		}
		ud_update_answer(FCOL1+inc,&Scolor[i]);
		ud_update_answer(FRGB1+inc,(int *)Srgb[i]);
		inc += 2;
	}
/*
.....End of routine
*/
done:;
	Sshadfl = shad;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnToggle(filedno, val, stat)
**       Method called when a toggle field is changed.
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
static UD_FSTAT OnToggle(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (*fieldno == FHID)
	{
		ud_set_traverse_mask(FTRA,(*val->frmint));
		ud_set_traverse_mask(FEDG,(*val->frmint));
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnColor(filedno, val, stat)
**       Method called when a Color field toggle is changed.
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
   if (*(val->frmint) == -1)
      ud_set_traverse_mask(*fieldno+1,UU_TRUE);
	else
      ud_set_traverse_mask(*fieldno+1,UU_FALSE);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnBrowse(filedno, val, stat)
**       Routine to get the Background Image file name.
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
static UD_FSTAT OnBrowse(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char sbuf[80],ext[UX_SUFFIX_LEN],descrip[80];
	int inum;
/*
.....Get filename to load
*/
	sprintf(sbuf,"Select Background Image File");
	strcpy(ext,"*.jpg");
	strcpy(descrip,"JPEG Files (*.jpg)");
	inum = 0;
	Sfile[0] = '\0';
	ud_get_filename(sbuf,sbuf,ext,Sfile,&inum,descrip, 1,UU_FALSE);
	if (inum != 0)
	{
		ux_add_ftype("jpg",Sfile,UX_NPRTERRS);
		ud_update_answer(FIMT,(int *)Sfile);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : S_save_modfile
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
	int i,stat;
	char msg[80];
	FILE *fptr;
	UX_pathname fname;
	static char yesno[2][10] = {"*NO","*YES"};
	static char dismod[3][10] = {"*SHADED","*WIRE","*HIDDEN"};
	static char disbuf[2][10] = {"*SWAP","*PIXEL"};
	static char cshade[4][10] = {"*SOLID","*GRADUATE","*4-CORNER","*IMAGE"};
	static char bcolor[65][96];
	static char crot[4][10] = {"*0","*90","*180","*270"};
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
	strcpy(bcolor[0], "*RGB");
	for (i=0; i<64;i++)
	{
		sprintf(bcolor[i+1], "*%s", uw_color_name[i]);
	}
/*
.....Open modals file
*/
	strcpy (fname, "nclipv_display.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) goto done;
/*
.....Store display modals
*/
	ux_fputs0("#DISPLAY#\n", fptr);
	sprintf(msg,"/DISPLAY/ %s\n",dismod[LW_display_prop.mode]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/AXES/ %s\n",yesno[LW_display_prop.axes]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/BUFFER/ %s\n",disbuf[LW_display_prop.buf]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SWAP_SCREEN/ %s\n",yesno[LW_display_prop.swap]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/MINIMIZE/ %s\n",yesno[LW_display_prop.minimize]);
	ux_fputs0(msg,fptr);
/*
.....Store Auto-hide modals
*/
	sprintf(msg,"/AUTO_HIDE/ %s\n",yesno[LW_display_prop.hide_auto]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/TRANSLUCENCY/ %d\n",LW_display_prop.hide_lucency);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/EDGE_DISPLAY/ %s\n",yesno[LW_display_prop.hide_edge]);
	ux_fputs0(msg, fptr);
/*
.....Store Background modals
*/
	ux_fputs0("\n#BACKGROUND#\n",fptr);
	sprintf(msg,"/SHADER/ %s\n",cshade[LW_display_prop.shader]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/COLOR/ %s\n",bcolor[LW_display_prop.bgcolor+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/RGB/ %g,%g,%g\n",LW_display_prop.bgrgb[0],
		LW_display_prop.bgrgb[1],LW_display_prop.bgrgb[2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/TOP_COLOR/ %s\n",bcolor[LW_display_prop.grad[0]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/BOT_COLOR/ %s\n",bcolor[LW_display_prop.grad[1]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/TOP_RGB/ %g,%g,%g\n",LW_display_prop.grgb[0][0],
		LW_display_prop.grgb[0][1],LW_display_prop.grgb[0][2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/BOT_RGB/ %g,%g,%g\n",LW_display_prop.grgb[1][0],
		LW_display_prop.grgb[1][1],LW_display_prop.grgb[1][2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/UL_COLOR/ %s\n",bcolor[LW_display_prop.fcolor[0]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/UR_COLOR/ %s\n",bcolor[LW_display_prop.fcolor[1]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/LL_COLOR/ %s\n",bcolor[LW_display_prop.fcolor[2]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/LR_COLOR/ %s\n",bcolor[LW_display_prop.fcolor[3]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/UL_RGB/ %g,%g,%g\n",LW_display_prop.frgb[0][0],
		LW_display_prop.frgb[0][1],LW_display_prop.frgb[0][2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/UR_RGB/ %g,%g,%g\n",LW_display_prop.frgb[1][0],
		LW_display_prop.frgb[1][1],LW_display_prop.frgb[1][2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/LL_RGB/ %g,%g,%g\n",LW_display_prop.frgb[2][0],
		LW_display_prop.frgb[2][1],LW_display_prop.frgb[2][2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/LR_RGB/ %g,%g,%g\n",LW_display_prop.frgb[3][0],
		LW_display_prop.frgb[3][1],LW_display_prop.frgb[3][2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/IMAGE/ %s\n",LW_display_prop.bgfile);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/ROTATE/ %s\n",crot[LW_display_prop.rotate]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/STRETCH/ %s\n",yesno[LW_display_prop.stretch]);
	ux_fputs0(msg,fptr);
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

/*********************************************************************
**    E_FUNCTION     : ul_ipv_swap_screen()
**       swap NCL graphic window screen and IPV screen
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_swap_screen()
{
#ifdef UU_IPV
	int save_PKx,save_PKy,save_cx,save_cy,markval;
	static int ipv_start=0;
	if (ipv_start)
		return;

	if (LW_active==UU_FALSE)
		return;
	UD_MARK(markval,UU_FALSE);
	if (markval == 0)
	{
		if (UM_swap_ipv)
			UM_swap_ipv = 0;
		else
			UM_swap_ipv = 1; 
		save_cx = glwdt.dspsize.raster.x;
		save_cy = glwdt.dspsize.raster.y;

		save_PKx = PKx;
		save_PKy = PKy;
/*
.....redisplay graphic window and IPV Window
.....call unsize function to display
*/
		NCL_swap_changed = 1;
		ul_ipv_open_window();
		PKx = save_cx;
		PKy = save_cy;
		ipv_resize_window();
		NCL_swap_changed = 1;
#if UU_COMP==UU_WIN2K
/*
......befor draw any thing, reset text font for NCL window
*/
		uw_ntcreate_font (-1);
#endif
		uw_glresize_graphics(save_PKx, save_PKy, 1);
		if (UM_swap_ipv == 1)
		{
			uw_glinit_shading();
			uw_gllight_define();
		}
	}
	NCL_swap_changed = 0;
	ipv_start = 0;
	UD_UNMARK(markval);
#endif
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_display_obstruct(ifl)
**			This function sets the obstruction auto-hide properties.
**	 PARAMETERS	
**		 INPUT  :
**        ifl    = UU_TRUE = Settings have changed, update graphics
**                 accordingly.  UU_FALSE = Only update graphics if
**                 auto-hide is active.
**		 OUTPUT :  none.
**	 RETURNS:    none.
**	 SIDE EFFECTS:  none.
**	 WARNINGS: none.
*********************************************************************/
void ul_ipv_display_obstruct(ifl)
UU_LOGICAL ifl;
{
	int i,j;
	LW_stock_struc *sd;
/*	LtLineStyle lnstyle;*/
	LtSolidStyle style;
	LtData data;
/*
.....Don't do anything if the viewport has not been defined yet
*/
	if (LW_viewport == 0) goto done;
	if (!LW_display_prop.hide_auto && !ifl) goto done;
/*
.....Set the obstruction style
........Set the line style
*/
	if (LW_display_prop.hide_auto)
	{
/*
		lnstyle = LiLineStyleCreate();
		LiDataSetBoolean(&data,LW_display_prop.hide_edge);
		LiLineStyleSetProperty(lnstyle,LI_MW_LINE_VISIBLE,&data);
		LiDataSetEnum(&data,LI_MW_PATTERN_DOTTED);
		LiLineStyleSetProperty(lnstyle,LI_MW_LINE_PATTERN,&data);
*/
/*
........Set the viewport style
*/
		style = LiSolidStyleCreate();
		LiDataSetNat32(&data,100-LW_display_prop.hide_lucency);
		LiSolidStyleSetProperty(style,LI_SOL_STYLE_PROP_TRANSPARENCY,&data);
		LiDataSetBoolean(&data,LW_display_prop.hide_edge);
		LiSolidStyleSetProperty(style,LI_SOL_STYLE_PROP_RENDER_EDGES,&data);
/*
		LiDataSetGenericPtr(&data,lnstyle);
		LiSolidStyleSetProperty(style,LI_SOL_STYLE_PROP_EDGE_STYLE,&data);
*/
		LiDataSetGenericPtr(&data,style);
		LiMWViewportSetProperty(LW_viewport,LI_VPORT_PROP_MW_OBSTRUCT_STYLE,
			&data);
		LiSolidStyleDestroy(style);
/*		LiLineStyleDestroy(lnstyle);*/
	}
/*
.....Find the important stocks and fixtures
*/
	LiDataSetBoolean(&data,LW_display_prop.hide_auto);
	for (j=0;j<2;j++)
	{
		sd = LW_stock_first[j];
		for (i=0;i<LW_nstock[j];i++)
		{
			if (sd->important && sd->stock != 0)
				LiMWViewportSetSessPrimProperty(LW_viewport,sd->stock,
					LI_VPSP_PROP_MW_IMPORTANT,&data);
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
	}
/*
.....Mark tools as important
*/
	if (LW_tool[0] != (LtSessionPrim)0)
	{
		for (j=0;j<LW_spindle_num;j++)
		{
			LiMWViewportSetSessPrimProperty(LW_viewport,LW_tool[j],
				LI_VPSP_PROP_MW_IMPORTANT,&data);

			if (LW_shank[j] != (LtSessionPrim)0)
				LiMWViewportSetSessPrimProperty(LW_viewport,LW_shank[j],
					LI_VPSP_PROP_MW_IMPORTANT,&data);

			for (i=0;i<LW_num_holder[j];i++)
				LiMWViewportSetSessPrimProperty(LW_viewport,LW_holder[j][i],
					LI_VPSP_PROP_MW_IMPORTANT,&data);
		}
	}
	ul_ipv_flush();
/*
.....End of routine
*/
done:;
}
