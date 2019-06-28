/*********************************************************************
**    NAME         :  vuview3.c
**       CONTAINS: User interface routines for modifying view data
**						for a viewport in a screen
**
**            uvu_change_view_form
**            uvu_background_form
**            uv_set_background
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       vuview3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:00
**************************************************************************/
#include "umath.h"
#include "udebug.h"
#include "usysdef.h"
#include "dasnog.h"
#include "gtbl.h"
#include "lumb.h"
#include "view.h"
#include "view1.h"
#include "mdcpln.h"
#include "mpocket.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "udfdata.h"
#include "udforms.h"

#define NO_DEFAULT 0
#define DEFAULT 1

#define NLAB 5
#define FLCOL 0
#define FLTOP 1
#define FLULF 2
#define FLBOT 3
#define FLURT 4

#define FSHD 0
#define FCOL1 1
#define FRGB1 2
#define FCOL2 3
#define FRGB2 4
#define FCOL3 5
#define FRGB3 6
#define FCOL4 7
#define FRGB4 8
#define FIMG 9
#define FIMT 10
#define FROT 11
#define FSTR 12
#define FPRE 13

extern UU_REAL UM_model_size;

void uv_set_background();

static UD_FSTAT OnShader();
static UD_FSTAT OnColor();
static UD_FSTAT OnBrowse();
static UD_FSTAT OnPreview();
static int S_save_modfile();
static void S_store_settings();

static int Scolor[4],Sshadfl,Sshader,Sbstr,Sbrot;
static UU_LOGICAL Spreview;
static UM_vector Srgb[4];
static UX_pathname Sfile;
static UV_backgnd_struc Sbackg;

extern char uw_color_name[64][96];
/*********************************************************************
**    E_FUNCTION     : uvu_change_view_form(view)
**       Change the view parameters
**    PARAMETERS   
**       INPUT  : 
**				view					view
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uvu_change_view_form(view)
	UV_view *view;
	
	{
	char buf[80];
	NCL_cmdbuf cmdbuf;
	static UU_LOGICAL cmdflag;
	static UM_vector norm;				/* normal vector */
	static UM_coord ref_pt,pt;				/* reference point */
	static UM_vector up_vec;			/* up vector */
	static UU_REAL magnification;		/* magnification factor */
	static int *ans[] = { (int *)norm, 
								 (int *)ref_pt, 
								 (int *)up_vec, 
								 (int *)&magnification,
								 (int *)&cmdflag };
	UU_REAL wxlen,mscale;
	int status;

	uu_denter(UU_MTRC,(us,"uvu_change_view_form(key=%x)", view->key));

	status = UU_SUCCESS;

	um_vctovc(view->cur_pln_norm, norm);
	UM_cc_exttoint(norm, norm);

	um_vctovc(view->cur_ref_pt, ref_pt);

	um_vctovc(view->cur_up_vect, up_vec);
	UM_cc_exttoint(up_vec, up_vec);

	magnification = 1.;
	cmdflag = UU_FALSE;

	/* Display and traverse the form */
	status = ud_form("chgview.frm", ans, ans);
	if (status==-1)
		return -1;
	um_unitvc(norm, norm);
	um_unitvc(up_vec, up_vec);
	if (!um_vcparall(norm, up_vec))
	{
		um_cross(up_vec, norm, up_vec);
		um_cross(norm, up_vec, up_vec);
		um_unitvc(up_vec, up_vec);
		if (cmdflag)
		{
			ncl_init_cmdbuf(&cmdbuf);
			sprintf(buf,"DRAFT/VIEW=%s,PARAMS",view->name);
			ncl_add_token(&cmdbuf,buf,NCL_comma);
			ncl_add_token(&cmdbuf,"CENTER=",NCL_nocomma);
			UM_cc_inttoext(ref_pt, pt);
			ncl_sprintf(buf,pt,3);
			ncl_add_token(&cmdbuf,buf,NCL_comma);
			ncl_add_token(&cmdbuf,"NORMAL=",NCL_nocomma);
			ncl_sprintf(buf,norm,3);
			ncl_add_token(&cmdbuf,buf,NCL_comma);
			ncl_add_token(&cmdbuf,"YAXIS=",NCL_nocomma);
			ncl_sprintf(buf,up_vec,3);
			ncl_add_token(&cmdbuf,buf,NCL_comma);
			mscale = UM_model_size / view->cur_aperture * magnification;
			ncl_add_token(&cmdbuf,"SCALE=",NCL_nocomma);
			ncl_sprintf(buf,&mscale,1);
			ncl_add_token(&cmdbuf,buf,NCL_comma);
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
/*
.....Update view
*/
		uv_update_pln_norm(view, norm);
		uv_update_up_vect(view, up_vec);
		uv_update_ref_pt(view, ref_pt);

			/* set the new view boundries */
		wxlen = view->cur_aperture;
		wxlen = wxlen / magnification;
		uv_update_vaperture(view, wxlen);
	}

failed:;

	uu_dexit;
	return(status);
	}

/*********************************************************************
**	 E_FUNCTION : uvu_background_form()
**			This function handles the Background Display Properties form.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS:    none.
**	 SIDE EFFECTS:  none.
**	 WARNINGS: none.
*********************************************************************/
void uvu_background_form()
{
	int i,j,status,inc,idum,ncol;
	int shadsav,colsav[4],rotsav,strsav;
	UU_LOGICAL ifl;
	UX_pathname filsav;
	UM_vector rgbsav[4];
	int *ans[16];
	static UD_METHOD methods[] = {OnShader,
		OnColor,UU_NULL, OnColor,UU_NULL, OnColor,UU_NULL, OnColor,UU_NULL,
		OnBrowse,UU_NULL,UU_NULL,UU_NULL,OnPreview};
	static char called[] = {6, 6,6, 6,6, 6,6, 6,6, 6,6,6,6, 6};
	static char traverse[] = {1, 1,1, 1,1, 1,1, 1,1, 1,1,1,1, 1};
	static char display[] = {1,1,1,1,1, 1, 1,1, 1,1, 1,1, 1,1, 1,1,1,1, 1};
/*
.....Save current background settings
*/
	Sbackg = UV_background;
/*
.....Initialize fields
*/
	Sshader = UV_background.shader;
	strcpy(Sfile,UV_background.bgfile);
	Sbrot = UV_background.rotate;
	Sbstr = UV_background.stretch;
	Spreview = UU_FALSE;
/*
.....Set up default answers &
.....Display fields
*/
	ans[FSHD] = &Sshader;
	inc = 0;
	for (i=0;i<4;i++)
	{
		ans[FCOL1+inc] = &Scolor[i];
		ans[FRGB1+inc] = (int *)Srgb[i];
		inc += 2;
	}
	for (i=0;i<NLAB;i++) display[i] = 0;
	for (i=FCOL1;i<=FSTR;i++) display[i+NLAB] = traverse[i] = 0;
	Sshadfl = Sshader;
	switch (Sshader)
	{
/*
........Solid background
*/
	case 0:
		Scolor[0] = UV_background.bgcolor;
		UM_cc_exttoint(UV_background.bgrgb,Srgb[0]);
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
		Scolor[0] = UV_background.grad[0];
		UM_cc_exttoint(UV_background.grgb[0],Srgb[0]);
		Scolor[1] = UV_background.grad[1];
		UM_cc_exttoint(UV_background.grgb[1],Srgb[1]);
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
			Scolor[i] = UV_background.fcolor[i];
			UM_cc_exttoint(UV_background.frgb[i],Srgb[i]);
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
	ans[FROT] = &Sbrot;
	ans[FSTR] = &Sbstr;
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
	shadsav = Sshader;
	for (i=0;i<4;i++)
	{
		colsav[i] = Scolor[i];
		um_vctovc(Srgb[i],rgbsav[i]);
	}
	strcpy(filsav,Sfile);
	rotsav = Sbrot;
	strsav = Sbstr;
/*
.....Get the Form input
*/
	status = ud_form1("background.frm",ans,ans,methods,called,display,traverse);
/*
.....Restore background settings
*/
	if (status == -1)
	{
		UV_background = Sbackg;
		ifl = Spreview;
		goto done;
	}
/*
.....Determine if background has changed
*/
	if (Sshader != shadsav || Spreview) ifl = UU_TRUE;
	else
	{
		ifl = UU_FALSE;
		ncol = 1;
		if (Sshader == 1) ncol = 2;
		else if (Sshader == 2) ncol = 4;
		else if (Sshader == 3)
		{
			if (strcmp(Sfile,filsav) != 0) ifl = UU_TRUE;
			if (Sbrot != rotsav || Sbstr != strsav) ifl = UU_TRUE;
			ncol = 0;
		}
		for (i=0;i<ncol;i++)
		{
			if (Scolor[i] != colsav[i]) ifl = UU_TRUE;
			else if (Scolor[i] == -1)
			{
				if (!um_cceqcc(Srgb[i],rgbsav[i])) ifl = UU_TRUE;
			}
		}
	}
/*
.....Store Background Shader parameters
*/
	if (ifl)
	{
		S_store_settings();
/*
.....Save the Background modals file
*/
		S_save_modfile();
	}
/*
.....Update the display
.....with the new properties
*/
done:;
	if (ifl)
	{
		uv_set_background();
		ug_setredrwflag(1);
		ud_updatews(UG_PERFORM);
	}
/*
.....End of routine
*/
	return;
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
		Scolor[0] = UV_background.bgcolor;
		UM_cc_exttoint(UV_background.bgrgb,Srgb[0]);
		nfld = 1;
		break;
/*
........Graduated background
*/
	case 1:
      ud_set_display_mask(UD_DISPLAYF,FLTOP,UU_TRUE);
      ud_set_display_mask(UD_DISPLAYF,FLBOT,UU_TRUE);
		Scolor[0] = UV_background.grad[0];
		Scolor[1] = UV_background.grad[1];
		UM_cc_exttoint(UV_background.grgb[0],Srgb[0]);
		UM_cc_exttoint(UV_background.grgb[1],Srgb[1]);
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
			Scolor[i] = UV_background.fcolor[i];
			UM_cc_exttoint(UV_background.frgb[i],Srgb[i]);
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
**    S_FUNCTION     :  OnPreview(filedno, val, stat)
**       Previews the new graphics settings without saving the changes.
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
static UD_FSTAT OnPreview(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Set background settings
*/
	S_store_settings();
/*
.....Update the display
.....with the new properties
*/
	uv_set_background();
	ug_setredrwflag(1);
	ud_updatews(UG_PERFORM);
/*
.....End of routine
*/
	Spreview = UU_TRUE;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     : S_store_settings
**       Sets the background settings from the form settings.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static void S_store_settings()
{
	int ncol,i,j;
/*
.....Store Background Shader parameters
*/
	UV_background.shader = Sshader;
	ncol = 1;
	if (Sshader == 1) ncol = 2;
	else if (Sshader == 2) ncol = 4;
	else if (Sshader == 3) ncol = 0;
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
	switch (Sshader)
	{
/*
........Solid background
*/
	case 0:
		if (Scolor[0] != -1)
		{
			UV_background.bgcolor = Scolor[0];
		}
		else
		{
			UV_background.bgcolor = Scolor[0];
			UM_cc_inttoext(Srgb[0],UV_background.bgrgb);
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
				UV_background.grad[i] = Scolor[i];
			}
			else
			{
				UV_background.grad[i] = Scolor[i];
				UM_cc_inttoext(Srgb[i],UV_background.grgb[i]);
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
				UV_background.fcolor[i] = Scolor[i];
			}
			else
			{
				UV_background.fcolor[i] = Scolor[i];
				UM_cc_inttoext(Srgb[i],UV_background.frgb[i]);
			}
		}
		break;
/*
........Image file
*/
	case 3:
		strcpy(UV_background.bgfile,Sfile);
		UV_background.rotate = Sbrot;
		UV_background.stretch = Sbstr;
		break;
	}
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
	UX_pathname fname;
	FILE *fptr;
	static char yesno[2][10] = {"*NO","*YES"};
	static char cshade[4][10] = {"*SOLID","*GRADUATE","*4-CORNER","*IMAGE"};
	static char bcolor[65][96];
	static char crot[4][10] = {"*0","*90","*180","*270"};
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
	strcpy(fname,"ncl_background.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,fname,
		3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;
/*
.....Store Background modals
*/
	ux_fputs0("\n#BACKGROUND#\n",fptr);
	sprintf(msg,"/SHADER/ %s\n",cshade[UV_background.shader]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/COLOR/ %s\n",bcolor[UV_background.bgcolor+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/RGB/ %g,%g,%g\n",UV_background.bgrgb[0],
		UV_background.bgrgb[1],UV_background.bgrgb[2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/TOP_COLOR/ %s\n",bcolor[UV_background.grad[0]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/BOT_COLOR/ %s\n",bcolor[UV_background.grad[1]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/TOP_RGB/ %g,%g,%g\n",UV_background.grgb[0][0],
		UV_background.grgb[0][1],UV_background.grgb[0][2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/BOT_RGB/ %g,%g,%g\n",UV_background.grgb[1][0],
		UV_background.grgb[1][1],UV_background.grgb[1][2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/UL_COLOR/ %s\n",bcolor[UV_background.fcolor[0]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/UR_COLOR/ %s\n",bcolor[UV_background.fcolor[1]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/LL_COLOR/ %s\n",bcolor[UV_background.fcolor[2]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/LR_COLOR/ %s\n",bcolor[UV_background.fcolor[3]+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/UL_RGB/ %g,%g,%g\n",UV_background.frgb[0][0],
		UV_background.frgb[0][1],UV_background.frgb[0][2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/UR_RGB/ %g,%g,%g\n",UV_background.frgb[1][0],
		UV_background.frgb[1][1],UV_background.frgb[1][2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/LL_RGB/ %g,%g,%g\n",UV_background.frgb[2][0],
		UV_background.frgb[2][1],UV_background.frgb[2][2]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/LR_RGB/ %g,%g,%g\n",UV_background.frgb[3][0],
		UV_background.frgb[3][1],UV_background.frgb[3][2]);
	ux_fputs0(msg,fptr);

/*
.....Not yet supported
*/
/*
	sprintf(msg,"/IMAGE/ %s\n",UV_background.bgfile);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/ROTATE/ %s\n",crot[UV_background.rotate]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/STRETCH/ %s\n",yesno[UV_background.stretch]);
	ux_fputs0(msg,fptr);
*/
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
**	 E_FUNCTION : uv_set_background()
**			This function handles the Background Display Properties form.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS:    none.
**	 SIDE EFFECTS:  none.
**	 WARNINGS: none.
*********************************************************************/
void uv_set_background()
{
	int i,inc;
/*
.....Solid Color
*/
	ug_segerasecolor = 0;
	switch (UV_background.shader)
	{
	case 0:
	case 3:
		if (UV_background.bgcolor == -1)
			um_vctovc(UV_background.bgrgb,UV_background.colors[0]);
		else
		{
			UV_background.colors[0][0] =
				UM_pkcolors[UV_background.bgcolor].red/255;
			UV_background.colors[0][1] =
				UM_pkcolors[UV_background.bgcolor].green/255;
			UV_background.colors[0][2] =
				UM_pkcolors[UV_background.bgcolor].blue/255;
			ug_segerasecolor = UV_background.bgcolor;
		}
		break;
/*
.....Graduated color
*/
	case 1:
		inc = 0;
		for (i=0;i<2;i++,inc+=2)
		{
			if (UV_background.grad[i] == -1)
			{
				um_vctovc(UV_background.grgb[i],UV_background.colors[inc]);
				um_vctovc(UV_background.grgb[i],UV_background.colors[inc+1]);
			}
			else
			{
				UV_background.colors[inc][0] = UV_background.colors[inc+1][0] =
					UM_pkcolors[UV_background.grad[i]].red/255;
				UV_background.colors[inc][1] = UV_background.colors[inc+1][1] =
					UM_pkcolors[UV_background.grad[i]].green/255;
				UV_background.colors[inc][2] = UV_background.colors[inc+1][2] =
					UM_pkcolors[UV_background.grad[i]].blue/255;
			}
		}
		break;
/*
.....4-Corner color
*/
	case 2:
		for (i=0;i<4;i++)
		{
			if (UV_background.fcolor[i] == -1)
			{
				um_vctovc(UV_background.frgb[i],UV_background.colors[i]);
				um_vctovc(UV_background.frgb[i],UV_background.colors[i+1]);
			}
			else
			{
				UV_background.colors[i][0] =
					UM_pkcolors[UV_background.fcolor[i]].red/255;
				UV_background.colors[i][1] =
					UM_pkcolors[UV_background.fcolor[i]].green/255;
				UV_background.colors[i][2] =
					UM_pkcolors[UV_background.fcolor[i]].blue/255;
			}
		}
		break;
	}
}
/**************************************************************************
**  E_FUNCTION:  uv_extrema_project()
**			Project the box defined by points l and r onto the viewing plane
**  PARAMETERS   
**      INPUT  :  l - lower left front of bounding box
**						u - upper right back of bounding box
**						v - view data structure onto which points are projected
**      OUTPUT :  l,u updated after projection onto viewing plane
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
#define UV_UNION 	{ \
				llf[0] = (llf[0]<ppt[0])  ?  llf[0] : ppt[0];\
				llf[1] = (llf[1]<ppt[1])  ?  llf[1] : ppt[1];\
				llf[2] = (llf[2]>ppt[2])  ?  llf[2] : ppt[2];\
				urb[0] = (urb[0]>ppt[0])  ?  urb[0] : ppt[0];\
				urb[1] = (urb[1]>ppt[1])  ?  urb[1] : ppt[1];\
				urb[2] = (urb[2]<ppt[2])  ?  urb[2] : ppt[2];\
					}

uv_extrema_project(l,u,v)
	UU_REAL l[3];
	UU_REAL u[3];
	UV_view *v;

	{
	UU_REAL ppt[3];
	UU_REAL llf[3];
	UU_REAL urb[3];
	int i;

	uu_denter(UU_MTRC,(us,"uv_extrema_project(<%g,%g,%g><%g,%g,%g>)",
							l[0],l[1],l[2],u[0],u[1],u[2]));

	ppt[0] = l[0]; ppt[1] = l[1]; ppt[2] = l[2];
	uvu_mcstovcs(v, 0, ppt, ppt);

	/* assume first point is bounding box */
	for(i=0;i<3;i++) llf[i]=ppt[i];
	for(i=0;i<3;i++) urb[i]=ppt[i];

	ppt[0] = l[0]; ppt[1] = l[1]; ppt[2] = u[2];
	uvu_mcstovcs(v, 0, ppt, ppt);
	UV_UNION;

	ppt[0] = l[0]; ppt[1] = u[1]; ppt[2] = l[2];
	uvu_mcstovcs(v, 0, ppt, ppt);
	UV_UNION;

	ppt[0] = l[0]; ppt[1] = u[1]; ppt[2] = u[2];
	uvu_mcstovcs(v, 0, ppt, ppt);
	UV_UNION;

	ppt[0] = u[0]; ppt[1] = l[1]; ppt[2] = l[2];
	uvu_mcstovcs(v, 0, ppt, ppt);
	UV_UNION;

	ppt[0] = u[0]; ppt[1] = l[1]; ppt[2] = u[2];
	uvu_mcstovcs(v, 0, ppt, ppt);
	UV_UNION;

	ppt[0] = u[0]; ppt[1] = u[1]; ppt[2] = u[2];
	uvu_mcstovcs(v, 0, ppt, ppt);
	UV_UNION;

	ppt[0] = u[0]; ppt[1] = u[1]; ppt[2] = l[2];
	uvu_mcstovcs(v, 0, ppt, ppt);
	UV_UNION;

	for(i=0;i<3;i++) l[i]=llf[i];
	for(i=0;i<3;i++) u[i]=urb[i];
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : uvu_mcstovcs(view, option, mcs, vcs)
**			Convert a vector or cartesian  coordinate specified in the
**			modeling  coordinate system to a vector or cartesian  coordinate
**			specified relative to the viewing system defined by the view
**			record.
**    PARAMETERS   
**       INPUT  : 
**          view           view record
**				option			0 => cartesian  coordinate;
**									1 => vector;
**				mcs				input (modeling coordinate system)
**       OUTPUT :  
**				vcs				output (view coordinate system)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uvu_mcstovcs(view, option, mcs, vcs)
	UV_view   *view;
	int option;
	UM_coord mcs;
	UM_coord vcs;

	{
	UM_vector temp;
	UM_vector xas;

	uu_denter(UU_MTRC,(us,"uvu_mcstovcs()"));
	um_cross(view->cur_up_vect, view->cur_pln_norm, xas);
	if (option == 1) um_vctovc(mcs, temp); else um_vcmnvc(mcs,  
		                                        view->cur_ref_pt, temp);
	vcs[UM_X] = um_dot(temp, xas);
	vcs[UM_Y] = um_dot(temp, view->cur_up_vect);
	vcs[UM_Z] = um_dot(temp, view->cur_pln_norm);
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : uvu_vcstomcs(view, option, vcs, mcs)
**			Convert a vector or cartesian  coordinate specified with the
**			view record  view to a vector or cartestian  coordinate specified
**			relative to the modeling coordinate system.
**    PARAMETERS   
**       INPUT  : 
**          view           view record
**				option		0 => cartesian  coordinate;
**								1 => vector
**				vcs			input (view coordinate system)
**       OUTPUT :  
**				mcs			output (modeling coordinate system)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uvu_vcstomcs(view, option, vcs, mcs)
	UV_view   *view;
	int option;
	UM_coord  vcs;
	UM_coord  mcs;

	{
	UM_vector	temp;
	UM_vector xas;

	um_vctovc(vcs,temp);
	um_cross(view->cur_up_vect, view->cur_pln_norm, xas);
	mcs[ UM_X] = (temp[UM_X] * xas[UM_X] + temp[UM_Y] * view->cur_up_vect[UM_X] 
					+ temp[UM_Z] * view->cur_pln_norm[UM_X] );
	mcs[ UM_Y] = (temp[UM_X] * xas[UM_Y] + temp[UM_Y] * view->cur_up_vect[UM_Y]
					+ temp[UM_Z] * view->cur_pln_norm[UM_Y] );
	mcs[ UM_Z] = (temp[UM_X] * xas[UM_Z] + temp[UM_Y] * view->cur_up_vect[UM_Z]
					+ temp[UM_Z] * view->cur_pln_norm[UM_Z] );
	if (option == 0) um_vcplvc(mcs,  view->cur_ref_pt, mcs);
	return 0;
	}
