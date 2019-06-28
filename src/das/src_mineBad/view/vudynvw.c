
/*********************************************************************
**    NAME         :  vudynvw.c
**       CONTAINS: user interface for dynamic viewing
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       vudynvw.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       02/08/16 , 09:19:28
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "dasnog.h"
#include "dtypes.h"
#include "view.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "spmouse.h"
#include "lipv.h"
#include "lumb.h"
/*
.....added for OpenGL
.....Yurong 8/17/98
*/
#include "driver.h"
#ifdef UU_OPENGL
#include "wsgl.h"
#endif

extern int UZ_nclipv_view;

int UV_dyndisply = 0;
int UV_dyncenter = 0;
int LW_dyncenter = 0;
int LW_dyn_zval = 0;
int UV_dynstatln = 1;
int UV_dynsegs = 10000;
int UV_dyn_zval = 0;
int UV_dynwheel = 0;
UU_REAL UV_dyncp[3],UV_dynvec[3];
UU_REAL LW_dyncp[3];
UV_view UV_dynview;
static char *vcbuf = {"Enter dynamic viewing center."};
static char *vvbuf = {"Enter dynamic rotation vector."};
static UU_REAL tmp_ans1, tmp_ans2, tmp_ans3, tmp_ans4, tmp_ans5, tmp_ans6, tmp_ans7, tmp_ans8, tmp_ans9;
static int sen_form_opened = 0;
static int S_dyn_center, S_ipvform;
static UU_REAL S_dyncen[3];
/*********************************************************************
**    I_FUNCTION     : S_save_modfile
**       Save the VIEW properties into modals file.
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
	int stat;
	char msg[80];
	UX_pathname fname;
	FILE *fptr;
	static char onoff[2][10] = {"*OFF","*ON"};
	static char dpart[3][10] = {"*PART","*AXIS", "*BOTH"};
	static char vrot[3][10] = {"*VIEWPORT","*USER", "*AUTO"};
	static char zcen[3][10] = {"*PICK","*CALC", "*OFF"};
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	if (LW_nclipv!=LW_STANDALONE)
		strcpy(fname,"ncl_view.mod");
	else
		strcpy(fname,"nclipv_view.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,fname,
		3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;
/*
.....Store VIEW modals
*/
	ux_fputs0("#VIEW#\n", fptr);

	sprintf(msg,"/MS_PAN_GAIN/ %f\n",UV_MS_pangain);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/MS_ROTATE_GAIN/ %f\n",UV_MS_rotgain);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/MS_ZOOM_GAIN/ %f\n",UV_MS_zoomgain);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/KB_PAN_GAIN/ %f\n",UV_KB_pangain);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/KB_ROTATE_GAIN/ %f\n",UV_KB_rotgain);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/KB_ZOOM_GAIN/ %f\n",UV_KB_zoomgain);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SM_PAN_GAIN/ %f\n",UV_SM_pangain);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SM_ROTATE_GAIN/ %f\n",UV_SM_rotgain);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SM_ZOOM_GAIN/ %f\n",UV_SM_zoomgain);
	ux_fputs0(msg, fptr);

	if (LW_nclipv!=LW_STANDALONE)
	{
		sprintf(msg,"/DISPLAY/ %s\n",dpart[UV_dyndisply]);
		ux_fputs0(msg, fptr);
	}
	sprintf(msg,"/CENTER/ %s\n",vrot[UV_dyncenter]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/Z_CENTER/ %s\n",zcen[UV_dyn_zval]);
	ux_fputs0(msg, fptr);

	if (LW_nclipv!=LW_STANDALONE)
	{
		sprintf(msg,"/STATUS_LINE/ %s\n",onoff[UV_dynstatln]);
		ux_fputs0(msg, fptr);
		sprintf(msg,"/SEGMENT/ %d\n",UV_dynsegs);
		ux_fputs0(msg, fptr);
	}
		
	sprintf(msg,"/MS_WHEEL/ %s\n",onoff[UV_dynwheel]);
	ux_fputs0(msg, fptr);
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
**    S_FUNCTION     :  Sselect_center(filedno, val, stat)
**       Method called when Sensitivity button is pushed
**    PARAMETERS   
**       INPUT  : fieldno	Field number being changed.
**                val		Current field value.
**                stat		Field status.
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT Sselect_center(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	void uvu_dyncenter(), ul_ipv_dyncenter();
	int cmdreject, cen_chc, sav_uvdyn_center, sav_lwdyn_center, tempchc;
	UU_REAL sav_dyncen[3];
	cen_chc = *(val->frmint);
	if (cen_chc!=1) 
	{
		S_dyn_center = cen_chc;
		return UD_FLDOK;
	}
	sav_uvdyn_center = UV_dyncenter;
	sav_lwdyn_center = LW_dyncenter;
	tempchc = S_dyn_center;
/*
.....Take down the form
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;

	if (S_ipvform==0)
	{
		sav_dyncen[0] = UV_dyncp[0];
		sav_dyncen[1] = UV_dyncp[1];
		sav_dyncen[2] = UV_dyncp[2];
		uvu_dyncenter();
		if (UV_dyncenter!=1)
		{
/*
......reset choice back to old value before change
*/
			tempchc = S_dyn_center;
		}
		else
		{
			tempchc = UV_dyncenter;
			S_dyncen[0] = UV_dyncp[0];
			S_dyncen[1] = UV_dyncp[1];
			S_dyncen[2] = UV_dyncp[2];
			UV_dyncp[0] = sav_dyncen[0];
			UV_dyncp[1] = sav_dyncen[1];
			UV_dyncp[2] = sav_dyncen[2];
		}
		S_dyn_center = tempchc;
	}
	else
	{
		if (LW_active)
		{
			sav_dyncen[0] = LW_dyncp[0];
			sav_dyncen[1] = LW_dyncp[1];
			sav_dyncen[2] = LW_dyncp[2];
			ul_ipv_dyncenter();
			if (LW_dyncenter!=1)
			{
/*
......reset choice back to old value before change
*/
				tempchc = S_dyn_center;
			}
			else
			{
				tempchc = LW_dyncenter;
				S_dyncen[0] = LW_dyncp[0];
				S_dyncen[1] = LW_dyncp[1];
				S_dyncen[2] = LW_dyncp[2];
				LW_dyncp[0] = sav_dyncen[0];
				LW_dyncp[1] = sav_dyncen[1];
				LW_dyncp[2] = sav_dyncen[2];
			}
		}
		else
		{
			ud_wrerr("IPV not active, can't assign the dynmamic center for IPV.");
			tempchc = S_dyn_center;
		}
		S_dyn_center = tempchc;
	}
done:;
	ud_form_vis();
	ud_dispfrm_update_answer(0, 1, (int *)&tempchc);
	UV_dyncenter = sav_uvdyn_center;
	LW_dyncenter = sav_lwdyn_center;
	UD_UNMARK(cmdreject);
	return UD_FLDOK;
}

/*********************************************************************
**    S_FUNCTION     :  uv_show_sensi(filedno, val, stat)
**       Method called when Sensitivity button is pushed
**    PARAMETERS   
**       INPUT  : fieldno	Field number being changed.
**                val		Current field value.
**                stat		Field status.
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT uv_show_sensi(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status, *ans[9];

	ans[0] = (int *)&tmp_ans1;
	ans[1] = (int *)&tmp_ans2;
	ans[2] = (int *)&tmp_ans3;
	ans[3] = (int *)&tmp_ans4;
	ans[4] = (int *)&tmp_ans5;
	ans[5] = (int *)&tmp_ans6;
	ans[6] = (int *)&tmp_ans7;
	ans[7] = (int *)&tmp_ans8;
	ans[8] = (int *)&tmp_ans9;
	status = ud_form_display("viewsens.frm", ans, ans);
	sen_form_opened = 1;
	return UD_FLDOK;
}
/********************************************************************* 
**  E_FUNCTION:        uvu_dynmodals
**      Dynamic viewing modals form.
**  PARAMETERS   
**      INPUT:    none
**      OUTPUT:   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uvu_dynmodals()
{
	int disp[9],cen, ipvcen, sline[5],dseg[5],zval;
	int status, *ans[9];
	int sens, wheel;
	static char called[] = {6,6,6,6,6,6,6,6};
	static UD_METHOD methods1[] = {UU_NULL,Sselect_center,UU_NULL,UU_NULL,UU_NULL, UU_NULL, uv_show_sensi};
	static UD_METHOD methods2[] = {Sselect_center, UU_NULL, UU_NULL, uv_show_sensi};
/*
.....Load the form
.....5 entries in form
........Field 0 is Dynamic Display
*/
	disp[0] = UV_dyndisply;
	S_dyn_center = UV_dyncenter;
/*
........Field 1 is Rotation Center
*/
	sline[0] = UV_dynstatln;
	dseg[0] = UV_dynsegs;
	S_ipvform = 0;
	wheel = UV_dynwheel;
	if (LW_nclipv!=LW_STANDALONE)
	{
		cen = UV_dyncenter;
		zval = UV_dyn_zval;
		ans[0] = (int *)disp;
		ans[1] = (int *)&cen;
		ans[2] = (int *)&zval;
/*
........Field 4 is Status line display
*/
		ans[3] = (int *)sline;
/*
........Field 5 is # of segments to display
*/
		ans[4] = (int *)dseg;
		ans[5] = (int *)&wheel;
		ans[6] = &sens;
	}
	else
	{
		cen = LW_dyncenter;
		zval = LW_dyn_zval;
		ans[0] = (int *)&cen;
		ans[1] = (int *)&zval;
		ans[2] = (int *)&wheel;
		ans[3] = &sens;
	}
	if (LW_nclipv==LW_STANDALONE)
	{
		S_ipvform = 1;
		if (S_dyn_center)
		{
			S_dyncen[0] = LW_dyncp[0] ;
			S_dyncen[1] = LW_dyncp[1];
			S_dyncen[2] = LW_dyncp[2];
		}
	}
	else
	{
		if (S_dyn_center)
		{
			S_dyncen[0] = UV_dyncp[0] ;
			S_dyncen[1] = UV_dyncp[1];
			S_dyncen[2] = UV_dyncp[2];
		}
	}
	tmp_ans1 = UV_MS_pangain;
	tmp_ans2 = UV_MS_rotgain;
	tmp_ans3 = UV_MS_zoomgain;
	tmp_ans4 = UV_KB_pangain;
	tmp_ans5 = UV_KB_rotgain;
	tmp_ans6 = UV_KB_zoomgain;
	tmp_ans7 = UV_SM_pangain;
	tmp_ans8 = UV_SM_rotgain;
	tmp_ans9 = UV_SM_zoomgain;
/*
.....Get the Form input
*/
	if (LW_nclipv!=LW_STANDALONE)
	{
		status = ud_form1("dynview.frm", ans, ans,methods1,called,UU_NULL,UU_NULL);
	}
	else
	{
		status = ud_form1("dynviewipv.frm", ans, ans,methods2,called,UU_NULL,UU_NULL);
	}
/*
.....Store form responses
*/
	if (status==-1)
		return;
	UV_dyndisply = disp[0];
	UV_dyncenter = cen;
	LW_dyncenter = cen;
	UV_dyn_zval = zval;
	LW_dyn_zval = zval;
	if (cen)
	{
		UV_dyncp[0] = LW_dyncp[0] = S_dyncen[0];
		UV_dyncp[1] = LW_dyncp[1] = S_dyncen[1];
		UV_dyncp[2] = LW_dyncp[2] = S_dyncen[2];
	}

	UV_dynstatln = sline[0];
	UV_dynsegs = dseg[0];
	UV_dynwheel = wheel;
	if (sen_form_opened==1)
	{
		UV_MS_pangain = (float)tmp_ans1;
		UV_MS_rotgain = (float)tmp_ans2;
		UV_MS_zoomgain = (float)tmp_ans3;
		UV_KB_pangain = (float)tmp_ans4;
		UV_KB_rotgain = (float)tmp_ans5;
		UV_KB_zoomgain = (float)tmp_ans6;
		UV_SM_pangain = (float)tmp_ans7;
		UV_SM_rotgain = (float)tmp_ans8;
		UV_SM_zoomgain = (float)tmp_ans9;
	}	
	S_save_modfile();
	sen_form_opened = 0;
	return;
}

/********************************************************************* 
**  E_FUNCTION:        uvu_dyncenter
**      Let the user change the dynamic viewing center.
**  PARAMETERS   
**      INPUT:    none
**      OUTPUT:   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uvu_dyncenter()

{
	int stat, numint, type, device, pet;
	UU_LOGICAL cmdreject;

	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0) goto fini;
/*
.....Save coord defaults and set to pick.
*/
	ud_qcord(&type, &device, &pet);
	ud_dtcord(UD_PICK,1,1);
/*
.....Get dynamic viewing center
*/
	stat = ud_ddas_nearpt(vcbuf, UV_dyncp, &numint);
	if (stat == 1 && numint > 0) 
	{
		UV_dyncenter = 1;
		if (UZ_nclipv_view)
		{
			if (LW_dyncenter == 0)
			{
				LW_dyncp[0] = UV_dynview.cur_ref_pt[0];
				LW_dyncp[1] = UV_dynview.cur_ref_pt[1];
				LW_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
		else
		{
			if (UV_dyncenter == 0)
			{
				UV_dyncp[0] = UV_dynview.cur_ref_pt[0];
				UV_dyncp[1] = UV_dynview.cur_ref_pt[1];
				UV_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
	}
/*
.....Restore coord defaults.
*/
	ud_dtcord(type, device, pet);

fini:
	UD_UNMARK(cmdreject);
}

/********************************************************************* 
**  E_FUNCTION:        uvu_dynpan
**      Invoke dynamic pan on the terminal
**  PARAMETERS   
**      INPUT:    none
**      OUTPUT:   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uvu_dynpan()

{
	UV_vport	vport;					/* view port in which dynamic pan to occurs */

	uu_denter(UU_MTRC,(us,"uvu_dynpan()"));
	if (uvu_pickvp(&vport) == UU_SUCCESS)
	{
		uv_getvid(vport.cur_view,&UV_dynview);
/*		UV_dyncp[0] = UV_dynview.cur_ref_pt[0];
		UV_dyncp[1] = UV_dynview.cur_ref_pt[1];
		UV_dyncp[2] = UV_dynview.cur_ref_pt[2];*/
		uv_delete_hidden(&vport);
		uv_dynpan(&vport);
	}
	uu_dexit;
	}

/********************************************************************* 
**  E_FUNCTION:        uvu_dynzoom
**      Let the user dynamically change the magnification of the display.
**  PARAMETERS   
**      INPUT:    none
**      OUTPUT:   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uvu_dynzoom()

{
/*	int stat,numint;*/
	UV_vport	vport;					/* view port in which dynamic zoom occurs */

	uu_denter( UU_MTRC,(us,"uvu_dynzoom()"));
	if (uvu_pickvp(&vport) == UU_SUCCESS) 
	{
		uv_getvid(vport.cur_view,&UV_dynview);
/*
.....Get center of viewport
*/
		if (UZ_nclipv_view)
		{
			if (LW_dyncenter == 0)
			{
				LW_dyncp[0] = UV_dynview.cur_ref_pt[0];
				LW_dyncp[1] = UV_dynview.cur_ref_pt[1];
				LW_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
		else
		{
			if (UV_dyncenter == 0)
			{
				UV_dyncp[0] = UV_dynview.cur_ref_pt[0];
				UV_dyncp[1] = UV_dynview.cur_ref_pt[1];
				UV_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
		uv_delete_hidden(&vport);
		uv_dynzoom(&vport);
	}
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:        uvu_dynzrot
**      Dynamically change the z axis rotation.
**  PARAMETERS   
**      INPUT:    none
**      OUTPUT:   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uvu_dynzrot()

{
	UV_vport	vport;			/* view port in which dynamic z rotation occurs */
/*	int numint,stat;*/

	uu_denter( UU_MTRC,(us,"uvu_dynzrot()"));
	if (uvu_pickvp(&vport) == UU_SUCCESS)
	{
		uv_delete_hidden(&vport);
/*
.....Get center of viewport
*/
		uv_getvid(vport.cur_view,&UV_dynview);
		if (UZ_nclipv_view)
		{
			if (LW_dyncenter == 0)
			{
				LW_dyncp[0] = UV_dynview.cur_ref_pt[0];
				LW_dyncp[1] = UV_dynview.cur_ref_pt[1];
				LW_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
		else
		{
			if (UV_dyncenter == 0)
			{
				UV_dyncp[0] = UV_dynview.cur_ref_pt[0];
				UV_dyncp[1] = UV_dynview.cur_ref_pt[1];
				UV_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
		uv_dynzrot(&vport);
	}
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:        uvu_dynxyrot
**      Change the view plane normal.
**  PARAMETERS   
**      INPUT:    none
**      OUTPUT:   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uvu_dynxyrot()
{
	UV_vport	vport;			/* view port in which dynamic xy rotation occurs */
/*	int numint,stat;*/

	uu_denter( UU_MTRC,(us,"uvu_dynxyrot()"));
	if (uvu_pickvp(&vport) == UU_SUCCESS)
	{
		uv_delete_hidden(&vport);
/*
.....Get center of viewport
*/
		uv_getvid(vport.cur_view,&UV_dynview);
		if (UZ_nclipv_view)
		{
			if (LW_dyncenter == 0)
			{
				LW_dyncp[0] = UV_dynview.cur_ref_pt[0];
				LW_dyncp[1] = UV_dynview.cur_ref_pt[1];
				LW_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
		else
		{
			if (UV_dyncenter == 0)
			{
				UV_dyncp[0] = UV_dynview.cur_ref_pt[0];
				UV_dyncp[1] = UV_dynview.cur_ref_pt[1];
				UV_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
		uv_dynxyrot(&vport);
	}
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:        uvu_dyntumble
**      Change the view plane normal.
**  PARAMETERS   
**      INPUT:    none
**      OUTPUT:   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uvu_dyntumble()
{
	UV_vport	vport;			/* view port in which dynamic xy rotation occurs */
/*	int numint,stat;*/

	uu_denter( UU_MTRC,(us,"uvu_dyntumble()"));
	if (uvu_pickvp(&vport) == UU_SUCCESS)
		{
		uv_delete_hidden(&vport);
/*
.....Get center of viewport
*/
		uv_getvid(vport.cur_view,&UV_dynview);
		if (UZ_nclipv_view)
		{
			if (LW_dyncenter == 0)
			{
				LW_dyncp[0] = UV_dynview.cur_ref_pt[0];
				LW_dyncp[1] = UV_dynview.cur_ref_pt[1];
				LW_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
		else
		{
			if (UV_dyncenter == 0)
			{
				UV_dyncp[0] = UV_dynview.cur_ref_pt[0];
				UV_dyncp[1] = UV_dynview.cur_ref_pt[1];
				UV_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
		uv_dyntumble(&vport);
	}
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:        uvu_dynvecrot
**      Change the view plane normal.
**  PARAMETERS   
**      INPUT:    none
**      OUTPUT:   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uvu_dynvecrot()
{
	UV_vport	vport;			/* view port in which dynamic xy rotation occurs */
	int numint,stat;

	if (uvu_pickvp(&vport) == UU_SUCCESS)
	{
		uv_delete_hidden(&vport);
/*
.....Get center of viewport
*/
		UD_locint = UD_PICK;
		stat = ud_ddas(UD_DASCART,vcbuf,UV_dyncp,1,&numint);
		if (stat == 1 && numint > 0)
		{
			UD_vecint = UD_PICK;
			stat = ud_ddas(UD_DASVEC,vvbuf,UV_dynvec,1,&numint);
			if (stat == 1 && numint > 0)
			{
				uv_getvid(vport.cur_view,&UV_dynview);
				uv_dynvecrot(&vport);
			}
		}
	}
}

/********************************************************************* 
**  E_FUNCTION:        uvu_dynmouse
**      Invoke dynamic mouse viewing on the terminal
**  PARAMETERS   
**      INPUT:    none
**      OUTPUT:   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uvu_dynmouse()

{
	UV_vport	vport;					/* view port in which dynamic pan to occurs */

	uu_denter(UU_MTRC,(us,"uvu_dynpan()"));
	if (uvu_pickvp(&vport) == UU_SUCCESS)
	{
		uv_getvid(vport.cur_view,&UV_dynview);
		if (UZ_nclipv_view)
		{
			if (LW_dyncenter == 0)
			{
				LW_dyncp[0] = UV_dynview.cur_ref_pt[0];
				LW_dyncp[1] = UV_dynview.cur_ref_pt[1];
				LW_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
		else
		{
			if (UV_dyncenter == 0)
			{
				UV_dyncp[0] = UV_dynview.cur_ref_pt[0];
				UV_dyncp[1] = UV_dynview.cur_ref_pt[1];
				UV_dyncp[2] = UV_dynview.cur_ref_pt[2];
			}
		}
		uv_delete_hidden(&vport);
		uv_dynmouse(&vport);
	}
	uu_dexit;
}
