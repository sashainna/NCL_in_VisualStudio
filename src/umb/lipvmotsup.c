/*********************************************************************
**    NAME         :  lipvmotsup.c
**       CONTAINS:
**          ul_ipv_playback_preview
**    COPYRIGHT 2015 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       lipvmotsup.c , 25.7
**    DATE AND TIME OF LAST  MODIFICATION
**       05/01/17 , 13:13:39
*********************************************************************/
#include "usysdef.h"
#include "lipv.h"
#include "lipvmplay.h"
#include "lipvstack.h"
#include "lipvmach.h"
#include "nclfc.h"
#include "nclstack.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"

enum
{
	FTYP, FSTK, FKEP, FVIW,
	FREW, FBCK, FMOT, FFWD, FFST, FPLY, FSTP
};

static int Sspeed=100,Stype=0,Ssteps=1,SfrmPlay=0;
static UU_LOGICAL Splay=UU_FALSE;
static UD_FSTAT OnButton(),OnToggle(),OnSpeed(),OnClosePreview();

static int Splay_modal[18],Sact_tool,Snstock,Snkeys;
static UU_LOGICAL Sstandalone=UU_FALSE,Save_session=UU_FALSE;
static UN_clstruc *Slast_clpt;
static UU_LIST *Skey_list;
static UU_LOGICAL Smach_simul=UU_FALSE;

static int S_init_form();
static void S_hide_stocks();

/*********************************************************************
**    E_FUNCTION     : ul_ipv_playback_preview()
**       Controlling routine for Verifying motion previewed from a
**       motion form in NCLIPV.
**    PARAMETERS
**       INPUT  :
**          key_list   = List of surfaces to use for contour stock.
**          nkeys      = Number of keys in 'key_list'.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_playback_preview(key_list,nkeys)
UU_LIST *key_list;
int nkeys;
{
	int status;
	UU_LOGICAL markval=UU_FALSE;
/*
.....Set up form fields
*/
	static UD_METHOD methods[] = {
		OnToggle,OnToggle,UU_NULL,OnToggle,
		OnButton,OnButton,OnSpeed,OnButton,OnButton,OnButton,UU_NULL,
		OnClosePreview};
	static char called[] = {6,6,6,6, 6,6, 6,6, 6,6, 6};
	static char display[] = {1,1,1,1, 1,1, 1,1, 1,1, 1};
	static char traverse[] = {1,1,1,1, 1,1, 1,1, 1,1, 1};
	static int *ans[] = {&Stype,UU_NULL,&UN_keep_stock,UU_NULL,
		UU_NULL,UU_NULL,&Sspeed,UU_NULL,UU_NULL,UU_NULL,&Ssteps};
/*
.....Initialize form
*/
	if (SfrmPlay != 0) goto done;
	status = S_init_form(traverse);
	if (status != UU_SUCCESS) goto done;
	Skey_list = key_list;
	Snkeys = nkeys;
/*
.....Disable machine simulation
*/
	Smach_simul = LW_mach_simul;
	if (Smach_simul) ul_ipv_mach_disable();
/*
.....Start NCLIPV settings
*/
	if (S_init_ipv(traverse) != UU_SUCCESS) 
	{
/*
......end nclipv session before return 
......since S_init_ipv already open NCLIPV window
*/
/*
.....End IPV Session
*/
		ul_ipv_end();
/*
.....Enable machine simulation if it was previously disabled
*/
		if (Smach_simul) ul_ipv_mach_enable();
			goto done;
	}
/*
.....Get the Form input
*/
	SfrmPlay = ud_form_display1("ipvpreview.frm", ans, ans, methods, called,
		display, traverse);
	if (SfrmPlay == -1)
	{
		SfrmPlay = 0;
		ud_wrerr("Could not display Playback Preview Motion form.");
	}
/*
....End of routine
*/
done:;
	return(SfrmPlay);
}

/*********************************************************************
**    I_FUNCTION     :  OnToggle(fieldno, val, stat)
**       Callback routine for form fields.
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
	int i;
	switch (*fieldno)
	{
/*
.....Stock Type
*/
	case FTYP:
		if (Stype == 2 && Snkeys == 0)
		{
			Stype = 0;
			ud_dispfrm_update_answer(SfrmPlay,FTYP,&Stype);
		}
		if (Stype == 0 && LW_nstock[0] == 0)
		{
			Stype = 1;
			ud_dispfrm_update_answer(SfrmPlay,FTYP,&Stype);
		}
		i = 0; if (Stype != 0) i = 1;
		ud_setfrm_traverse_mask(SfrmPlay,FSTK,i);
/*
........Going from Existing to Standalone Stock
........Delete the standalone stock
*/
		if (Sstandalone && LW_nstock[0] > Snstock)
			ul_ipv_remove_stock_ind(0,-1);
/*
........Restore the IPV session
*/
		if (Stype == 0)
		{
			if (Snstock > 0)
			{
				if (Sstandalone && Save_session) ul_ipv_restore_session();
				ud_setfrm_traverse_mask(SfrmPlay,FKEP,1);
			}
			LW_nstock[0] = Snstock;
			Sstandalone = UU_FALSE;
		}
		break;
/*
.....Define Stock
*/
	case FSTK:
/*
........Delete previously created stock
*/
		if (Sstandalone && LW_nstock[0] > Snstock)
			ul_ipv_remove_stock_ind(0,-1);
/*
........Hide previously defined stocks/fixtures
*/
		else if (!Sstandalone && LW_nstock[0] >= Snstock && Snstock != 0)
		{
			S_hide_stocks();
			UN_keep_stock = 0;
			ud_dispfrm_update_answer(SfrmPlay,FKEP,&UN_keep_stock);
			ud_setfrm_traverse_mask(SfrmPlay,FKEP,UU_FALSE);
			Sstandalone = UU_TRUE;
		}
/*
........Define Stock
*/
		switch (Stype)
		{
		case 1:
			ul_verify_box(0,3);
			break;
		case 2:
			ul_verify_contour(0,Skey_list,UU_FALSE);
			break;
		case 3:
			ul_ipv_load_stock();
			break;
		case 4:
			ul_ipv_load_stl(0);
			break;
		}
/*
........Enable playback buttons
*/
		if (LW_nstock[0] > Snstock)
		{
			Sstandalone = UU_TRUE;
			ud_setfrm_traverse_mask(SfrmPlay,FREW,UU_TRUE);
			ud_setfrm_traverse_mask(SfrmPlay,FPLY,LW_mot_stack_active);
			ud_setfrm_traverse_mask(SfrmPlay,FFWD,UU_TRUE);
			ud_setfrm_traverse_mask(SfrmPlay,FFST,UU_TRUE);
			ud_setfrm_traverse_mask(SfrmPlay,FPLY,UU_TRUE);
		}
		break;
/*
.....Enter Dynamic Viewing
*/
	case FVIW:
		if (ul_ipv_view_active()) uz_dyn_mouse(UU_TRUE);
		break;
	}
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnButton(fieldno, val, stat)
**       Routine to handle form push buttons.
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
static UD_FSTAT OnButton(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL idid;
/*
.....Set correct pointers
.....if at end of playback
*/
	if (UN_clfile_current == UU_NULL) UN_clfile_current = Slast_clpt;
/*
.....Rewind
*/
	switch (*fieldno)
	{
	case FREW:
		ul_ipv_deselect_tool();
		if (LW_mot_stack_active)
		{
			ul_ipv_mot_stack_delete(UU_TRUE);
			ul_ipv_mot_stack_init();
		}
		if (!Sstandalone)
		{
			if (Save_session) ul_ipv_restore_session();
		}
		else
			ul_ipv_reset_prim(0,-1);
		LW_progress_count = 0;
		UN_clfile_current = Slast_clpt = UU_NULL;
		ul_ipv_flush();

		idid = ul_ipv_mot_stack_step(1,"");
		if (!idid)
		{
			Splay_modal[1] = 100;
			Splay_modal[2] = -1;
			Splay_modal[3] = 1;
			LW_act_tool[0] = LW_ntool - 1;
			Splay_modal[0] = 1;
			ncl_motion_playback(Splay_modal,0,UU_NULL,&LW_tool_list,&LW_ntool);
			Slast_clpt = UN_clpt[2];
		}
		break;
/*
.....Step backwards
*/
	case FBCK:
		ul_ipv_mot_stack_step(-1,"");
		break;
/*
.....Step forwards
*/
	case FFWD:
		idid = ul_ipv_mot_stack_step(1,"");
		if (!idid)
		{
			Splay_modal[1] = 100;
			Splay_modal[2] = -1;
			Splay_modal[3] = 1;
			LW_act_tool[0] = LW_ntool - 1;
			Splay_modal[0] = 1;
			ncl_motion_playback(Splay_modal,0,UU_NULL,&LW_tool_list,&LW_ntool);
			Slast_clpt = UN_clpt[2];
		}
		break;
/*
.....Fast forward
.....Motion Playback
*/
	case FFST:
	case FPLY:
/*
........Step to end of UNDO stack
*/
		Splay = UU_TRUE;
		if (*fieldno == FFST)
		{
			ul_ipv_mot_stack_step(2,"");
			Splay_modal[1] = 100;
			Splay_modal[3] = 100000000;
		}
		else
		{
			do
			{
				idid = ul_ipv_mot_stack_step(1,"");
			} while (idid);
			Splay_modal[1] = Sspeed;
			Splay_modal[3] = Ssteps;
		}
/*
........Verify preview motion
*/
		LW_act_tool[0] = LW_ntool - 1;
		Splay_modal[0] = 1;
		Splay_modal[2] = 0;
		ncl_motion_playback(Splay_modal,0,UU_NULL,&LW_tool_list,&LW_ntool);
		Slast_clpt = UN_clpt[2];
		Splay = UU_FALSE;
		break;
	}
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnSpeed(fieldno, val, stat)
**       Callback routine for form fields.
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
static UD_FSTAT OnSpeed(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
/*
.....Speed
*/
	case FMOT:
		Sspeed = val->frmint[0];
		ncl_playback_speed(Sspeed);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnClosePreview()
**       Closes the Preview Playback form.
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
static UD_FSTAT OnClosePreview()
{
	int nstock;
/*
.....Stop the simulation
*/
	if (Splay) setintr();
	Splay = UU_FALSE;
/*
.....Standalone stock was created
*/
	if (Sstandalone)
	{
		nstock = Snstock;
		ul_ipv_deselect_tool();
/*
........Delete the current stock
*/
		if ((Snstock > 0 || UN_keep_stock == 0) && LW_nstock[0] > Snstock)
			ul_ipv_remove_stock_ind(0,-1);
/*
........Add current stock to existing stocks
*/
		else if (LW_nstock[0] > Snstock && UN_keep_stock == 1)
			Snstock++;
/*
........Restore the IPV session
*/
		if (nstock > 0 && Save_session) ul_ipv_restore_session();
		LW_nstock[0] = Snstock;
	}
/*
.....Existing stock was used
*/
	else
	{
		if (!UN_keep_stock && Save_session) 
		{
/*
......need remove the current stock and motion stock if 
......"Keep Stock" not check since some ipv env such as Ssession_start
......Ssession_end need reset
......Yurong
*/
			if (Snstock > 0 || UN_keep_stock == 0)
				ul_ipv_remove_stock_ind(0,-1);
			if (LW_mot_stack_active)
			{
				ul_ipv_mot_stack_delete(UU_TRUE);
			}
			ul_ipv_restore_session();
		}
	}
/*
.....End IPV Session
*/
	ul_ipv_end();
/*
.....Enable machine simulation if it was previously disabled
*/
	if (Smach_simul) ul_ipv_mach_enable();
/*
.....Redisplay the main form
*/
	SfrmPlay = 0;
	ud_form_vis();
/*
.....Restore global IPV variables
*/
	LW_act_tool[0] = Sact_tool;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  S_init_form(traverse)
**       Initialize the field traversal settings.
**    PARAMETERS
**       INPUT  :
**          traverse  = Field traversal settings.
**       OUTPUT :
**          traverse  = Updated field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_init_form(traverse)
char *traverse;
{
/*
.....End of routine
*/
done:;
	Sstandalone = UU_FALSE;
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  S_init_ipv(traverse)
**       Initialize NCLIPV for Preview style verification.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          traverse  = Updated field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_init_ipv(traverse)
char *traverse;
{
	int i,iclw[6],cfl[3],status;
	UU_REAL cutr[20];
	char sym[3][MAXSYMLEN];
	UU_KEY_ID key[3];
	UN_cutter_list *cpt;
/*
.....Open the IPV window
*/
	ncl_pause_temp_clfile();
	status = ul_ipv_start(UU_FALSE);
	ncl_resume_temp_clfile();
	if (status != UU_SUCCESS) goto failed;
/*
.....Save global IPV variables
*/
	Sact_tool = LW_act_tool[0];
	Snstock = LW_nstock[0];
	Save_session = UU_FALSE;
/*
.....Set the stock field traversals
.....Based on if there are any defined stocks
*/
	if (Stype == 0 && LW_nstock[0] == 0) 
		Stype = 1;

	if (Stype == 0) 
		traverse[FSTK] = 0;
	else
		traverse[FSTK] = 1;

	if (LW_nstock[0] != 0 && Stype != 0) 
		traverse[FKEP] = 0;
	else
		traverse[FKEP] = 1;

	if (!LW_mot_stack_active) 
		traverse[FBCK] = 0;
	else 
		traverse[FBCK] = 1;

	if (LW_nstock[0] == 0)
	{
		traverse[FREW] = 0;
		traverse[FFWD] = 0;
		traverse[FFST] = 0;
		traverse[FPLY] = 0;
	}
	else
	{
		traverse[FREW] = 1;
		traverse[FFWD] = 1;
		traverse[FFST] = 1;
		traverse[FPLY] = 1;
	}
/*
.....Define playback settings
*/
	for (i=0;i<18;i++) Splay_modal[i] = 0;
	Splay_modal[14] = 1;
	Splay_modal[17] = 4;
	UN_clfile_current = UU_NULL;
	UN_clfile_curpt = 0;
	Slast_clpt = UU_NULL;
/*
.....Initialize Monitor form
*/
	if (LW_monitor_field[IPVMON_PROGRESS])
	{
		LW_progress_count = 0;
		LW_progress_total = ncl_count_clrec();
	}
/*
.....Define tools
*/
	ncl_pause_temp_clfile();
	ul_ipv_append_tools(&LW_tool_list,&LW_ntool);
	ncl_resume_temp_clfile();
	if (LW_ntool == 0) goto nocutter;
	cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	iclw[0] = cpt[LW_ntool-1].isn; iclw[5] = cpt[LW_ntool-1].clrec;
	ncl_process_ipvcutter(0,iclw,cutr,cfl,&LW_tool_list,sym[0],sym[1],sym[2],key);
/*
.....Save the current session
*/
	if (Snstock > 0)
	{
		Save_session = UU_TRUE;
		ul_ipv_save_session();
	}
/*
.....Save the motion stack
*/
	if (LW_mot_stack_active)
		ul_ipv_mot_stack_save();
	else
	{
/*
.....if we don't have undo stack, we
.....still need clear and reset
*/
		ul_ipv_mot_stack_del_sess();
/*
.....Reset the motion stack
*/
		ul_ipv_mot_stack_init();
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Could not initialize NCLIPV
*/
failed:;
	ud_wrerr("Could not initialize NCLIPV.");
	status = UU_FAILURE;
	for (i=FTYP;i<=FSTP;i++) ud_setfrm_traverse_mask(SfrmPlay,i,0);
	goto done;
/*
.....No cutters defined
*/
nocutter:;
	ud_wrerr("There are no defined tools.");
	status = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_hide_stocks()
**       Hides existing stocks/fixtures when using a local stock for
**       verification.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_hide_stocks()
{
	int i,j,ifl,nstock;
	LW_stock_struc *sptr,*sdtmp;
	LtData data;
/*
.....Initialize routine
*/
	nstock = LW_nstock[0];
	LW_nstock[0] = Snstock;
/*
.....Hide all stocks and fixtures
*/
	if (LW_active)
	{
		for (j=0;j<2;j++)
		{
			sptr = LW_stock_first[j];
			LiDataSetBoolean(&data,FALSE);
			for (i=0;i<LW_nstock[j];i++)
			{
				ifl = 0;
				do
				{
					ul_ipv_get_next_stock(sptr,&sdtmp,&ifl,UU_FALSE);
					if (ifl == -2) break;
					ul_ipv_invis_stock(sdtmp);
				} while (ifl != -1);
				sptr = (LW_stock_struc *)uu_lsnext(sptr);
			}
		}
	}
	LW_nstock[0] = nstock;
}
