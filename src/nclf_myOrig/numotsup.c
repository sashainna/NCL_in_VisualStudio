/*********************************************************************
**    NAME         :  numotsup.c
**       CONTAINS:
**          nclu_playback_preview
**          nclu_save_preview_modals
**    COPYRIGHT 2015 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       numotsup.c , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**       10/27/16 , 13:22:15
*********************************************************************/
#include "usysdef.h"
#include "gtbl.h"
#include "mdpick.h"
#include "nclfc.h"
#include "nclstack.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"

enum
{
	FREW,
	FBCK,
	FMOT,
	FFWD,
	FFST,
	FPLY
};

extern char uw_color_name[64][96];

static int Sspeed,SfrmPlay=0;
static UU_LOGICAL Splay=UU_FALSE;
static UD_FSTAT OnButton(),OnClosePreview();

/*********************************************************************
**    E_FUNCTION     : nclu_playback_preview()
**       Controlling routine for Playing Back motion previewed from a
**       motion form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_playback_preview()
{
	int i,j,status,iend,mary[3];
	UU_LOGICAL markval=UU_FALSE;
	UD_METHOD save_entry;
	UN_motseg *mlist[3];
/*
.....Set up form fields
*/
	static UD_METHOD methods[] = {OnButton,OnButton,UU_NULL,OnButton,
		OnButton,OnButton,OnClosePreview};
	static char called[] = {6,6, 6, 6,6, 6};
	static char display[] = {1,1, 1, 1,1, 1};
	static char traverse[] = {1,1, 1, 1,1, 1};
	static int *ans[] = {UU_NULL,UU_NULL,(int *)&Sspeed,UU_NULL,
		UU_NULL,UU_NULL};
/*
.....Initialize form
*/
	if (SfrmPlay != 0) goto done;
	Sspeed = 100;
/*
.....Get the Form input
*/
	SfrmPlay = ud_form_display1("playpreview.frm", ans, ans, methods, called,
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
**    E_FUNCTION     : nclu_save_preview_modals()
**       Save the Preview motion settings into an external modal file.
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
int nclu_save_preview_modals()
{
	int i, stat;
	char msg[80];
	FILE *fptr;
	UX_pathname fname;
	char slcolor[65][96];
	static char sflag[2][24]={"*HIDE","*FADE"},yesno[2][24]={"*NO","*YES"};
/*
.....Initialize routine
*/
	strcpy(slcolor[0], "*DEFAULT");
	for (i=0; i<64;i++)
	{
		sprintf(slcolor[i+1], "*%s", uw_color_name[i]);
	}
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy (fname, "ncl_preview.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) goto done;
/*
.....Store Preview modals
*/
	ux_fputs0("#PREVIEW_MOTION#\n", fptr);

	sprintf(msg,"/UNUSED_GEO/ %s\n",sflag[UN_unused_geo_flag]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/COLOR/ %s\n",slcolor[UN_unused_geo_color+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/KEEP_STOCK/ %s\n",yesno[UN_keep_stock]);
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
**    I_FUNCTION     :  OnButton(fieldno, val, stat)
**       Routine to backspace the motion display.
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
	switch (*fieldno)
	{
/*
.....Rewind
*/
	case FREW:
		ncl_step_displayed(-2,0,UU_NULL);
		break;
/*
.....Step backwards
*/
	case FBCK:
		ncl_step_displayed(-1,0,UU_NULL);
		break;
/*
.....Step forwards
*/
	case FFWD:
		ncl_step_displayed(1,0,UU_NULL);
		break;
/*
.....Fast forward
*/
	case FFST:
		Splay = UU_TRUE;
		ncl_step_displayed(2,0,UU_NULL);
		Splay = UU_FALSE;
		break;
/*
.....Motion Playback
*/
	case FPLY:
		Splay = UU_TRUE;
		ncl_step_displayed(1,2,&Sspeed);
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
/*
.....we need fast forward to the end before close
*/
	Splay = UU_TRUE;
	ncl_step_displayed(2,0,UU_NULL);
	ud_updatews(UG_SUPPRESS);
	Splay = UU_FALSE;
	SfrmPlay = 0;
	ud_form_vis();
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     :  nclu_mot_check_colors(sflst,kfl,cklst1,kfl1,cklst2,kfl2)
**       Verifies that a hilited entity does not store a previous
**       hilite color if the entity is already stored in another list.
**    PARAMETERS
**       INPUT  :
**          sflst    Pointer to list of selected geometry structures.
**          kfl      0 = 'sflst' is single entity (UM_sgeo), 1 = list.
**          cklst1   Pointer to list of previously selected geometry structures.
**          kfl1     0 = 'cklst1' is single entity (UM_sgeo), 1 = list.
**          cklst2   Pointer to list of previously selected geometry structures.
**          kfl2     0 = 'cklst2' is single entity (UM_sgeo), 1 = list.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_mot_check_colors(sflst,kfl,cklst1,kfl1,cklst2,kfl2)
UU_LIST *sflst,*cklst1,*cklst2;
int kfl,kfl1,kfl2;
{
	int i,j,nc,nc1,nc2;
	UM_sgeo *geo,*ck1,*ck2;
/*
.....Setup geometry pointers
*/
	if (kfl == 1)
	{
		geo = (UM_sgeo *)UU_LIST_ARRAY(sflst);
		nc = UU_LIST_LENGTH(sflst);
	}
	else
	{
		geo = (UM_sgeo *)sflst;
		nc = 1;
	}
	if (kfl1 == 1)
	{
		ck1 = (UM_sgeo *)UU_LIST_ARRAY(cklst1);
		nc1 = UU_LIST_LENGTH(cklst1);
	}
	else
	{
		ck1 = (UM_sgeo *)cklst1;
		nc1 = 1;
	}
	if (kfl2 == 1)
	{
		ck2 = (UM_sgeo *)UU_LIST_ARRAY(cklst2);
		nc2 = UU_LIST_LENGTH(cklst2);
	}
	else
	{
		ck2 = (UM_sgeo *)cklst2;
		nc2 = 1;
	}
/*
.....Loop through list
*/
	for (i=0;i<nc;i++)
	{
		for (j=0;j<nc1;j++)
		{
			if (geo[i].key == ck1[j].key)
			{
				geo[i].color = ck1[j].color;
				break;
			}
		}
		for (j=0;j<nc2;j++)
		{
			if (geo[i].key == ck2[j].key)
			{
				geo[i].color = ck2[j].color;
				break;
			}
		}
	}
}
