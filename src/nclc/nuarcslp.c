/**********************************************************
**    NAME:      nuarcslp.c
**
**    CONTAINS:
**       nclu_arcslp_fillet()
**       nclu_arcslp_fillet_form()
**       nclu_arcslp_set_fillet()
**       nclu_arcslp_command()
**       nclu_revers_clfile()
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nuarcslp.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       07/13/15 , 09:22:40
***********************************************************/

#include <math.h>
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "mfort.h"
#include "modef.h"
#include "nkeywd.h"
#include "nclcmd.h"
#include "nclfc.h"
#include "nclfile.h"
#include "nclinp.h"
#include "nconst.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include "udforms.h"
#include "udfconst.h"
#include "udfdata.h"

enum
{
	FENA, FRAD, FTOL, FCMB,
	FWRN, FFIX, FDEV,
	FCTL, FFED, FMXF, FCND, FDIA, FVIDEO
};

static UD_FSTAT OnFilTog(),S_init_form(),OnClose();
static void S_define_fillet();

extern UD_METHOD UD_initfrm_intry;

static int Sfrm1=0;
static UU_LOGICAL Sactive1 = UU_FALSE;
static int Sftog[6];
static char Sfvar[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static NCLX_mot_fillet Sfillet;
static char *Soption[3]={"Radius","Options","Feed Rates"};

#define NEQTOL(r,s) (fabs(r-s)>=.0001)

/*********************************************************************
**    E_FUNCTION     : nclu_arcslp_fillet()
**       Controlling routine for the Corner Rounding form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_arcslp_fillet()
{
	int status;
	char cfill[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	NCLX_mot_fillet fillet;
	NCL_cmdbuf cmdbuf;
/*
.....Get the current ARCSLP settings
*/
	NclxMotGetFillet(&fillet);
/*
.....Display the form
*/
	status = nclu_arcslp_fillet_form(&fillet,cfill,UU_TRUE,UU_FALSE);
/*
.....Output the ARCSLP/FILLET command
*/
   if (status == UU_SUCCESS)
   {
		nclu_arcslp_command(&cmdbuf,&fillet,cfill,UU_TRUE);
      ncl_set_cmdmode(UU_TRUE);
      ncl_call(&cmdbuf);
   }
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
**    E_FUNCTION     : nclu_arcslp_fillet_form(fillet,caxis,modal)
**       Controlling routine for the Corner Rounding form.
**    PARAMETERS
**       INPUT  :
**          fillet    - Active ARCSLP/FILLET parameters.
**          modal     - UU_TRUE = Form is modal (standard),
**                      UU_FALSE = Form is display type subform.
**          reentry   - UU_TRUE = Form is being reentered for the 2nd
**                      time.  Only used for nonmodal form type.
**       OUTPUT :
**          fillet    - Updated ARCSLP/FILLET parameters.
**          cfill     - Textual form responses.
**    RETURNS      :
**          UU_SUCCESS if the form was accepted.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_arcslp_fillet_form(fillet,cfill,modal,reentry)
NCLX_mot_fillet *fillet;
char cfill[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
UU_LOGICAL modal,reentry;
{
	int status,irtn,fieldno,stat;
	UD_DDATA val;
	UD_METHOD save_entry;

	static char traverse[] = {
		1,0,0,0,
		0,0,0,
		0,0,0,0,0,1};
	static char display[] = {
		1,1,1,1,
		1,1,1,
		1,1,1,1,1,1,1};
	static char called[] = {
		6,6,6,6,
		6,6,6,
		6,6,6,6,6,6};
	static UD_METHOD methods[] = {
		OnFilTog,UU_NULL,UU_NULL,UU_NULL,
		OnFilTog,OnFilTog,UU_NULL,
		OnFilTog,UU_NULL,UU_NULL,UU_NULL,UU_NULL,OnVideo,OnClose};
	static int *ans[]={
		&Sftog[0],(int *)Sfvar[0],(int *)Sfvar[1],&Sftog[1],
		&Sftog[2],&Sftog[3],(int *)Sfvar[2],
		&Sftog[4],(int *)Sfvar[3],(int *)Sfvar[4],&Sftog[5],(int *)Sfvar[5], UU_NULL};
/*
.....Initialize routine
*/
	Sfillet = *fillet;
	save_entry = UD_initfrm_intry;
/*
.....Set up form fields
*/
	S_define_fillet(fillet,Sftog,Sfvar,traverse,reentry);
/*
.....Get the form input
*/
	if (modal)
	{
		Sfrm1 = 0;
		UD_initfrm_intry = S_init_form;
		status = ud_form1("arcslp.frm",ans,ans,methods,called,display,traverse);
		UD_initfrm_intry = save_entry;
	}
	else if (!Sactive1)
	{
		Sfrm1 = ud_form_display1("arcslp.frm",ans,ans,methods,called,display,
			traverse);
		if (Sfrm1 != 0)
		{
			Sactive1 = UU_TRUE;
			fieldno = stat = 0;
			S_init_form(&fieldno,&val,stat);
		}
		goto done;
	}
	if (status == -1) return(status);
/*
.....Store the ARCSLP parameters
*/
	if (!Sactive1) irtn = nclu_arcslp_set_fillet(fillet,cfill);
/*
.....End of routine
*/
done:;
	return(UU_SUCCESS);
}

/***********************************************************************
**     E_FUNCTION   :   nclu_arcslp_set_fillet(fillet,cfill)
**        Defines the Fillet settings from the form field parameters.
**     PARAMETERS
**        INPUT  : none
**        OUTPUT :
**           fillet   = Current Fillet settings.
**           cfill    = Textual form responses.
**        RETURNS      : none
**        SIDE EFFECTS : none
**        WARININGS    : none
************************************************************************/
int nclu_arcslp_set_fillet(fillet,cfill)
NCLX_mot_fillet *fillet;
char cfill[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int i;

	fillet->combine = Sftog[1];
	fillet->warn = Sftog[2];
	fillet->same = Sftog[3];
	fillet->fedctl = Sftog[4];
	fillet->direction = Sftog[5];
	if (Sftog[0] == 0) strcpy(cfill[0],"0.");
	else strcpy(cfill[0],Sfvar[0]);
	for (i=1;i<6;i++) strcpy(cfill[i],Sfvar[i]);
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_arcslp_command(cmdbuf,arcslp,cfill,kfl,flag)
**       Builds and outputs the ARCSLP command.
**    PARAMETERS
**       INPUT  :
**          fedrat   = Active Arcslp parameters.
**          cfill    = Textual form responses.
**          kfl      = UU_TRUE = Command is generated from the interface.
**                     Check if changed from previous settings and use
**                     'cfill' variables in command output.
**          flag     = UU_TRUE = output command to source file.
**                     UU_FALSE = Preview command only.
**       OUTPUT :
**          cmdbuf   = Command buffer to containt the ARCSLP command(s).
**    RETURNS      : UU_TRUE if a command was output.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_arcslp_command(cmdbuf,arcslp,cfill,kfl,flag)
NCL_cmdbuf *cmdbuf;
NCLX_mot_fillet *arcslp;
char cfill[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
UU_LOGICAL kfl,flag;
{
	int i,nc,inum;
   UU_LOGICAL changed,tfl;
	UU_REAL rval;
	char tbuf[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1],*tptr[6];
/*
.....Determine if settings have changed
*/
	if (kfl) changed = S_arcslp_changed(arcslp,cfill);
	else changed = UU_TRUE;
	if (!changed) goto done;
/*
.....Initialize command
*/
	ncl_init_cmdbuf(cmdbuf);
	if (!flag) ncl_add_token(cmdbuf, "*", NCL_nocomma);
	ncl_add_token(cmdbuf,NCL_arcslp,NCL_nocomma);
	ncl_add_token(cmdbuf,NCL_fillet,NCL_comma);
/*
.....ARCSLP/FILLET,0
*/
	if (kfl)
	{
		tfl = UU_FALSE;
		nc = strlen(cfill[0]); ul_strip_blanks(cfill[0],&nc);
		if (ul_to_reals(&rval,&inum,1,cfill[0]) == UU_SUCCESS && rval == 0.)
			tfl = UU_TRUE;
	}
	else
		tfl = !(NEQTOL(arcslp->rad,0.));
	if (tfl)
	{
		ncl_add_token(cmdbuf,"0",NCL_nocomma);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(cmdbuf);
		ncl_call(cmdbuf);
		goto done;
	}
/*
.....Initialize routine
*/
	if (kfl)
	{
		for (i=0;i<6;i++) tptr[i] = cfill[i];
	}
	else
	{
		for (i=0;i<6;i++) tptr[i] = tbuf[i];
		ncl_sprintf(tbuf[0],&arcslp->rad,1);
		ncl_sprintf(tbuf[1],&arcslp->tol,1);
		ncl_sprintf(tbuf[2],&arcslp->maxang,1);
		ncl_sprintf(tbuf[3],&arcslp->fedrt,1);
		ncl_sprintf(tbuf[4],&arcslp->fmax,1);
		ncl_sprintf(tbuf[5],&arcslp->cdia,1);
	}

	ncl_add_token(cmdbuf,tptr[0],NCL_comma);
	ncl_add_token(cmdbuf,tptr[1],NCL_comma);
	if (arcslp->warn == 1)
		ncl_add_token(cmdbuf,NCL_warn,NCL_comma);
	if (arcslp->combine == 1)
		ncl_add_token(cmdbuf,NCL_combin,NCL_comma);
	if (arcslp->same == 1)
	{
		ncl_add_token(cmdbuf,NCL_lock,NCL_comma);
		ncl_add_token(cmdbuf,tptr[2],NCL_comma);
	}

	if (arcslp->fedctl == 1)
	{
		nc = strlen(tptr[3]); ul_strip_blanks(tptr[3],&nc);
		if (ul_to_reals(&rval,&inum,1,tptr[3]) != UU_SUCCESS || rval != 0.)
		{
			ncl_add_token(cmdbuf,NCL_feedrat,NCL_comma);
			ncl_add_token(cmdbuf,tptr[3],NCL_comma);
			ncl_add_token(cmdbuf,tptr[4],NCL_comma);
			if (arcslp->direction == NCLX_TLLFT)
				ncl_add_token(cmdbuf,NCL_left,NCL_comma);
			else
				ncl_add_token(cmdbuf,NCL_right,NCL_comma);
			ncl_add_token(cmdbuf,tptr[5],NCL_comma);
		}
	}
	ncl_add_cmdbuf(cmdbuf);
done:;
	return(changed);
}

/**********************************************************
**
**     FUNCTION:   nclu_revers_clfile(option)
**
**     PURPOSE:    Builds the command REVERS/ON-OFF
**
**                 option = 0 - Outputs the REVERS/ON command.
**
**                 option = 1 - Outputs the REVERS/OFF command.
** 
***********************************************************/
void nclu_revers_clfile(option)
int option;
{
	NCL_cmdbuf cmdbuf;
/*
.....Build command
*/
	ncl_init_cmdbuf(&cmdbuf);
	ncl_add_token(&cmdbuf,NCL_revers,NCL_nocomma);
	if (option == 0)
	{
		ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
		ud_prmerr("Start of clfile reversal marked.");
	}
	else
	{
		ncl_add_token(&cmdbuf,NCL_off,NCL_comma);
		ud_prmerr("End of clfile reversal marked.");
	}
/*
.....Output the command
*/
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....End of routine
*/
	return;
}

/***********************************************************************
**     I_FUNCTION   :   OnFilTog(fieldno, val, stat)
**        Method called when a toggle or checkbox field is changed.
**     PARAMETERS
**        INPUT  :
**           fieldno  Field number being changed.
**           val      Current field value.
**           stat     Field status.
**        OUTPUT :
**           none
**        RETURNS      : none
**        SIDE EFFECTS : none
**        WARININGS    : none
************************************************************************/
static UD_FSTAT OnFilTog(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	UU_LOGICAL flg;

	switch (*fieldno)
	{
/*
.....Enable Corner Rounding
*/
	case FENA:
		if (Sftog[0] == 0)
			for (i=FRAD;i<=FDIA;i++) ud_setfrm_traverse_mask(Sfrm1,i,UU_FALSE);
		else
		{
			for (i=FRAD;i<=FFIX;i++) ud_setfrm_traverse_mask(Sfrm1,i,UU_TRUE);
			flg = Sftog[2] == 1 && Sftog[3] == 1;
			ud_setfrm_traverse_mask(Sfrm1,FDEV,flg);
			ud_setfrm_traverse_mask(Sfrm1,FCTL,UU_TRUE);
			for (i=FFED;i<=FDIA;i++) ud_setfrm_traverse_mask(Sfrm1,i,Sftog[4]);
		}
		S_init_form(fieldno,val,stat);
		break;
/*
.....Output warnings
.....Fixed tool axis
*/
	case FWRN:
	case FFIX:
		flg = Sftog[2] == 1 && Sftog[3] == 1;
		ud_setfrm_traverse_mask(Sfrm1,FDEV,flg);
		break;
/*
.....Feed rate control
*/
	case FCTL:
		for (i=FFED;i<=FDIA;i++) ud_setfrm_traverse_mask(Sfrm1,i,Sftog[4]);
		break;
	}
	return(UD_FLDOK);
}

/***********************************************************************
**     I_FUNCTION   :   OnClose()
**        Method called Advanced Settings form is closed.
**     PARAMETERS
**        INPUT  :
**           none
**        OUTPUT :
**           none
**        RETURNS      : none
**        SIDE EFFECTS : none
**        WARININGS    : none
************************************************************************/
static UD_FSTAT OnClose()
{
	Sactive1 = UU_FALSE;
	Sfrm1 = 0;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_init_form(fieldno,val,stat)
**       Defines the active section when the form is displayed.
**    PARAMETERS
**       INPUT  :
**          fieldno = Not used.
**          val     = Not used.
**          stat    = Not used.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT S_init_form(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_form_section_enable(Sfrm1,Soption[1],Sftog[0]);
	ud_form_section_enable(Sfrm1,Soption[2],Sftog[0]);
	return(UD_FLDOK);
}

/***********************************************************************
**     I_FUNCTION   :   S_define_fillet(fillet,ftog,fvar,traverse)
**        Defines the field parameters for the ARCSLP/FILLET fields
**        based on the active Fillet settings.
**     PARAMETERS
**        INPUT  :
**           fillet   = Current Fillet settings.
**           reentry  = UU_TRUE - Don't initialize fillet settings.
**        OUTPUT :
**           ftog[0]  = Enable Corner Rounding.
**           ftog[1]  = Combine motion.
**           ftog[2]  = Output warnings.
**           ftog[3]  = Fixed tool axis.
**           ftog[4]  = Feed rate control.
**           ftog[5]  = Tool condition.
**           fvar[0]  = Fillet radius.
**           fvar[1]  = Fillet tolerance.
**           fvar[2]  = Maximum deviation.
**           fvar[3]  = Feed rate.
**           fvar[4]  = Maximum feed rate.
**           fvar[5]  = Cutter diameter.
**           traverse = Field traversal settings.
**        RETURNS      : none
**        SIDE EFFECTS : none
**        WARININGS    : none
************************************************************************/
static void S_define_fillet(fillet,ftog,fvar,traverse,reentry)
NCLX_mot_fillet *fillet;
int ftog[];
char fvar[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char *traverse;
UU_LOGICAL reentry;
{
	int i;
/*
.....Define ARCSLP/FILLET settings
*/
	if (!reentry)
	{
		ftog[0] = fillet->rad != 0. ? 1 : 0;
		ftog[1] = fillet->combine;
		ftog[2] = fillet->warn;
		ftog[3] = fillet->same;
		ftog[4] = fillet->fedctl;
		ftog[5] = fillet->direction;
		ncl_sprintf(fvar[0],&fillet->rad,1);
		ncl_sprintf(fvar[1],&fillet->tol,1);
		ncl_sprintf(fvar[2],&fillet->maxang,1);
		ncl_sprintf(fvar[3],&fillet->fedrt,1);
		ncl_sprintf(fvar[4],&fillet->fmax,1);
		ncl_sprintf(fvar[5],&fillet->cdia,1);
	}
/*
.....Set ARCSLP/FILLET fields
*/
	if (ftog[0] == 0)
		for (i=FRAD;i<=FDIA;i++) traverse[i] = 0;
	else
	{
		traverse[FRAD] = traverse[FTOL] = traverse[FCMB] = 1;
		traverse[FWRN] = traverse[FFIX] = 1;
		traverse[FDEV] = ftog[2] == 1 && ftog[3] == 1;
		traverse[FCTL] = 1;
		for (i=FFED;i<=FDIA;i++) traverse[i] = ftog[4];
	}
}

/*********************************************************************
**    I_FUNCTION     : S_arcslp_changed(fillet,cfill)
**       Determines if the ARCSLP settings have changed and a command
**       should be output.
**    PARAMETERS
**       INPUT  :
**          fillet   = Active Arcslp parameters.
**          cfill    = Textual form responses.
**       OUTPUT : none
**    RETURNS      : UU_TRUE if a command should be output.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_arcslp_changed(fillet,cfill)
NCLX_mot_fillet *fillet;
char cfill[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int inum;
   UU_LOGICAL chg;
	UU_REAL rval;
/*
.....Determine if ARCSLP settings have changed
*/
	chg = UU_FALSE;
	if (fillet->same != Sfillet.same ||
		fillet->combine != Sfillet.combine ||
		fillet->fedctl != Sfillet.fedctl ||
		fillet->warn != Sfillet.warn) chg = UU_TRUE;
	else
	{
		if (ul_to_reals(&rval,&inum,1,cfill[0]) == UU_SUCCESS)
			if (NEQTOL(rval,Sfillet.rad)) chg = UU_TRUE;
		if (ul_to_reals(&rval,&inum,1,cfill[1]) == UU_SUCCESS)
			if (NEQTOL(rval,Sfillet.tol)) chg = UU_TRUE;
		if (ul_to_reals(&rval,&inum,1,cfill[2]) == UU_SUCCESS)
			if (NEQTOL(rval,Sfillet.maxang)) chg = UU_TRUE;
		if (!chg && fillet->fedctl)
		{
			if (fillet->direction != Sfillet.direction) chg = UU_TRUE;
			if (ul_to_reals(&rval,&inum,1,cfill[3]) == UU_SUCCESS)
				if (NEQTOL(rval,Sfillet.fedrt)) chg = UU_TRUE;
			if (ul_to_reals(&rval,&inum,1,cfill[4]) == UU_SUCCESS)
				if (NEQTOL(rval,Sfillet.fmax)) chg = UU_TRUE;
			if (ul_to_reals(&rval,&inum,1,cfill[5]) == UU_SUCCESS)
				if (NEQTOL(rval,Sfillet.cdia)) chg = UU_TRUE;
		}
	}
	return(chg);
}
