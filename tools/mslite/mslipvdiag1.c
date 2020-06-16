/*********************************************************************
**    NAME         :  mslipvdiag1.c
**		include function for clash diag. Those function all different
**			with NCL, but some function will use the same name as NCL
**			because functions from NCL will use it. (for MSLITE)
**       CONTAINS:
**				msl_diag_form
**				ul_ipv_clash_form 
**    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       mslipvdiag1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:59
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "lcom.h"
#include "lipv.h"
#include "lipvmach.h"
#include "nclfc.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"

static int Sfrm2;
static UU_LOGICAL Sactive2;
static UU_LOGICAL Sstopfl[16];
static UD_LIST stat_list;

static UD_FSTAT OnClash(), OnClose2();
static void S_diag_form();
static void S_write_msg();

/*********************************************************************
**    E_FUNCTION     : msl_diag_form()
**       Processes the Clash detection form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void msl_diag_form()
{
	int i;
	UU_LOGICAL reject;
	if (LW_mach_naxes < 1)
	{
		ud_wrerr("No machine loaded yet");
		return;
	}
/*
.....Initialize the form variables
*/
	for (i=0;i<16;i++)
	{
		Sstopfl[i] = LW_clash_stop[i];
	}
/*
.....Display the form
*/
	S_diag_form(0,&reject);
/*
.....Store the clash parameters
*/
	if (!reject)
	{
		for (i=0;i<16;i++)
		{
			LW_clash_stop[i] = Sstopfl[i];
		}
/*
.....redefine clash
*/
		ul_ipv_define_clash(UU_NULL);
	}
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_clash_form(clash)
**       Processes the NCLIPV Measurement form.
**    PARAMETERS
**       INPUT  :
**          clash    = Defines the type of entities that clashed,
**                     Location of clash, and
**                     Source ISN that caused clash.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ul_ipv_clash_form(clash)
LW_clash_rec_struc clash[];
{
	int markval,i,status,ix;
	UU_LOGICAL stopfl,logfl,rejfl,disfrm,frmflag;
	char sbuf[2][20],dbuf[20],cbuf[80],buf[80];
	LW_mach_axis_style style[2];
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1};
	static UD_METHOD methods[] = {UU_NULL, OnClash};
	static char called[] = {6,6};
	static char display[] = {1,1};
	static int *ans[] = {(int *)&stat_list, UU_NULL};
/*
.....Initialize routine
*/
	stopfl = logfl = disfrm = frmflag = UU_FALSE;
	rejfl = UU_FALSE;
	Sactive2 = UU_FALSE;

	stat_list.num_item = 0;
	stat_list.item = UU_NULL;
	stat_list.answer = UU_NULL;
/*
.....Determine style of solids that clashed
*/
	for (ix=0;ix<LW_nclash;ix++)
	{
/*
.....Clash error
*/
		if (clash[ix].type == LW_DIAG_CLASH)
		{
			for (i=0;i<2;i++)
			{
				switch (clash[ix].ent[i])
				{
				case LW_CLASH_HEAD:
					style[i] = LW_MACH_HEAD;
					break;
				case LW_CLASH_AXIS:
					style[i] = LW_MACH_AXIS;
					break;
				default:
					continue;
				}
			}
/*
.....Determine if clash needs to be reported
*/
			ul_ipv_clash_axes(style[0],style[1],&stopfl,&logfl);
			if (!stopfl) continue;
			strcpy(dbuf,"CLASH");
/*
.....Get clash entities
*/
			for (i=0;i<2;i++)
			{
				switch (clash[ix].ent[i])
				{
				case LW_CLASH_HEAD:
				case LW_CLASH_AXIS:
					if (i == 0)
						strcpy(sbuf[i],clash[ix].errmsg1);
					else
						strcpy(sbuf[i],clash[ix].errmsg2);
					break;
				}
			}
/*
.....Initialize form
*/
			if (stopfl && !frmflag)
			{
				ul_ipv_init_list(&stat_list,100);
				for (i=0;i<16;i++)
				{
					Sstopfl[i] = LW_clash_stop[i];
				}
			}
			if (stopfl) frmflag = UU_TRUE;
/*
.....Output the clash record
*/
			S_write_msg(" ",stopfl);
			sprintf(buf,"%s   - %s and %s collide at ",dbuf,sbuf[0],sbuf[1]);
			S_write_msg(buf,stopfl);
			sprintf(buf,"          Location = %g,%g,%g",clash[ix].loc[0],
				clash[ix].loc[1],clash[ix].loc[2]);
			S_write_msg(buf,stopfl);
		}
	}
/*
.....Load the source lines
*/
	if (!frmflag) goto done;
/*
.....Command Reject
*/
	UD_MARK(markval,UU_FALSE);
	if (markval != 0) goto reject;
/*
.....Display the Form
*/
	status = ud_form1("mslclash.frm", ans, ans, methods, called,
		display, traverse);
	if (status == -1) goto reject;
/*
.....Save form results
*/
	for (i=0;i<16;i++)
	{
		LW_clash_stop[i] = Sstopfl[i];
	}
	goto done;
/*
.....Form was rejected
.....Stop playback
*/
reject:;
	rejfl = UU_TRUE;
	rsttrm();
/*
.....End of routine
*/
done:;
	if (frmflag)
	{
		ud_free_flist(&stat_list);
		UD_UNMARK(markval);
	}
	LW_nclash = 0;
	LW_clash_flag = UU_FALSE;
	if (rejfl)
		return 0;
	else
		return 1;
}

/*********************************************************************
**    E_FUNCTION     : S_diag_form(frmfl,reject)
**       Processes the Machine Clash Detection form.
**    PARAMETERS
**       INPUT  :
**          frmfl    = 0 = Normal form, 1 = Display only form.
**       OUTPUT :
**          reject   = UU_TRUE if form was rejected (only set when
**                     'frmfl' = 0)
**    RETURNS      : none
**    SIDE EFFECTS : The static variables 'Sstopfl' are
**                   used and set by this form.
**    WARNINGS     : none
*********************************************************************/
static void S_diag_form(frmfl,reject)
int frmfl;
UU_LOGICAL *reject;
{
	UD_METHOD methods[4];
	int *ans[3],i,markval,status;
	char called[4];
	char display[4];
	char traverse[4];
/*
.....Initialize routine
*/
	if (frmfl == 1 && Sactive2) return;
	if (!LW_mach_simul)
	{
		return;
	}
	for (i=0;i<3;i++)
	{
		traverse[i] = display[i] = 1;
		called[i] = 6; methods[i] = UU_NULL;
	}
	if (frmfl == 1) methods[3] = OnClose2;
/*
.....Store the default answers
*/
	ans[0] = (int *)&Sstopfl[10];
	ans[1] = (int *)&Sstopfl[11];
	ans[2] = (int *)&Sstopfl[14];
/*
.....Display the form
*/
	if (frmfl == 1)
	{
		Sfrm2 = ud_form_display1("mslmachdiag.frm", ans, ans, methods, called,
			display, traverse);
		if (Sfrm2 != -1) Sactive2 = UU_TRUE;
	}
/*
.....Standard form
*/
	else
	{
/*
.....Command Reject
*/
		UD_MARK(markval,UU_FALSE);
		if (markval != 0)
			*reject = UU_TRUE;
/*
........Display the form
*/
		else
		{
			*reject = UU_FALSE;
			status = ud_form("mslmachdiag.frm", ans, ans);
			if (status == -1) *reject = UU_TRUE;
		}
		UD_UNMARK(markval);
	}
}


/*********************************************************************
**    S_FUNCTION     :  OnClash(fieldno, val, stat)
**       Method called when the Clash Settings button is pushed.
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
static UD_FSTAT OnClash(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL reject;
/*
.....Display the Clash Settings form
*/
	S_diag_form(1,&reject);
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  OnClose2()
**       Method called when the Machine Clash Detection form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose2()
{
	Sactive2 = UU_FALSE;
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     : S_write_msg(buf,stopfl,logfl)
**       Writes a line to the status window and log file.
**    PARAMETERS
**       INPUT  :   buf     = Buffer to output.
**                  stopfl  = UU_TRUE = write to status window.
**                  logfl   = UU_TRUE = write to log file.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_write_msg(buf,stopfl,logfl)
char *buf;
UU_LOGICAL stopfl,logfl;
{
/*
.....Write to status window
*/
	if (stopfl)
		ul_ipv_put_list(&stat_list,buf);
/*
.....Write to log file
*/
	if (logfl)
		ul_ipv_write_log(buf);
}
