/*********************************************************************
**    NAME         :  lipvplay.c
**       CONTAINS:
**			ul_ipv_playback()
**			ul_ipv_monitor_form()
**       ul_ipv_monitor_close()
**       ul_ipv_monitor_active()
**			ul_ipv_update_monitor()
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvplay.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:16
*********************************************************************/

#include "usysdef.h"
#include "xenv1.h"
#include "lumb.h"
#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"
#include "mdcpln.h"
#include "mfort.h"
#include "nclfc.h"
#include "nclfile.h"
#include "nclmplay.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "view.h"
#include <ctype.h>
#include "lcom.h"

extern char *frm_ptr;

void ul_ipv_update_monitor();
static UD_FSTAT OnCloseMon();
static int S_save_modfile();

static int SfrmMon=0;
static int Smon_nfld,Smon_prog,Smtype=-1;
static int Smon_fld[N_IPVMON_FLD+16],Sactfld[N_IPVMON_FLD];
static int Saxis[10],Snaxis,Shead[12],Snhead;
static char Smon_text[N_IPVMON_FLD+16][20];

static struct
{
	UU_REAL tend[3];
	UU_REAL tax[3];
	UU_REAL axis[13];
	UU_REAL cutr[3];
	UU_REAL tlen;
	UU_REAL sp_val;
	UU_REAL fedrat;
	UU_REAL movtim;
	UU_REAL mchtim;
	int isn;
	int type;
	int tlno;
	int coolnt;
	int sp_dir;
	int cc_mode,cc_dir;
	int fr_mode;
	int progress;
} Sblock;

static UD_FSTAT OnReset();

/*********************************************************************
**    E_FUNCTION     : ul_ipv_playback()
**       Processes the NCLIPV PLAYBACK form.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ul_ipv_playback()
{
	static int tstrt,tmode,tspd,tstep,tdyn,tcyc,tanl,trap,tshow;
	int status;
	UU_LOGICAL idid;
/*
.....Set up form fields
.......Added tshow for new option to show source during IPV
.......playback - ASF 12/18/13.
*/
   static char traverse[]     = {1,1,1,1, 1,1,1,1, 1, 1};
   static UD_METHOD methods[] = {UU_NULL,UU_NULL,UU_NULL,UU_NULL,
                                 UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
											OnReset};
   static char called[]       = {6,6,6,6, 6,6,6,6, 6, 6};
	static int *ans[] = {&tstrt, &tmode, &tspd, &tstep, &tdyn, &tcyc, &trap,
		&tanl, &tshow, UU_NULL};
	int ifl,markval;
/*
.....Playback is already active
*/
	if (UN_playback_active) return UU_SUCCESS;
	idid = UU_FALSE;
/*
.....Make sure NCLIPV is active
*/
	if (!LW_active)
	{
		ul_verify(1);
		if (!LW_active)
		{
			ud_wrerr("NCLIPV is not active.");
			return(UU_SUCCESS);
		}
	}
/*
.....NCLIPV Machine Simulation active
.....Make sure that a simulation file is used
*/
	if (LW_mach_simul && LW_mach_desc.type == -1)
	{
		ud_wrerr(
			"You must use a simulation file when Machine Simulation is active.");
		return(UU_SUCCESS);
	}
/*
.....Make sure we have a cutter list
*/
	if (LW_ntool == 0 || (LW_ntool == 1 && LW_ntool_sess > 0) ||
		(/*UN_clfile_current == UU_NULL &&*/
		UN_last_clpt[1] != UN_clpt[UN_clfile]))
			ul_ipv_append_tools(&LW_tool_list,&LW_ntool);
/*
.....Trap reject op
.....For Analyzation Playback
*/
	ifl = 0;
	UD_MARK(markval,UU_TRUE);
	if (markval != 0) goto done;
/*
.....Set up the field entries
*/
	if (LW_mach_type == LW_LATHE) LW_play_modal[14] = 3;
	else if ((LW_mach_type == LW_MILL || LW_mach_type == LW_STRINGER) &&
		LW_play_modal[14] == 3) LW_play_modal[14] = 1;
	tstrt = LW_play_modal[0]; tspd  = LW_play_modal[1]; tmode = LW_play_modal[2];
	tstep = LW_play_modal[3]; tdyn  = LW_play_modal[5] - 1;
	tcyc  = LW_play_modal[14]; tanl = LW_play_modal[15];
	tshow = LW_play_modal[16]; trap = LW_play_modal[17];
/*
.....Enable correct fields
.....based on DISPLAY field value
*/
	traverse[1] = traverse[2] = traverse[3] = traverse[4] = traverse[5] = 1;
	if (LW_mach_simul)
	{
		traverse[4] = 0;
		tdyn = 0;
	}
	if (LW_mach_desc.type != -1) traverse[5] = 0;
	if (LW_mach_mode == LW_RAPIDCUT)
	{
		traverse[1] = traverse[2] = traverse[3] = traverse[4] = 0;
		tmode = 0; tdyn = 0;
	}
form:;
/*
.....Get the Form input
*/
	UN_playback_active = UU_FALSE;
	status = ud_form1("ipvplayback.frm", ans, ans, methods, called, UU_NULL,
		traverse);
	if (status==-1) goto done;
/*
.....Save the form entries
*/
	LW_play_modal[0] = tstrt; LW_play_modal[1] = tspd; LW_play_modal[2] = tmode;
	LW_play_modal[3] = tstep; LW_play_modal[5] = tdyn + 1;
	LW_play_modal[14] = tcyc; LW_play_modal[15] = tanl;
	LW_play_modal[16] = tshow; LW_play_modal[17] = trap;
/*
.....Initialize the playback parameter structure
*/
	if (tstrt == 0)
	{
		UN_clfile_current = UN_clfile_start;
		UN_clfile_curpt = 0;
		Sblock.mchtim = 0.;
/*
........Load the current clfile parameters
........when starting from a "*Command"
*/
		if ((UN_playfile_start == 2 && UN_playfile_src != 2) ||
			(LW_active && UN_playfile_start != 0))
				ncl_fill_sequnc(UN_clfile_start);
	}
	if (LW_play_modal[0] == 1 && UN_clfile_current == UU_NULL)
		UN_clfile_current = UN_last_clpt[1];
	if (UN_clfile_current == UU_NULL && UN_clfile_curpt == 0)
		ncl_reset_mplayback();
/*
.....Setup the tool axis
*/
	if (ifl == 0)
	{
		UN_playtax_save[0] = UN_playparm.spt[3];
		UN_playtax_save[1] = UN_playparm.spt[4];
		UN_playtax_save[2] = UN_playparm.spt[5];
		ifl = 1;
	}
/*
.....Write NCLIPV playback header
*/
	idid = UU_TRUE;
	LW_dntcut = UU_FALSE;
	ul_ipv_log_session();
/*
.....Count the number of clfile records
.....in playback range if Progress bar
.....is enabled in Monitor Panel
*/
	if (LW_monitor_field[IPVMON_PROGRESS])
		LW_progress_total = ncl_count_clrec();
/*
.....Move the Undo Stack to the end
*/
	ul_ipv_mot_stack_step(2,"");
/*
.....Start the playback
*/
	ncl_motion_playback(LW_play_modal,0,UU_NULL,&LW_tool_list,&LW_ntool);
/*
.....User exited NCL in the middle of step mode
*/
	if (ncl_is_exiting()) goto done;
	UN_playtax_save[0] = UN_playparm.spt[3];
	UN_playtax_save[1] = UN_playparm.spt[4];
	UN_playtax_save[2] = UN_playparm.spt[5];
	UN_last_clpt[1] = UN_clpt[UN_clfile];
/*
.....Write NCLIPV playback trailer
*/
	ul_ipv_log_sessend();
/*
.....Update the progress bar
*/
	if (LW_monitor) ul_ipv_update_monitor(UU_NULL,UU_NULL,UU_NULL,UU_TRUE);
	goto form;
/*
.....End of routine
*/
done:;
	UN_playback_active = UU_FALSE;
	if (idid) S_save_modfile();
	UD_UNMARK(markval);
	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION     : S_save_modfile
**       Save the NCLIPV playback properties into modals file.
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
	static char begcur[2][10] = {"*BEGIN","*CURRENT"};
	static char cmode[3][10] = {"*GO","*STEP","*TOOL"};
	static char cdyn[2][10] = {"*CUTTER","*PART"};
	static char cyc[4][10] = {"*OFF","*SIMPLE","*DETAIL","*LATHE"};
	static char analy[3][10] = {"*OFF","*FEED","*INTERP"};
	static char rapid[5][10] = {"*OFF","*XAXIS","*YAXIS","*ZAXIS","*TLAXIS"};
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy (fname, "nclipv_playback.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) goto done;
/*
.....Store playback modals
*/
	ux_fputs0("#PLAYBACK#\n", fptr);
	sprintf(msg,"/START/ %s\n",begcur[LW_play_modal[0]]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SPEED/ %d\n",LW_play_modal[1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/MODE/ %s\n",cmode[LW_play_modal[2]]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/STEPS/ %d\n",LW_play_modal[3]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/DYNAMIC/ %s\n",cdyn[LW_play_modal[5]-1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/CYCLE/ %s\n",cyc[LW_play_modal[14]]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/ANALYZE/ %s\n",analy[LW_play_modal[15]]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/RAPID/ %s\n",rapid[LW_play_modal[17]]);
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
**    E_FUNCTION :  ul_ipv_monitor_form()
**      Create and display a dynamic form for the NCLIPV Monitor panel.
**    PARAMETERS
**       INPUT:
**         none
**       OUTPUT:
**         none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_monitor_form()
{
	int nfld,i,j,k,n,status,irow,inc,ipt,nc,fr_start[4],fr_nline[4],ifrm,y;
	int sfld,nfrm,ifrmend,ist,ien,kpt;
	UU_LOGICAL iaxs,ihed;
	char tmp[80],ctmp[10][21],caxis[10][21],sbuf[21],haxis[12][21];
	static char traverse[N_IPVMON_FLD+20];
	static UD_METHOD methods[N_IPVMON_FLD+21];
	static int *ans[N_IPVMON_FLD+20];
	static char called[N_IPVMON_FLD+20];
	static char *clab[]={"ISN","Mode","Mach","Tool End","Tool Axis","Linear",
		"Rotary","Head 2","Head 3","Head 4","Tool","Dia","Crad","Hgt","Comp",
		"Feed","Time","Rpm","Cool","Total","Progress"};
	static char *straxis[]={"X","Y","Z","C","A","X2","Y2","Z2","A2","B2","C2",
		"U","V","W","I","J","K","Q"};
/*	static char *fr_title[] = {"Position","Tool","Speeds"};*/
/*
.....Form is already open
*/
	frm_ptr = UU_NULL;
	if (SfrmMon != 0) goto done;
/*
.....Calculate the number of displayed fields
*/
	nfld = 0;
	Snaxis = 0;
	Snhead = 0;
	nfrm = 0;
	for (i=0;i<4;i++)
	{
		fr_start[i] = -1;
		fr_nline[i] = 0;
	}
	for (i=0;i<N_IPVMON_FLD-1;i++)
	{
		if (LW_monitor_field[i] == 1)
		{
/*
........Machine linear axes
*/
			sfld = nfld;
			if (i == IPVMON_LINAXS)
			{
				ipt = kpt = 0;
				for (j=0;j<3;j++)
				{
					inc = 2;
					if (LW_mach_type == LW_STRINGER) inc = 1;
					for (k=0;k<LW_mach_desc.numlin[j];k++)
					{
						Saxis[Snaxis] = ipt + k;
						strcpy(caxis[Snaxis],LW_mach_desc.linaxs[kpt+k]);
						nfld++;
						Snaxis++;
					}
					ipt = ipt + inc;
					kpt = kpt + 2;
				}
			}
/*
........Machine rotary axes
*/
			else if (i == IPVMON_ROTAXS)
			{
				inc = 6;
				if (LW_mach_type == LW_STRINGER) inc = 3;
				for (j=0;j<LW_mach_desc.numrot;j++)
				{
					strcpy(sbuf,LW_mach_desc.rotaxs[j]);
					nc = strlen(sbuf);
					ul_strip_blanks(sbuf,&nc);
					if (sbuf[0] != '\0')
					{
						Saxis[Snaxis] = inc + j;
						strcpy(caxis[Snaxis],sbuf);
						nfld++;
						Snaxis++;
					}
				}
			}
/*
........Stringer Head axes
*/
			else if (i == IPVMON_HEAD2 || i == IPVMON_HEAD3 || i == IPVMON_HEAD4)
			{
				if (LW_mach_type == LW_STRINGER)
				{
					if (i == IPVMON_HEAD2) {ist = 5; ien = 11;}
					else if (i == IPVMON_HEAD3) {ist = 11; ien = 14;}
					else {ist = 14; ien = 17;}
					for (j=ist;j<ien;j++)
					{
						if (LW_mach_axes[j] != -1)
						{
							Shead[Snhead] = j;
							strcpy(haxis[Snhead],straxis[j]);
							nfld++;
							Snhead++;
						}
					}
				}
			}
/*
........Tool position
*/
			else if (i == IPVMON_TEND || i == IPVMON_TLAXIS) nfld += 3;
/*
........All other fields
*/
			else nfld++;
/*
........Calculate frame start points and sizes
*/
			ifrm = -1;
			if (i >= IPVMON_TEND && i <= IPVMON_ROTAXS) ifrm = 0;
			if (i >= IPVMON_HEAD2 && i <= IPVMON_HEAD4) ifrm = 1;
			else if (i >= IPVMON_LOADTL && i <= IPVMON_CUTCOM) ifrm = 2;
			else if (i >= IPVMON_FEDRAT && i <= IPVMON_SPINDL) ifrm = 3;
			if (ifrm != -1)
			{
				if (fr_start[ifrm] == -1)
				{
					fr_start[ifrm] = sfld;
					nfrm++;
				}
				fr_nline[ifrm] = fr_nline[ifrm] + (nfld - sfld);
			}
		}
	}
	if (nfld == 0) goto done;
/*
.....Allocate memory for form
*/
	frm_ptr = (char *)uu_lsnew();
	if (frm_ptr == 0)
	{
		ud_wrerr("Could not allocate memory for the form.");
		status = UU_FAILURE;
		goto done;
	}
	frm_ptr = (char *)uu_lsinsrt(frm_ptr,13000);
	frm_ptr[0] = '\0';
/*
.....Create the form header
*/
	sprintf(frm_ptr,"#HEADER#\n");
	strcat(frm_ptr,"/TITLE/ NCLIPV Monitor Panel\n");
	if (LW_monitor_field[N_IPVMON_FLD-1] == 0)
		strcat(frm_ptr,"/POSITION/ 50,50\n");
	else if (LW_monitor_field[N_IPVMON_FLD-1] == 1)
		strcat(frm_ptr,"/POSITION/ Left\n");
	else
		strcat(frm_ptr,"/POSITION/ Right\n");
	strcat(frm_ptr,"/DOCKABLE/ LEFT,RIGHT\n");
	sprintf(tmp,"/SIZE/ 95,%d\n",nfld*14+10+nfrm*16);
	strcat(frm_ptr,tmp);
/*
.....Create the fields
*/
	irow = 7;
	Smon_nfld = 0;
	iaxs = ihed = UU_FALSE;
	ifrm = 0;
	ifrmend = 10000;
	for (i=0;i<N_IPVMON_FLD-1;i++)
	{
		if (LW_monitor_field[i] == 1)
		{
			Sactfld[i] = Smon_nfld;
			if (irow >= ifrmend)
			{
/*				irow += 3;*/
				irow += 5;
				ifrmend = 10000;
			}
/*
........See if it's time to output a Frame
*/
			while (fr_start[ifrm] == -1 && ifrm < 4) ifrm++;
			if (Smon_nfld == fr_start[ifrm])
			{
				strcat(frm_ptr,"#FRAME#\n");
/*				sprintf(tmp,"/TITLE/ %s\n",fr_title[ifrm]);*/
				sprintf(tmp,"/TITLE/\n");
				strcat(frm_ptr,tmp);
				sprintf(tmp,"/POSITION/ 0,%d\n",irow);
				strcat(frm_ptr,tmp);
				y = fr_nline[ifrm] * 14 + 14;
				sprintf(tmp,"/SIZE/ 90,%d\n",y);
				strcat(frm_ptr,tmp);
				ifrmend = irow + y - 7;
/*				irow += 12;*/
				irow += 10;
				ifrm++;
			}
/*
........Tool End field
*/
			if (i == IPVMON_TEND)
			{
				n = 3;
				strcpy(ctmp[0],"X"); strcpy(ctmp[1],"Y"); strcpy(ctmp[2],"Z");
			}
/*
........Tool Axis field
*/
			else if (i == IPVMON_TLAXIS)
			{
				n = 3;
				strcpy(ctmp[0],"I"); strcpy(ctmp[1],"J"); strcpy(ctmp[2],"K");
			}
/*
........Machine axes field
*/
			else if (i == IPVMON_LINAXS || i == IPVMON_ROTAXS)
			{
				if (!iaxs)
				{
					if (i == IPVMON_ROTAXS && LW_monitor_field[IPVMON_LINAXS] == 0)
						Sactfld[IPVMON_LINAXS] = Sactfld[i];
					iaxs = UU_TRUE;
					n = Snaxis;
					for (j=0;j<n;j++) strcpy(ctmp[j],caxis[j]);
				}
				else n = 0;
			}
/*
........Stringer Head axes fields
*/
			else if (i >= IPVMON_HEAD2 && i <= IPVMON_HEAD4)
			{
				if (!ihed)
				{
					if (i > IPVMON_HEAD2 && LW_monitor_field[IPVMON_HEAD2] == 0)
						Sactfld[IPVMON_HEAD2] = Sactfld[i];
					ihed = UU_TRUE;
					n = Snhead;
					for (j=0;j<n;j++) strcpy(ctmp[j],haxis[j]);
				}
				else n = 0;
			}
/*
........All other fields
*/
			else
			{
				n = 1;
				strcpy(ctmp[0],clab[i]);
			}
			for (j=0;j<n;j++)
			{
/*
........Progress field
*/
				if (i == IPVMON_PROGRESS)
				{
					strcat(frm_ptr,"#PROGRESS#\n");
					strcat(frm_ptr,"/LABEL/\n");
					if (nfld > 1) irow += 2;
					sprintf(tmp,"/POSITION/ 5,%d\n",irow);
					strcat(frm_ptr,tmp);
					strcat(frm_ptr,"/SIZE/ 85,14\n");
					strcat(frm_ptr,"/TYPE/ UD_DASINT\n");
				}
/*
........Text field
*/
				else
				{
					strcat(frm_ptr,"#DISPLAY#\n");
					sprintf(tmp,"/LABEL/ %s:\n",ctmp[j]);
					strcat(frm_ptr,tmp);
					sprintf(tmp,"/POSITION/ 5,%d,30,%d\n",irow,irow);
					strcat(frm_ptr,tmp);
					strcat(frm_ptr,"/SIZE/ 115,14\n");
					strcat(frm_ptr,"/TYPE/ UD_DASSTRING\n");
					strcat(frm_ptr,"/LEN/ 12\n");
					strcat(frm_ptr,"/PREC/ 12\n");
				}
/*
........All field types
*/
				irow += 14;
				Smon_fld[Smon_nfld] = i;
				Smon_nfld++;
			}
		}
	}
/*
.....End of form
*/
	strcat(frm_ptr,"~END\n");
	inc = strlen(frm_ptr);
/*
.....Set up field properties
*/
	for (i=0;i<Smon_nfld;i++)
	{
		if (Smon_fld[i] == IPVMON_PROGRESS)
		{
			ans[i] = &Smon_prog;
			Smon_prog = 0.;
		}
		else
		{
			ans[i] = (int *)&Smon_text[i];
			strcpy(Smon_text[i]," ");
		}
		methods[i] = UU_NULL;
		called[i] = 6;
		traverse[i] = 1;
	}
	called[Smon_nfld] = 6;
	methods[Smon_nfld] = OnCloseMon;
/*
.....Display the form
*/
	SfrmMon = ud_form_display1("INTERNAL.INTERNAL",ans,ans,methods,called,
		UU_NULL,traverse);
/*
.....Initialize previous motion parameters
*/
	Sblock.isn = -99999;
	Sblock.type = -99999;
	Sblock.tlno = -99999.9999;
	Sblock.tlen = -99999.9999;
	Sblock.coolnt = -99999;
	Sblock.sp_dir = -99999;
	Sblock.sp_val = -99999.9999;
	Sblock.cc_mode = -99999;
	Sblock.cc_dir = -99999;
	Sblock.fr_mode = -99999;
	Sblock.fedrat = -99999.9999;
	Sblock.movtim = -99999.9999;
	Sblock.mchtim = 0.;
	Sblock.progress = 0.;
	for (i=0;i<3;i++)
	{
		Sblock.tend[i] = -99999.9999;
		Sblock.tax[i] = -99999.9999;
		Sblock.cutr[i] = -99999.9999;
	}
	for (i=0;i<13;i++) Sblock.axis[i] = -99999.9999;
	Smtype = -1;
/*
.....End of routine
*/
done:;
	if (frm_ptr !=  UU_NULL) uu_lsdel(frm_ptr);
/*
.....Locks up Standalone NCLIPV
.....for a bit of time
*/
/* if (SfrmMon != 0) ud_update_form(SfrmMon); */
}

/*********************************************************************
**    E_FUNCTION :  ul_ipv_monitor_close()
**      Close the NCLIPV Monitor panel.
**    PARAMETERS
**       INPUT:
**         none
**       OUTPUT:
**         none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_monitor_close()
{
	if (SfrmMon != 0) ud_close_dispfrm(SfrmMon);
	SfrmMon = 0;
}

/*********************************************************************
**    I_FUNCTION     : OnCloseMon()
**       Callback routine for the NCLIPV Monitor close button.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnCloseMon()
{
/*
.....Mark the form as closed
*/
	SfrmMon = 0;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_monitor_active()
**       Determines if the NCLIPV Monitor Panel is currently active.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_TRUE if the Monitor Panel is displayed, UU_FALSE
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ul_ipv_monitor_active()
{
	return(SfrmMon);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_update_monitor(mblock,mdata,mattr)
**       Updates the NCLIPV Monitor Panel form.
**    PARAMETERS   
**       INPUT  : 
**          mblock    = Motion (position) block structure.
**          mdata     = Motion data structure.
**          mattr     = Motion attribute structure.
**          prog_only = UU_TRUE updates the progress bar only.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_update_monitor(mblock,mdata,mattr,prog_only)
UN_motion_block *mblock;
UN_mot_data *mdata;
UN_mot_attr *mattr;
UU_LOGICAL prog_only;
{
	int ifld,i,type,inc;
	UU_LOGICAL iaxs;
	UU_REAL dia,rad,hgt,rval,axis[LW_MAX_AXES];
	UN_cutter_list *cpt;
	static char *cmode[]={"Linear","Circular","Rapid","Cycle","Multi-axis"};
	static char *cmach[]={"Mill","Lathe","Mill/Turn","Stringer"};
	static char *ccdir[]={"Off","Left","Right"};
	static char *cftyp[]={"Fpm","Fpr"};
	static char *cspdir[]={"Clw","Cclw"};
	static char *ccool[]={"Off","Flood","Mist","Air"};
/*
.....Return if form has been closed
*/
	if (SfrmMon == 0) return;
	iaxs = UU_FALSE;
/*
.....Progress Bar
*/
	if (LW_monitor_field[IPVMON_PROGRESS] == 1)
	{
		ifld = Sactfld[IPVMON_PROGRESS];
		if (LW_progress_total == 0) Smon_prog = 100;
		else Smon_prog =
			((UU_REAL)LW_progress_count/(UU_REAL)LW_progress_total*100);
		if (Smon_prog != Sblock.progress)
		{
			ud_dispfrm_update_answer(SfrmMon,ifld,&Smon_prog);
			Sblock.progress = Smon_prog;
		}
	}
	if (prog_only) goto done;
/*
.....Update ISN
*/
	if (LW_monitor_field[IPVMON_ISN] == 1 && mdata->isn != Sblock.isn)
	{
		ifld = Sactfld[IPVMON_ISN];
		sprintf(Smon_text[ifld],"%d",mdata->isn);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Sblock.isn = mdata->isn;
	}
/*
.....Machining mode
*/
	type = mblock->type; if (type >= 100) type -= 100;
	if (LW_monitor_field[IPVMON_MODE] == 1 && type != Sblock.type)
	{
		ifld = Sactfld[IPVMON_MODE];
		strcpy(Smon_text[ifld],cmode[type]);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Sblock.type = type;
	}
/*
.....Machine type
*/
	if (LW_monitor_field[IPVMON_MACHIN] == 1 && LW_mach_type != Smtype)
	{
		ifld = Sactfld[IPVMON_MACHIN];
		strcpy(Smon_text[ifld],cmach[LW_mach_type]);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Smtype = LW_mach_type;
	}
/*
.....Tool end point
*/
	if (LW_monitor_field[IPVMON_TEND] == 1)
	{
		for (i=0;i<3;i++)
		{
			ifld = Sactfld[IPVMON_TEND] + i;
			if (fabs(mblock->ept[i]-Sblock.tend[i]) > UM_FUZZ)
			{
				UM_len_inttoext(mblock->ept[i],rval);
				sprintf(Smon_text[ifld],"%9.4lf",rval);
				ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
				Sblock.tend[i] = mblock->ept[i];
			}
		}
	}
/*
.....Tool axis
*/
	if (LW_monitor_field[IPVMON_TLAXIS] == 1)
	{
		for (i=0;i<3;i++)
		{
			ifld = Sactfld[IPVMON_TLAXIS] + i;
			if (fabs(mblock->ept[i+3]-Sblock.tax[i]) > UM_FUZZ)
			{
				sprintf(Smon_text[ifld],"%8.6lf",mblock->ept[i+3]);
				ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
				Sblock.tax[i] = mblock->ept[i+3];
			}
		}
	}
/*
.....Machine axes
*/
	if (LW_monitor_field[IPVMON_LINAXS] == 1 ||
		LW_monitor_field[IPVMON_ROTAXS] == 1)
	{
		if (!iaxs) ul_ipv_calc_axis(mblock->axis,mblock->type,axis);
		iaxs = UU_TRUE;
		for (i=0;i<Snaxis;i++)
		{
			ifld = Sactfld[IPVMON_LINAXS] + i;
			if (fabs(axis[Saxis[i]]-Sblock.axis[Saxis[i]]) > UM_FUZZ)
			{
				if (Saxis[i] < 6)
				{
					UM_len_inttoext(axis[Saxis[i]],rval);
				}
				else rval = axis[Saxis[i]];
				sprintf(Smon_text[ifld],"%9.4lf",rval);
				ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
				Sblock.axis[Saxis[i]] = axis[Saxis[i]];
			}
		}
	}
/*
.....Stringer axes
*/
	if (LW_monitor_field[IPVMON_HEAD2] == 1 ||
	   LW_monitor_field[IPVMON_HEAD3] == 1 ||
		LW_monitor_field[IPVMON_HEAD4] == 1)
	{
		if (!iaxs) ul_ipv_calc_axis(mblock->axis,mblock->type,axis);
		iaxs = UU_TRUE;
		for (i=0;i<Snhead;i++)
		{
			inc = Shead[i];
			ifld = Sactfld[IPVMON_HEAD2] + i;
			if (fabs(axis[inc]-Sblock.axis[inc]) > UM_FUZZ)
			{
				if (Shead[i] == 2 || Shead[i] >= 6)
				{
					UM_len_inttoext(axis[inc],rval);
				}
				else rval = axis[inc];
				sprintf(Smon_text[ifld],"%9.4lf",rval);
				ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
				Sblock.axis[inc] = axis[inc];
			}
		}
	}
/*
.....LOADTL
*/
	if (LW_monitor_field[IPVMON_LOADTL] == 1 &&
		(mattr->loadtl != Sblock.tlno || mattr->tlen != Sblock.tlen))
	{
		ifld = Sactfld[IPVMON_LOADTL];
		UM_len_inttoext(mattr->tlen,rval);
		sprintf(Smon_text[ifld],"%d , %6.3lf",mattr->loadtl,rval);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Sblock.tlno = mattr->loadtl;
		Sblock.tlen = mattr->tlen;
	}
/*
.....Get Cutter parameters
*/
	cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	if (cpt != UU_NULL && LW_act_tool[0] >= 0)
	{
		dia = cpt[LW_act_tool[0]].cutter[0];
		rad = cpt[LW_act_tool[0]].cutter[1];
		hgt = cpt[LW_act_tool[0]].cutter[2];
	}
	else
	{
		dia = 0.;
		rad = 0.;
		hgt = 0.;
	}
/*
.....Cutter diameter
*/
	if (LW_monitor_field[IPVMON_DIA] == 1 && dia != Sblock.cutr[0])
	{
		ifld = Sactfld[IPVMON_DIA];
		UM_len_inttoext(dia,rval);
		sprintf(Smon_text[ifld],"%6.3lf",rval);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Sblock.cutr[0] = dia;
	}
/*
.....Cutter radius
*/
	if (LW_monitor_field[IPVMON_RAD] == 1 && dia != Sblock.cutr[1])
	{
		ifld = Sactfld[IPVMON_RAD];
		UM_len_inttoext(rad,rval);
		sprintf(Smon_text[ifld],"%6.3lf",rval);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Sblock.cutr[1] = rad;
	}
/*
.....Cutter height
*/
	if (LW_monitor_field[IPVMON_HGT] == 1 && dia != Sblock.cutr[2])
	{
		ifld = Sactfld[IPVMON_HGT];
		UM_len_inttoext(hgt,rval);
		sprintf(Smon_text[ifld],"%6.3lf",rval);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Sblock.cutr[2] = hgt;
	}
/*
.....Cutcom
*/
	if (LW_monitor_field[IPVMON_CUTCOM] == 1 && mattr->cc_dir != Sblock.cc_dir)
	{
		ifld = Sactfld[IPVMON_CUTCOM];
		strcpy(Smon_text[ifld],ccdir[mattr->cc_dir]);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Sblock.cc_dir = mattr->cc_dir;
	}
/*
.....Feed rate
*/
	if (LW_monitor_field[IPVMON_FEDRAT] == 1 &&
		(mdata->fr_val != Sblock.fedrat || mdata->fr_mode != Sblock.fr_mode))
	{
		ifld = Sactfld[IPVMON_FEDRAT];
		if (mdata->fr_mode == 0)
			strcpy(Smon_text[ifld],cmode[2]);
		else
		{
			UM_len_inttoext(mdata->fr_val,rval);
			sprintf(Smon_text[ifld],"%6.3lf,%s",rval,cftyp[mdata->fr_mode-1]);
		}
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Sblock.fedrat = mdata->fr_val;
		Sblock.fr_mode = mdata->fr_mode;
	}
/*
.....Move time
*/
	if (LW_monitor_field[IPVMON_MOVTIM] == 1 && mblock->time != Sblock.movtim)
	{
		ifld = Sactfld[IPVMON_MOVTIM];
		sprintf(Smon_text[ifld],"%6.3lf",mblock->time);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Sblock.movtim = mblock->time;
	}
/*
.....Spindle
*/
	if (LW_monitor_field[IPVMON_SPINDL] == 1 &&
		(mattr->sp_val != Sblock.sp_val || mattr->sp_mode != Sblock.sp_dir))
	{
		ifld = Sactfld[IPVMON_SPINDL];
		sprintf(Smon_text[ifld],"%6.3lf,%s",mattr->sp_val,
			cspdir[mattr->sp_mode]);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Sblock.sp_val = mattr->sp_val;
		Sblock.sp_dir = mattr->sp_mode;
	}
/*
.....Coolant
*/
	if (LW_monitor_field[IPVMON_COOLNT] == 1 && mattr->coolnt != Sblock.coolnt)
	{
		ifld = Sactfld[IPVMON_COOLNT];
		strcpy(Smon_text[ifld],ccool[mattr->coolnt]);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
		Sblock.coolnt = mattr->coolnt;
	}
/*
.....Total Machine time
*/
	Sblock.mchtim = Sblock.mchtim + mblock->time;
	if (LW_monitor_field[IPVMON_MCHTIM] == 1)
	{
		ifld = Sactfld[IPVMON_MCHTIM];
		sprintf(Smon_text[ifld],"%6.3lf",Sblock.mchtim);
		ud_dispfrm_update_answer(SfrmMon,ifld,(int *)&Smon_text[ifld]);
	}
/*
.....Update the form
*/
done:;
	ud_update_form(SfrmMon);
}

/*********************************************************************
**    I_FUNCTION     :  OnReset(filedno, val, stat)
**       Resets the NCLIPV session.
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
static UD_FSTAT OnReset(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Reset session
*/
	ul_ipv_reset_session(UU_TRUE);
/*
.....Start at beginning of playback
*/
	UN_clfile_current = UN_last_clpt[1] = UN_clfile_start;
	UN_clfile_curpt = 0;
	Sblock.mchtim = 0.;
/*
........Load the current clfile parameters
........when starting from a "*Command"
*/
	if (UN_playfile_start != 0)
			ncl_fill_sequnc(UN_clfile_start);
/*
.....End of routine
*/
	return(UD_FLDOK);
}
