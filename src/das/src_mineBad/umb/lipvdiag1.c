/*********************************************************************
**    NAME         :  lipvdiag1.c
**       CONTAINS:
**				ul_ipv_diag_form
**				ul_ipv_clash_form
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvdiag1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:13
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "lcom.h"
#include "lipv.h"
#include "lipvmach.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"

#define FLOG 1
#define FLST 3
#define NFLD 32
#define NADD 6

#define FSRC 1
#define FCAL 2
#define FRST 3
#define FEDT 4

static int Sresfl,Scolor,Sreset=1,Sisn,Slast_isn;
static int Sfrm1,Sfrm2,Sinc;
static UU_LOGICAL Sopnd,Sactive1,Sactive2;
static UU_LOGICAL Sstopfl[16],Slogfl[16];
static UX_pathname Sfile,Sfile_last,Sfile_first;
static UD_LIST Sstat_list,Ssrc_list,Scall_list,Slog_list;

static UD_FSTAT OnFile(),OnClash(),OnLogFile(),OnClose1(),OnClose2(),OnView();
static UD_FSTAT OnBrowse(),OnReset(),OnClear(),OnDefault(),OnSet(),OnEdit();
static UD_FSTAT OnSource(),OnStack(),OnFind();
static void S_diag_form(),S_log_form(),S_close_log(),S_init_log(),S_load_log();
static void S_write_msg(),S_fill_lists(),S_add_bounds();

/*********************************************************************
**    E_FUNCTION     : ul_ipv_diag_form()
**       Processes the NCLIPV Clash detection form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_diag_form()
{
	int i;
	UU_LOGICAL reject;
/*
.....Initialize the form variables
*/
	for (i=0;i<16;i++)
	{
		Sstopfl[i] = LW_clash_stop[i];
		Slogfl[i] = LW_clash_log[i];
	}
	Scolor = LW_clash_color;
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
			LW_clash_log[i] = Slogfl[i];
		}
		LW_clash_color = Scolor;
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
void ul_ipv_clash_form(clash)
LW_clash_rec_struc clash[];
{
	int markval,i,status,ix,tlno;
	UU_LOGICAL stopfl,logfl,rejfl,disfrm,frmflag;
	char sbuf[2][20],dbuf[20],cbuf[80],buf[80];
	UN_cutter_list *cpt;
	LW_mach_axis_style style[2];
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1,1,1,1,1,1,1};
	static UD_METHOD methods[] = {UU_NULL,OnSource,OnStack,UU_NULL,OnEdit,
		OnView,OnFind,OnClash};
	static char called[] = {6,6,6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1,1,1,1};
	static int *ans[] = {(int *)&Sstat_list,(int *)&Ssrc_list,(int *)&Scall_list,
		&Sreset,UU_NULL,UU_NULL,UU_NULL,UU_NULL};
/*
.....Initialize routine
*/
	getnln(&Slast_isn);
	stopfl = logfl = disfrm = frmflag = UU_FALSE;
	rejfl = UU_FALSE;
	Sactive2 = UU_FALSE;
	Sinc = 0;
	Scolor = LW_clash_color;
	tlno = LW_mot_attr->tlno;

	Sstat_list.num_item = 0;
	Sstat_list.item = UU_NULL;
	Sstat_list.answer = UU_NULL;

	Ssrc_list.num_item = 0;
	Ssrc_list.item = UU_NULL;
	Ssrc_list.answer = UU_NULL;
/*
.....Determine style of solids that clashed
*/
	cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
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
				case LW_CLASH_STOCK:
					style[i] = LW_MACH_STOCK;
					break;
				case LW_CLASH_FIXTURE:
					style[i] = LW_MACH_FIXTURE;
					break;
				case LW_CLASH_TOOL:
					style[i] = LW_MACH_TOOL;
					break;
				case LW_CLASH_HOLDER:
					style[i] = LW_MACH_HOLDER;
					break;
				case LW_CLASH_SHANK:
					if (cpt[LW_act_tool[0]].shank_clash == 1)
						style[i] = LW_MACH_HOLDER;
					else
						style[i] = LW_MACH_TOOL;
					break;
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
			if (!stopfl && !logfl) continue;
/*
.....Determine the clash type
*/
			if (LW_mot_data->fr_mode == 0 ||
				(cpt[LW_act_tool[0]].rapid != 0. &&
				LW_mot_data->fr_val >= cpt[LW_act_tool[0]].rapid))
					strcpy(dbuf,"RAPID");
			else
				strcpy(dbuf,"CLASH");
/*
.....Get clash entities
*/
			for (i=0;i<2;i++)
			{
				switch (clash[ix].ent[i])
				{
				case LW_CLASH_STOCK:
					sprintf(sbuf[i],"Stock #%d",clash[ix].id[i]);
					break;
				case LW_CLASH_FIXTURE:
					sprintf(sbuf[i],"Fixture #%d",clash[ix].id[i]);
					break;
				case LW_CLASH_TOOL:
					strcpy(sbuf[i],"Tool");
					tlno = clash[ix].id[i];
					break;
				case LW_CLASH_HOLDER:
					strcpy(sbuf[i],"Holder");
					tlno = clash[ix].id[i];
					break;
				case LW_CLASH_SHANK:
					strcpy(sbuf[i],"Shank");
					tlno = clash[ix].id[i];
					break;
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
				ul_ipv_init_list(&Sstat_list,100);
				ul_ipv_init_list(&Ssrc_list,100);
				for (i=0;i<16;i++)
				{
					Sstopfl[i] = LW_clash_stop[i];
					Slogfl[i] = LW_clash_log[i];
				}
				S_init_log();
			}
			if (stopfl) frmflag = UU_TRUE;
/*
.....Output the clash record
*/
			S_write_msg(" ",stopfl,logfl);
			sprintf(buf,"%s   - %s and %s collide at ISN #%d",dbuf,sbuf[0],sbuf[1],
				clash[ix].isn);
			S_write_msg(buf,stopfl,logfl);
			sprintf(buf,"          Location = %g,%g,%g",clash[ix].loc[0],
				clash[ix].loc[1],clash[ix].loc[2]);
			S_write_msg(buf,stopfl,logfl);
			ul_ipv_format_tool(cbuf,LW_act_tool[tlno]);
			strcpy(buf,"          ");
			strcat(buf,cbuf);
			S_write_msg(buf,stopfl,logfl);
			if (LW_mot_data->fr_mode != 0)
			{
				if (LW_mot_data->fr_mode == 2)
					sprintf(buf,"          FEDRAT/FPR,%f",LW_mot_data->fr_val);
				else
					sprintf(buf,"          FEDRAT/FPM,%f",LW_mot_data->fr_val);
				S_write_msg(buf,stopfl,logfl);
			}
		}
/*
.....Post-processor error
*/
		else
		{
			if (LW_clash_log[15]) logfl = UU_TRUE;
			if (LW_clash_stop[15]) stopfl = UU_TRUE;
/*
.....Initialize form
*/
			if (stopfl && !frmflag)
			{
				ul_ipv_init_list(&Sstat_list,100);
				ul_ipv_init_list(&Ssrc_list,100);
				for (i=0;i<16;i++)
				{
					Sstopfl[i] = LW_clash_stop[i];
					Slogfl[i] = LW_clash_log[i];
				}
				S_init_log();
			}
			if (stopfl) frmflag = UU_TRUE;
/*
.....Output the clash record
*/
			S_write_msg(" ",stopfl,logfl);
			sprintf(buf,"Post-processor error at ISN #%d",clash[ix].isn);
			S_write_msg(buf,stopfl,logfl);
			S_write_msg(clash[ix].errmsg1,stopfl,logfl);
			if (clash[ix].errnc[1] != 0)
				S_write_msg(clash[ix].errmsg2,stopfl,logfl);
		}
/*
.....Increment the number of errors
*/
		if (logfl) LW_errors++;
	}
/*
.....Load the source lines
*/
	Sisn = clash[0].isn;
	if (!frmflag) goto done;
	if (LW_mot_ndata == 0)
		S_fill_lists(UU_NULL);
	else
		S_fill_lists(LW_mot_data);
/*
.....Command Reject
*/
	UD_MARK(markval,UU_FALSE);
	if (markval != 0) goto reject;
/*
.....Don't display EDIT fields
.....with standalone NCLIPV
*/
	if (LW_nclipv == LW_STANDALONE)
	{
		display[FRST] = display[FEDT] = 0;
		traverse[FRST] = traverse[FEDT] = 0;
	}
/*
.....Display the Form
*/
	status = ud_form1("ipvclash.frm", ans, ans, methods, called,
		display, traverse);
	if (status == -1) goto reject;
/*
.....Save form results
*/
	for (i=0;i<16;i++)
	{
		LW_clash_stop[i] = Sstopfl[i];
		LW_clash_log[i] = Slogfl[i];
	}
	LW_clash_color = Scolor;
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
		ud_free_flist(&Sstat_list);
		ud_free_flist(&Ssrc_list);
		S_close_log(rejfl);
		UD_UNMARK(markval);
	}
	LW_nclash = 0;
	LW_clash_flag = UU_FALSE;
	return;
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
**    SIDE EFFECTS : The static variables 'Sstopfl' and 'Slogfl' are
**                   used and set by this form.
**    WARNINGS     : none
*********************************************************************/
static void S_diag_form(frmfl,reject)
int frmfl;
UU_LOGICAL *reject;
{
	int *ans[NFLD+2],i,markval,status;
	char traverse[NFLD+NADD];
	UD_METHOD methods[NFLD+NADD];
	char called[NFLD+NADD];
	char display[NFLD+NADD];
/*
.....Initialize routine
*/
	if (frmfl == 1 && Sactive2) return;
	for (i=0;i<NFLD+NADD;i++)
	{
		traverse[i] = display[i] = 1;
		called[i] = 6; methods[i] = UU_NULL;
	}
	if (!LW_mach_simul)
	{
		for (i=8;i<15;i++)
		{
			traverse[i] = traverse[i+16] = 0;
		}
		traverse[2] = traverse[3] = traverse[6] = traverse[7] = 0;
		traverse[18] = traverse[19] = traverse[22] = traverse[23] = 0;
	}
	methods[NFLD] = OnLogFile;
	methods[NFLD+2] = OnClear;
	methods[NFLD+3] = OnSet;
	methods[NFLD+4] = OnDefault;
	if (frmfl == 1) methods[NFLD+4] = OnClose2;
/*
.....Store the default answers
*/
	for (i=0;i<NFLD/2;i++)
	{
		ans[i] = (int *)&Sstopfl[i];
		ans[i+NFLD/2] = (int *)&Slogfl[i];
	}
	ans[NFLD+1] = &Scolor;
/*
.....Display the form
*/
	if (frmfl == 1)
	{
		Sfrm2 = ud_form_display1("ipvmachdiag.frm", ans, ans, methods, called,
			display, traverse);
		if (Sfrm2 != -1) Sactive2 = UU_TRUE;
	}
/*
.....Standard form
*/
	else
	{
/*
........Initialize log file
*/
		S_init_log();
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
			status = ud_form1("ipvmachdiag.frm", ans, ans, methods, called,
				display, traverse);
			if (status == -1) *reject = UU_TRUE;
		}
/*
........Close log file
*/
		S_close_log(*reject);
		UD_UNMARK(markval);
	}
}

/*********************************************************************
**    S_FUNCTION     :  OnBrowse(filedno, val, stat)
**       Method called when the Filename button is pushed.
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
static UD_FSTAT OnBrowse(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int inum;
	char sbuf[80],ext[UX_SUFFIX_LEN],descrip[80];
/*
.....Get name of log file
*/
	strcpy(ext,"*.log");
	strcpy(descrip,"Log Files (*.log)");
	inum = 0;
	ud_get_filename(sbuf,sbuf,ext,Sfile,&inum,descrip, 1, UU_FALSE);
	if (inum != 0)
	{
		ux_add_ftype("log",Sfile,UX_NPRTERRS);
		ud_dispfrm_update_answer(Sfrm1,FLOG,(int *)&Sfile);
		OnFile(fieldno, val, stat);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSource(fieldno, val, stat)
**       Method called at when  tool in the tool listbox
**			is selected
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
static UD_FSTAT OnSource(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char temp[81], *tok, *strtok();
/*
.....val.frmstr contains the selected string
....."isn COMMAND"
*/
	if (val->frmstr!=NULL)
	{
		strcpy(Ssrc_list.answer, val->frmstr);
		strcpy(temp, val->frmstr);
		tok = strtok(temp, " ");
		if (tok!=NULL) Sisn = atoi(tok);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnStack(fieldno, val, stat)
**       Updates the Source list when a line is selected in the
**       Call Stack list.
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
static UD_FSTAT OnStack(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int isn;
	char *tok,*strtok();
	char temp[81];
/*
.....Get ISN of call stack line
.....and display appropriate source lines
*/
	if (val->frmstr != UU_NULL)
	{
		strcpy(Scall_list.answer,val->frmstr);
		strcpy(temp,val->frmstr);
		tok = strtok(temp,":");
		if (tok != UU_NULL)
		{
			isn = atoi(tok);
			ncl_motisn_source_list(isn,&Ssrc_list);
		}
		ud_update_answer(FSRC,(int *)&Ssrc_list);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnEdit(fieldno, val, stat)
**       Method called when the "Edit" button is pushed
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
static UD_FSTAT OnEdit(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int markval;
	if (Sisn > 0)
	{
		UD_MARK(markval,UU_TRUE);
		if (markval != 0) goto done;
		setnln(&Sisn);
		ud_form_invis();
		ncl_cmd_mode();
done:;
		if (Sreset) setnln(&Slast_isn);
		ud_form_vis();
		UD_UNMARK(markval);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnView(fieldno, val, stat)
**       Method called when the Clash View button is pushed.
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
static UD_FSTAT OnView(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL reject;
/*
.....Take down the form and enter dynamic viewing
*/
	ud_form_invis();
	ul_ipv_view_active(); uz_dyn_mouse();
	ud_form_vis();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnFind(fieldno, val, stat)
**       Method called when the "Find" button is pushed
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
static UD_FSTAT OnFind(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	UU_REAL dis;
	LtData data;
	LtDoubleBounds bounds,box;
/*
.....Initialize bounding box
*/
	box[LI_MINX] = 100000.;
	box[LI_MINY] = 100000.;
	box[LI_MINZ] = 100000.;
	box[LI_MAXX] = -100000.;
	box[LI_MAXY] = -100000.;
	box[LI_MAXZ] = -100000.;
/*
.....Calculate bounding box of tool
*/
	if (LW_tool[0] != 0 || LW_shank[0] != 0 || LW_num_holder[0] != 0)
	{
		if (LW_tool[0] != 0)
		{
			LiSessionPrimGetProperty(LW_tool[0],LI_SOLID_PROP_MW_BOUNDS,&data);
			LiDataGetDoubleBounds(&data,bounds);
			S_add_bounds(bounds,box);
		}
		if (LW_shank[0] != 0)
		{
			LiSessionPrimGetProperty(LW_shank[0],LI_SOLID_PROP_MW_BOUNDS,&data);
			LiDataGetDoubleBounds(&data,bounds);
			S_add_bounds(bounds,box);
		}
		for (i=0;i<LW_num_holder[0];i++)
		{
			LiSessionPrimGetProperty(LW_holder[0][i],LI_SOLID_PROP_MW_BOUNDS,
				&data);
			LiDataGetDoubleBounds(&data,bounds);
			S_add_bounds(bounds,box);
		}
	}
/*
.....Tool is not defined
.....Assume 5x5x5 box
*/
	else
	{
		box[LI_MINX] = -5.;
		box[LI_MINY] = -5.;
		box[LI_MINZ] = -5.;
		box[LI_MAXX] = 5.;
		box[LI_MAXY] = 5.;
		box[LI_MAXZ] = 5.;
	}
/*
.....Adjust box for clash position
*/
	dis = (box[LI_MAXX]-box[LI_MINX]) / 2.;
	LW_diff_bounds[LI_MINX] = LW_clash_record[Sinc].loc[0] - dis;
	LW_diff_bounds[LI_MAXX] = LW_clash_record[Sinc].loc[0] + dis;
	dis = (box[LI_MAXY]-box[LI_MINY]) / 2.;
	LW_diff_bounds[LI_MINY] = LW_clash_record[Sinc].loc[1] - dis;
	LW_diff_bounds[LI_MAXY] = LW_clash_record[Sinc].loc[1] + dis;
	dis = (box[LI_MAXZ]-box[LI_MINZ]) / 2.;
	LW_diff_bounds[LI_MINZ] = LW_clash_record[Sinc].loc[2] - dis;
	LW_diff_bounds[LI_MAXZ] = LW_clash_record[Sinc].loc[2] + dis;
	Sinc++; if (Sinc >= LW_nclash) Sinc = 0;
/*
.....Fit the view to the requested clash
*/
	LW_diff_solid_view = UU_TRUE;
	ul_ipv_view_active();
	uz_extrema_zoom();
	ul_ipv_view_inactive();
	LW_diff_solid_view = UU_FALSE;
	return(UD_FLDOK);
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
**    E_FUNCTION     : OnClear()
**       Clears all stop and log fields in the clash form.
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
static UD_FSTAT OnClear(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
/*
.....Clear all clash fields
*/
	for (i=0;i<NFLD/2;i++)
	{
		Sstopfl[i] = Slogfl[i] = 0;
		if (Sactive2)
		{
			ud_dispfrm_update_answer(Sfrm2,i,(int *)&Sstopfl[i]);
			ud_dispfrm_update_answer(Sfrm2,i+NFLD/2,(int *)&Slogfl[i]);
		}
		else
		{
			ud_update_answer(i,(int *)&Sstopfl[i]);
			ud_update_answer(i+NFLD/2,(int *)&Slogfl[i]);
		}
	}
/*
....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose1(filedno, val, stat)
**       Method called when the Log File form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose1()
{
	Sactive1 = UU_FALSE;
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
**    E_FUNCTION     : OnDefault()
**       Changes all stop and log fields in the clash form to their default
**       settings.
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
static UD_FSTAT OnDefault(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
/*
.....Clear all clash fields
*/
	for (i=0;i<NFLD/2;i++)
	{
		Sstopfl[i] = LW_default_stop[i];
		Slogfl[i] = LW_default_log[i];
		if (Sactive2)
		{
			ud_dispfrm_update_answer(Sfrm2,i,(int *)&Sstopfl[i]);
			ud_dispfrm_update_answer(Sfrm2,i+NFLD/2,(int *)&Slogfl[i]);
		}
		else
		{
			ud_update_answer(i,(int *)&Sstopfl[i]);
			ud_update_answer(i+NFLD/2,(int *)&Slogfl[i]);
		}
	}
/*
....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnFile(fieldno, val, stat)
**       Method called when the log filename field is changed.
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
static UD_FSTAT OnFile(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
	UU_LOGICAL opnd;
/*
.....Only load the log file
.....if the filename has changed
*/
	if (strcmp(Sfile,Sfile_last) != 0)
	{
/*
.....Get and load the log file
*/
		opnd = ul_ipv_get_diagfile(Sfile_last);
		if (opnd) ul_ipv_close_diag();
		status = ul_ipv_open_diag(Sfile,UU_FALSE,LI_FILE_READ);
		if (status == UU_SUCCESS) S_load_log();
		ud_dispfrm_update_answer(Sfrm1,FLST,(int *)&Slog_list);
		if (opnd) ul_ipv_open_diag(Sfile_first,UU_FALSE,LI_FILE_APPEND);
		strcpy(Sfile_last,Sfile);
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : OnLogFile()
**       Processes the NCLIPV Log File form.
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
static UD_FSTAT OnLogFile(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1, 1, 1,1};
	static UD_METHOD methods[] = {OnBrowse,OnFile,UU_NULL,UU_NULL,OnReset,
		OnClose1};
	static char called[] = {6,6, 6, 6,6};
	static char display[] = {1,1, 1, 1,1};
	static int *ans[] = {UU_NULL,(int *)Sfile,(int *)&Sresfl,
		(int *)&Slog_list,UU_NULL};
/*
.....Display the Form
*/
	if (!Sactive1)
	{
		Sfrm1 = ud_form_display1("ipvdiag.frm", ans, ans, methods, called,
			display, traverse);
		if (Sfrm1 != -1) Sactive1 = UU_TRUE;
	}
/*
.....Open new log file if necessary
*/
	if (strcmp(Sfile,Sfile_first) != 0)
	{
		ul_ipv_close_diag();
		if (Sopnd) ul_ipv_open_diag(Sfile,UU_FALSE,LI_FILE_APPEND);
		else strcpy(LW_diag_file,Sfile);
	}
/*
....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnReset(filedno, val, stat)
**       Method called when the RESET button is pressed.
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
	int status;
	UU_LOGICAL opnd;
	char fname[UX_MAX_PATH_LEN];
/*
.....Is the log file opened?
*/
	opnd = ul_ipv_get_diagfile(fname);
	if (opnd) ul_ipv_close_diag();
/*
.....Clear log file
*/
	status = ul_ipv_open_diag(Sfile,UU_FALSE,LI_FILE_WRITE);
	if (status == UU_SUCCESS) ul_ipv_close_diag();
	ul_ipv_init_list(&Slog_list,10000);
	ud_dispfrm_update_answer(Sfrm1,FLST,(int *)&Slog_list);
/*
.....Open original log file
*/
	if (opnd) ul_ipv_open_diag(Sfile_first,UU_FALSE,LI_FILE_APPEND);
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : OnSet()
**       Sets all stop and log fields in the clash form.
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
static UD_FSTAT OnSet(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
/*
.....Set all clash fields
*/
	for (i=0;i<NFLD/2;i++)
	{
		Sstopfl[i] = Slogfl[i] = 1;
		if (Sactive2)
		{
			ud_dispfrm_update_answer(Sfrm2,i,(int *)&Sstopfl[i]);
			ud_dispfrm_update_answer(Sfrm2,i+NFLD/2,(int *)&Slogfl[i]);
		}
		else
		{
			ud_update_answer(i,(int *)&Sstopfl[i]);
			ud_update_answer(i+NFLD/2,(int *)&Slogfl[i]);
		}
	}
/*
....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  S_close_log(reject)
**       Saves the Log File form data when the main form is closed.
**    PARAMETERS
**       INPUT  :
**          reject  = UU_TRUE if main form was rejected.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_close_log(reject)
UU_LOGICAL reject;
{
/*
.....Save form results
*/
	if (!reject)
	{
		LW_reset_log = Sresfl;
/*
.....Open new log file if necessary
*/
		if (strcmp(Sfile,Sfile_first) != 0)
		{
			ul_ipv_close_diag();
			if (Sopnd) ul_ipv_open_diag(Sfile,UU_FALSE,LI_FILE_APPEND);
			else strcpy(LW_diag_file,Sfile);
		}
	}
/*
.....Reject Op
.....Restore log file name
*/
	else
	{
		strcpy(LW_diag_file,Sfile_first);
	}
}

/*********************************************************************
**    S_FUNCTION     :  S_init_log()
**       Initializes the Log File form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_init_log()
{
	int status;
/*
.....Initialize Log File form
*/
	Sresfl = LW_reset_log;
/*
.....Get and load the log file
*/
	Slog_list.num_item = 0;
	Slog_list.item = UU_NULL;
	Slog_list.answer = UU_NULL;
	Sopnd = ul_ipv_get_diagfile(Sfile);
	if (Sopnd) ul_ipv_close_diag();
	status = ul_ipv_open_diag(Sfile,UU_FALSE,LI_FILE_READ);
	if (status == UU_SUCCESS) S_load_log();
	if (Sopnd) ul_ipv_open_diag(Sfile,UU_FALSE,LI_FILE_APPEND);
	strcpy(Sfile_last,Sfile);
	strcpy(Sfile_first,Sfile);
}

/*********************************************************************
**    S_FUNCTION     :  S_load_log()
**       Loads the NCLIPV log file.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_load_log()
{
	int stat,n;
	char buf[132];
/*
.....Initialize list
*/
	n = 0;
	do
	{
		stat = ul_ipv_read_log(buf);
		if (stat == UU_SUCCESS) n++;
	} while (stat == UU_SUCCESS);
	ul_ipv_init_list(&Slog_list,n);
	ul_ipv_rewind_log();
/*
.....Read the log file
*/
	do
	{
		stat = ul_ipv_read_log(buf);
		if (stat == UU_SUCCESS)
		{
			if (strlen(buf) > 80) buf[80] = '\0';
			ul_ipv_put_list(&Slog_list,buf);
		}
	} while (stat == UU_SUCCESS);
/*
.....Close the log file
*/
	ul_ipv_close_diag();
}

/*********************************************************************
**    S_FUNCTION     :  S_fill_lists(mdata)
**       Propogates the Source and Call lists with the data associated
**       with the current selection.
**    PARAMETERS
**       INPUT  :
**          mdata   = Motion attribute bundle.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fill_lists(mdata)
UN_mot_data *mdata;
{
	int isn;
/*
.....Get Call list
*/
	if (mdata == UU_NULL)
	{
		ul_ipv_init_list(&Scall_list,1);
		ul_ipv_put_list(&Scall_list,"");
		ul_ipv_init_list(&Ssrc_list,1);
		ul_ipv_put_list(&Ssrc_list,"");
	}
	else
	{
		ncl_motisn_call_list(mdata->isnptr,UU_NULL,&Scall_list,&isn,2);
		ncl_motisn_source_list(isn,&Ssrc_list);
	}
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
		ul_ipv_put_list(&Sstat_list,buf);
/*
.....Write to log file
*/
	if (logfl)
		ul_ipv_write_log(buf);
}

/*********************************************************************
**    S_FUNCTION     : S_add_bounds(bounds,box)
**       Adds the bound box 'bounds' to 'box'.
**    PARAMETERS
**       INPUT  :   bounds  = Bounding box to add.
**                  box     = Box to be added to.
**       OUTPUT :   box     = Adjusted bounding box.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_add_bounds(bounds,box)
LtDoubleBounds bounds,box;
{
	if (bounds[LI_MINX] < box[LI_MINX]) box[LI_MINX] = bounds[LI_MINX];
	if (bounds[LI_MINY] < box[LI_MINY]) box[LI_MINY] = bounds[LI_MINY];
	if (bounds[LI_MINZ] < box[LI_MINZ]) box[LI_MINZ] = bounds[LI_MINZ];
	if (bounds[LI_MAXX] > box[LI_MAXX]) box[LI_MAXX] = bounds[LI_MAXX];
	if (bounds[LI_MAXY] > box[LI_MAXY]) box[LI_MAXY] = bounds[LI_MAXY];
	if (bounds[LI_MAXZ] > box[LI_MAXZ]) box[LI_MAXZ] = bounds[LI_MAXZ];
}
