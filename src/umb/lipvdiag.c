
/*********************************************************************
**  FILENAME: lipvdiag.c
**  CONTAINS:   ul_ipv_open_diag
**              ul_ipv_get_diagfile
**              ul_ipv_log_session
**              ul_ipv_log_sessend
**              ul_ipv_close_diag
**              ul_ipv_diag_handler
**              ul_ipv_diag_error
**              ul_ipv_clash_ents()
**              ul_ipv_write_log
**              ul_ipv_read_log
**              ul_ipv_rewind_log
**              ul_ipv_reset_log
**              ul_ipv_reset_clash_colors
**              ul_ipv_reset_clash_stack
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvdiag.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       11/04/16 , 10:33:20
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "xfsys0.h"
#include "lcom.h"
#include <ctype.h>
#include "nclmplay.h"
#include "lipv.h"
#include "lipvmach.h"
#include "mdcpln.h"

void ul_ipv_diag_handler();
void ul_ipv_clash_ents();

static void S_push_clash_stack();

static struct {LtSessionPrim prim; int color;} Sclash_stack[LW_MAXCLASH];
static int Sclash_count = 0;

static FILE *Slog_fd;

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_open_diag(log_file)
**		Opens the NCLIPV diagnostic (log) file.
**	 PARAMETERS	
**		 INPUT  : 
**        log_file   = Name of log file to open.
**        headr      = UU_TRUE if header should be written.
**        ftype      = LI_FILE_type setting (read,append,etc.).
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_open_diag(log_file,headr,ftype)
char *log_file;
UU_LOGICAL headr;
int ftype;
{
#if UU_COMP == UU_WIN2K
	int i;
#endif
	int nc;
 	char fname[UX_MAX_PATH_LEN],fname1[UX_MAX_PATH_LEN],ldate[24],buf[132];
	char fn[81];
/*
.....Open log file
*/
	Slog_fd = UU_NULL;
	if (log_file == UU_NULL) goto done;
	ul_get_fname(log_file,fname);
	strcpy(LW_diag_file,fname);
	nc = strlen(fname);
/*
.....for WinNT, we allow filename with spaces,
.....only remove trailling spaces for WinNT
.....Yurong 1/17/02
*/
#if UU_COMP!=UU_WIN2K
	ul_strip_blanks(fname,&nc);
#else
/*
.....we also need to remove preceding spaces
.....Yurong 3/1/02
*/
	for (i=0; i<strlen(fname); i++)
	{
		if (fname[i]!=' ') break;
	}
	strcpy(fname, &(fname[i]));
	for (nc=strlen(fname); nc>0; nc--)
	{
		if (fname[nc-1]==' ')
			fname[nc-1] = '\0';
		else
			break;
	}
	nc = strlen(fname);
#endif
	if (nc != 0)
	{
		Slog_fd = LiFileOpen(fname,ftype);
/*
.....Could not open file
*/
		if (Slog_fd == UU_NULL)
		{
			ud_wrerr("Could not open NCLIPV log file.");
		}
/*
.....Write out header
*/
		else if (headr)
		{
			ul_ipv_write_log(" ");
			ul_ipv_write_log("-----------------------------------------------------------");
			ul_ipv_write_log("-");
			ul_build_full_fname("",fname,"",LW_diag_file);
			nc = strlen(LW_diag_file);
			if (nc < 80)
			{
				strcpy(fn,LW_diag_file);
			}
			else
			{
				strcpy(fn,"...");
				strcat(fn,&LW_diag_file[nc-78]);
			}
			sprintf(buf,"-  NCLIPV Log file: %s",fn);
			ul_ipv_write_log(buf);
			ul_date_and_time(ldate);
			sprintf(buf,"-  Date: %s",ldate);
			ul_ipv_write_log(buf);
			ul_build_full_fname("",UL_program,UL_program_suffix,fname1);
			nc = strlen(fname1);
			if (nc < 80)
			{
				strcpy(fn,fname1);
			}
			else
			{
				strcpy(fn,"...");
				strcat(fn,&fname1[nc-78]);
			}
			sprintf(buf,"-  Part program: %s",fn);
			ul_ipv_write_log(buf);
			if (UM_cpln.length_unit == 0) strcpy(ldate,"Inches");
			else strcpy(ldate,"Millimeters");
			sprintf(buf,"-  Units: %s",ldate);
			ul_ipv_write_log(buf);
			ul_ipv_write_log("-");
			ul_ipv_write_log("-----------------------------------------------------------");
		}
	}
/*
.....Set diagnostic Callback
*/
	LiCallBackSet(LI_CALL_DIAGNOSTIC,(LtFunc)ul_ipv_diag_handler);
/*
.....End of routine
*/
done:;
	return(UU_SUCCESS);
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_get_diagfile(log_file)
**		Returns the NCLIPV diagnostic (log) file name.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : 
**        log_file   = Name of diagnostic log file.
**	 RETURNS: UU_TRUE if the log file is currently open.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
UU_LOGICAL ul_ipv_get_diagfile(log_file)
char *log_file;
{
/*
.....Return log file name
*/
	strcpy(log_file,LW_diag_file);
	if (Slog_fd == UU_NULL) return(UU_FALSE);
	else return(UU_TRUE);
}
	
/*********************************************************************
**	 E_FUNCTION: int ul_ipv_log_session()
**		Writes the start of a playback sequence to the log file.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_log_session()
{
	int tsrc,tstrt,tend,stat,i;
	char buf[UX_MAX_PATH_LEN+40],tfile[UX_MAX_PATH_LEN],sbuf[80],ebuf[80];
	UN_cutter_list *ipt;
/*
.....Get toolpath currently being played back
*/
	ncl_get_clfile_src(&tsrc,tfile,&tstrt,sbuf,&tend,ebuf,UU_TRUE);
/*
.....Write to log file
*/
	ul_ipv_write_log(" ");
	sprintf(buf,"Start Processing - %s",tfile);
	stat = ul_ipv_write_log(buf);
	strcpy(buf,"Mode = ");
	if (LW_mach_mode == LW_RAPIDCUT) strcat(buf,"RapidCut");
	else strcat(buf,"VisiCut");
	stat = ul_ipv_write_log(buf);
	sprintf(buf,"Start at %s",sbuf);
	stat = ul_ipv_write_log(buf);
	sprintf(buf,"Stop at %s",ebuf);
	stat = ul_ipv_write_log(buf);
/*
.....Initialize the cutter times
*/
	ipt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	for (i=0;i<LW_ntool;i++) ipt[i].mchtim = 0.;
/*
.....Initialize number of errors
*/
	LW_errors = 0;
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**	 E_FUNCTION: int ul_ipv_log_sessend()
**		Writes the end of a playback sequence to the log file.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_log_sessend()
{
	int i,stat;
	char cbuf[140],tbuf[80];
	UU_REAL mchtim;
	UN_cutter_list *ipt;
/*
.....Write header
*/
	stat = ul_ipv_write_log(" ");
	stat = ul_ipv_write_log(" ");
	stat = ul_ipv_write_log("                       --- Toolpath Summary ---");
	stat = ul_ipv_write_log("Tool Description                                          Machine Time");
	stat = ul_ipv_write_log("----------------------------------------------------     ------------");
/*
.....Display the cutter times
*/
	ipt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	mchtim = 0.;
	for (i=0;i<LW_ntool;i++)
	{
		if (ipt[i].mchtim != 0.)
		{
			ul_ipv_format_tool(cbuf,i);
			ul_format_time(ipt[i].mchtim,tbuf);
			strcat(cbuf,"                                                            ");
			strcpy(&cbuf[60],tbuf);
			stat = ul_ipv_write_log(cbuf);
			mchtim = mchtim + ipt[i].mchtim;
		}
	}
/*
.....Display total time
*/
	ul_format_time(mchtim,tbuf);
	sprintf(cbuf,"Estimated machine time = %s",tbuf);
	stat = ul_ipv_write_log(" ");
	stat = ul_ipv_write_log(cbuf);
/*
.....Display number of errors
*/
	sprintf(cbuf,"Number of errors: %d",LW_errors);
	stat = ul_ipv_write_log(" ");
	stat = ul_ipv_write_log(cbuf);
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_close_diag()
**		Closes the NCLIPV diagnostic (log) file.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_close_diag()
{
/*
.....Close the log file
*/
	if (Slog_fd != UU_NULL) LiFileClose(Slog_fd);
	Slog_fd = UU_NULL;
}

/*********************************************************************
**	 I_FUNCTION: ul_ipv_diag_handler(status,diag,id,nargs,args)
**		NCLIPV diagnostic handler.
**	 PARAMETERS	
**		 INPUT  : status  = Severity of diagnostic.
**              diag    = Diagnostic number.
**              id      = Module ID of routine which generated diag.
**              nargs   = Number of arguments passed in 'args'.
**              args    = Arguments furnished.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_diag_handler(status,diag,id,nargs,args)
LtStatus status;
LtDiagnostic diag;
LtNat32 id;
LtNat32 nargs;
LtData *args;
{
	int i;
	char buf[200];
	LtSessionPrim prims[2];
	LtDouble pt[3];
	LW_mach_model_struc *mpt;
	switch(diag)
	{
/*
.....Clash occurred
*/
	case LI_DIAG_ERROR_CLASH_STOP:
	case LI_DIAG_ERROR_CLASH_CONTINUE:
		if (LW_nclash == LW_MAXCLASH) return;
		prims[0] = LiDataGetGenericPtr(&args[0]);
		prims[1] = LiDataGetGenericPtr(&args[1]);
		pt[0] = LiDataGetDouble(&args[2]);
		pt[1] = LiDataGetDouble(&args[3]);
		pt[2] = LiDataGetDouble(&args[4]);
		for (i=0;i<3;i++) UM_len_inttoext(pt[i],
			LW_clash_record[LW_nclash].loc[i]);
/*
........Determine clash entities
*/
		ul_ipv_clash_ents(prims);
/*
........Do not record clashes between
........Invisible spindles and other entities
*/
		for (i=0;i<LW_MAX_SPINDLE;i++)
		{
			if (LW_clash_record[LW_nclash].axis[0] == LW_spindle[i] ||
				LW_clash_record[LW_nclash].axis[1] == LW_spindle[i])
			{
				mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
				if (!mpt[LW_spindle[i]].visible) return;
				break;
			}
		}
/*
.....Set the clash colors
.....if entity is not a stock
.....(will change the color of cut faces also)
*/
		if (LW_clash_record[LW_nclash].ent[0] != LW_CLASH_STOCK)
			S_push_clash_stack(prims[0],LW_clash_record[LW_nclash].color[0]);
		if (LW_clash_record[LW_nclash].ent[1] != LW_CLASH_STOCK)
			S_push_clash_stack(prims[1],LW_clash_record[LW_nclash].color[1]);
/*
........If these two entities are already
........on the clash stack, then ignore this one
*/
		for (i=0;i<LW_nclash;i++)
		{
			if (
				(LW_clash_record[i].axis[0] == LW_clash_record[LW_nclash].axis[0] &&
				LW_clash_record[i].axis[1] == LW_clash_record[LW_nclash].axis[1]) ||
				(LW_clash_record[i].axis[0] == LW_clash_record[LW_nclash].axis[1] &&
				LW_clash_record[i].axis[1] == LW_clash_record[LW_nclash].axis[0]))
					return;
		}
/*
........Store clash parameters
*/
		if (LW_mot_data == UU_NULL)
			LW_clash_record[LW_nclash].isn = 0;
		else
			LW_clash_record[LW_nclash].isn = LW_mot_data->isn;
		LW_clash_record[LW_nclash].type = LW_DIAG_CLASH;
		LW_clash_flag = UU_TRUE;
		LW_nclash++;
		break;
/*
.....Standard diagnostic
*/
	default:
#ifdef _DEBUG
		if (status == LI_STATUS_WARNING)
		{
			sprintf(buf,"WARNING - Module = %d  Code = %d",id,diag);
			ul_ipv_write_log(buf);
		}
		else if (status == LI_STATUS_ERROR)
		{
			sprintf(buf,"ERROR   - Module = %d  Code = %d",id,diag);
			ul_ipv_write_log(buf);
			LW_errors++;
		}
#endif
		break;
	}
}

/*********************************************************************
**	 I_FUNCTION: ul_ipv_diag_error(isn,msg1,nc1,msg2,nc2)
**		NCLIPV diagnostic handler for post-processor generated errors.
**	 PARAMETERS	
**		 INPUT  : isn     = Input sequence number.
**              msg1    = First line of error message.
**              nc1     = Number of characters in 'msg1'.
**		          msg2    = Second line of error message.
**              nc2     = Number of characters in 'msg2'.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: Displays the NCLIPV diagnostic form.
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_diag_error(isn,msg1,nc1,msg2,nc2)
char *msg1,*msg2;
UM_int4 isn;
UM_int2 nc1,nc2;
{
/*
.....Store error messages in
.....clash record
*/
	LW_clash_record[LW_nclash].type = LW_DIAG_ERROR;
	LW_clash_record[LW_nclash].isn = isn;
	strcpy(LW_clash_record[LW_nclash].errmsg1,msg1);
	strcpy(LW_clash_record[LW_nclash].errmsg2,msg2);
	LW_clash_record[LW_nclash].errnc[0] = nc1;
	LW_clash_record[LW_nclash].errnc[1] = nc2;
	LW_clash_flag = UU_TRUE;
	LW_nclash++;
/*
........Output clash message
........and stop playback if requested
*/
	ul_ipv_clash_form(LW_clash_record);
}

/*********************************************************************
**	 I_FUNCTION: ul_ipv_clash_ents(prims)
**		Sets up the global clash record (LW_clash_record) depending on
**    the primtives that clashed.
**	 PARAMETERS	
**		 INPUT  : prims   = Primitives involved in clash.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_clash_ents(prims)
LtSessionPrim prims[];
{
	int i,j,k,nid,ifl;
	LW_stock_struc *sd,*sdtmp;
	LW_clash_entity itype;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	UN_cutter_list *cpt;
/*
.....Check for tool parts
*/
	cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	for (i=0;i<2;i++)
	{
		LW_clash_record[LW_nclash].ent[i] = LW_CLASH_NONE;
		for (j=0;j<LW_spindle_num;j++)
		{
			if (prims[i] == LW_tool[j])
			{
				LW_clash_record[LW_nclash].ent[i] = LW_CLASH_TOOL;
				LW_clash_record[LW_nclash].axis[i] = LW_mach_nmodel;
				LW_clash_record[LW_nclash].color[i] = LW_tool_material;
				LW_clash_record[LW_nclash].id[i] = j;
				break;
			}
			else if (prims[i] == LW_shank[j])
			{
				LW_clash_record[LW_nclash].ent[i] = LW_CLASH_SHANK;
				LW_clash_record[LW_nclash].axis[i] = LW_mach_nmodel + 2;
				LW_clash_record[LW_nclash].color[i] = cpt[LW_act_tool[j]].color[1];
				if (LW_clash_record[LW_nclash].color[i] == -1)
					LW_clash_record[LW_nclash].color[i] = LW_default_tool.shank_color;
				LW_clash_record[LW_nclash].id[i] = j;
				break;
			}
			else
			{
				for (k=0;k<LW_num_holder[j];k++)
				{
					if (prims[i] == LW_holder[j][k])
					{
						LW_clash_record[LW_nclash].ent[i] = LW_CLASH_HOLDER;
						LW_clash_record[LW_nclash].ent[i] = LW_CLASH_HOLDER;
						LW_clash_record[LW_nclash].axis[i] = LW_mach_nmodel + 1;
						LW_clash_record[LW_nclash].color[i] =
							cpt[LW_act_tool[j]].color[2];
						if (LW_clash_record[LW_nclash].color[i] == -1)
							LW_clash_record[LW_nclash].color[i] =
								LW_default_tool.hold_color;
						LW_clash_record[LW_nclash].id[i] = j;
						break;
					}
				}
			}
		}
/*
.....Check for stocks and fixtures
*/
		if (LW_clash_record[LW_nclash].ent[i] == LW_CLASH_NONE)
		{
			itype = LW_CLASH_STOCK;
			for (j=0;j<2;j++)
			{
				sd = LW_stock_first[j];
				for (k=0;k<LW_nstock[j];k++)
				{
					ifl = 0;
					do
					{
						ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
						if (ifl == -2) break;
						if (sdtmp->stock == prims[i])
						{
							LW_clash_record[LW_nclash].ent[i] = itype;
							LW_clash_record[LW_nclash].id[i] = sd->id;
							LW_clash_record[LW_nclash].axis[i] =
								LW_mach_nmodel + 10 + j * 1000 + k;
							LW_clash_record[LW_nclash].color[i] = sdtmp->color;
							break;
						}
					} while (ifl != -1);
					sd = (LW_stock_struc *)uu_lsnext(sd);
				}
				itype = LW_CLASH_FIXTURE;
			}
		}
/*
.....Check for machine components
*/
		if (LW_clash_record[LW_nclash].ent[i] == LW_CLASH_NONE && LW_mach_simul)
		{
			mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
			spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
			for (j=0;j<LW_mach_nmodel;j++)
			{
				for (k=mpt[j].beg_solid;k<=mpt[j].end_solid;k++)
				{
					if (spt[k].stock.stock == prims[i])
					{
						if (mpt[j].style == LW_MACH_HEAD)
							LW_clash_record[LW_nclash].ent[i] = LW_CLASH_HEAD;
						else
							LW_clash_record[LW_nclash].ent[i] = LW_CLASH_AXIS;
						if (i == 0)
							sprintf(LW_clash_record[LW_nclash].errmsg1,"%s-axis",
								mpt[j].axisname);
						else
							sprintf(LW_clash_record[LW_nclash].errmsg2,"%s-axis",
								mpt[j].axisname);
						LW_clash_record[LW_nclash].axis[i] = j;
						LW_clash_record[LW_nclash].color[i] = spt[k].stock.color;
						break;
					}
				}
				if (LW_clash_record[LW_nclash].ent[i] != LW_CLASH_NONE) break;
			}
		}
	}
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_write_log(buf)
**		Writes a line to the NCLIPV log file.
**	 PARAMETERS	
**		 INPUT  : buf     = Buffer to write.
**		 OUTPUT : none.
**	 RETURNS: UU_FAILURE if a write error occurred.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_write_log(buf)
char *buf;
{
int inum,stat;
/*
.....Write to the log file
*/
	stat = UU_SUCCESS;
	if (Slog_fd != UU_NULL)
	{
		stat = ux_fwrite0(buf,strlen(buf),1,Slog_fd,&inum);
		if (stat != UU_SUCCESS)
		{
			ud_wrerr("Error writing to NCLIPV log file.");
			LiFileClose(Slog_fd);
			Slog_fd = UU_NULL;
			return(stat);
		}
#if UU_COMP == UU_WIN2K
		stat = ux_fwrite0("\r\n",1,2,Slog_fd,&inum);
#else
		stat = ux_fwrite0("\n",1,1,Slog_fd,&inum);
#endif
	}
	return(stat);
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_read_log(buf)
**		Reads a line from the NCLIPV log file.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : buf     = Buffer to write.
**	 RETURNS: UU_FAILURE if a write error occurred.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_read_log(buf)
char *buf;
{
int inum,stat;
/*
.....Read from the log file
*/
	stat = UU_FAILURE;
	if (Slog_fd != UU_NULL)
	{
		stat = ul_fread(Slog_fd,buf,132,&inum);
		if (stat != UU_SUCCESS) stat = UU_FAILURE;
	}
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_rewind_log()
**		Rewinds the NCLIPV log file.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_rewind_log(buf)
{
/*
.....Rewind the log file
*/
	if (Slog_fd != UU_NULL) rewind(Slog_fd);
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_reset_log()
**		Resets (clears) the log file.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none
**	 RETURNS: UU_FAILURE if a write error occurred.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_reset_log()
{
	int status;
	UU_LOGICAL opnd;
	UX_pathname fname;
/*
.....Is the log file opened?
*/
	opnd = ul_ipv_get_diagfile(fname);
	if (opnd) ul_ipv_close_diag();
/*
.....Clear the log file
*/
	status = ul_ipv_open_diag(fname,UU_FALSE,LI_FILE_WRITE);
	if (!opnd) ul_ipv_close_diag();
	return(status);
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_reset_clash_colors()
**		Resets the materials of the last entities involved in a clash.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_reset_clash_colors()
{
	int i;
	LtData stuff;
/*
.....Reset all clash primitives materials
*/
	for (i=0;i<Sclash_count;i++)
	{
		LiDataSetGenericPtr(&stuff,LW_material[Sclash_stack[i].color]);
		LiSessionPrimSetVisualProperty(Sclash_stack[i].prim,
			LI_MW_VIS_PROP_MATERIAL,&stuff);
	}
/*
.....Reset stack
*/
	Sclash_count = 0;
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_reset_clash_stack()
**		Resets the clash colors stack.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_reset_clash_stack()
{
/*
.....Reset stack
*/
	Sclash_count = 0;
}

/*********************************************************************
**	 I_FUNCTION: S_push_clash_stack(prim)
**		Changes the color of a primitive involved in a clash and saves
**    its original color on the clash stack.
**	 PARAMETERS	
**		 INPUT  :
**        prim    = Primitive involved in clash.
**        color   = Primitive's color.
**		 OUTPUT : none
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
static void S_push_clash_stack(prim,color)
LtSessionPrim prim;
int color;
{
	int i;
	LtData stuff;
/*
.....Make sure prim is not already on stack
*/
	if (Sclash_count == LW_MAXCLASH || LW_clash_color == 0) goto done;
	for (i=0;i<Sclash_count;i++)
		if (prim == Sclash_stack[i].prim) goto done;
/*
.....Store the primitive and its color
.....in the clash stack
*/
	Sclash_stack[Sclash_count].prim = prim;
	Sclash_stack[Sclash_count].color = color;
	Sclash_count++;
/*
.....Set the color of the primitive
.....to the clash color
*/
	LiDataSetGenericPtr(&stuff,LW_material[LW_clash_color]);
	LiSessionPrimSetVisualProperty(prim,LI_MW_VIS_PROP_MATERIAL,&stuff);
/*
.....End of routine
*/
done:;
	return;
}
