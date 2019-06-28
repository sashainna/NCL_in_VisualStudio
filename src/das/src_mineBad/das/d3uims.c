/*********************************************************************
**
**    NAME         :  d3uims.c
**
**       CONTAINS:
**				ud_signon(flag)
**				ud_load_layout()
**				ud_save_layout()
**				ud_menu_design()
**				ud_open_pocket(drawing)
**				ud_close_pocket()
**				ud_open_window(line,cols)
**				ud_close_window()
**				ud_win_out (buff)
**				ud_desel_vpik()
**				ud_post_msg()
**				ud_restore_pickarea()
**				ud_store_batchmsg()
**				ud_send_nclinfo()
**				ud_getbat_info()
**				ud_setform_pocket()
**
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**
C*     MODULE NAME AND RELEASE LEVEL 
C*       d3uims.c , 26.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       09/25/18 , 10:22:27
**
*********************************************************************/
#include "udebug.h"
#include "usysdef.h"
#include "zsysdep.h"
#include	"gtbl.h"
#include "g.h"
#include "driver.h"
#include "dsubcom.h"
#include "gentry.h"
#include "usysg.h"
#include "dasnog.h"
#include "dinput.h"
#include "uims.h"
#include "mfort.h"
#include "nclfc.h"
#include "wsmf.h"
#include "gdidd.h"
#include "zkeysym.h"
#include "mpocket.h"
#include "mxxx.h"
#include "dmotif.h"
#include "lcom.h"

typedef struct
{
	int flag;
	char ppfile[256];
	int current, highest, lines;
	char macro[64];
	int warn, error;
} NCLInfo;
int NCL_com_size=0;
extern int UL_clswin_flag;
extern  char UR_dpn[];

void ferrmsg(buf,buflen)
UM_f77_str_ptr buf;
UM_int2 *buflen;
{
	int nchar;
	char msg[256];
	char *cstr;
	UM_int2 ifl, ifl35=35;
/*
.....Ignore this routine in batch
*/
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;

	cstr = UM_cstr_of_f77_str(buf);
	nchar = *buflen;
	strncpy(msg, cstr, nchar);
	msg[nchar] = '\0';
	(*(ug_gksstli.wsopen[0].connid)[UW_ERROR_MSG])(msg);
}

/*********************************************************************
**
**    E_FUNCTION:ud_signon(flag)
**       interface signon
**
**    PARAMETERS   
**       INPUT  : 
**          flag: used by UNIX before
**				= 1 if unauth should be called.
**				but now, not used
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_signon(flag)
int flag;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_SIGNON])(flag);
}

/*********************************************************************
**
**    E_FUNCTION:ud_load_layout()
**       load interface layout 
**
**    PARAMETERS   
**       INPUT  : 
**          fname  = Name of file to load, or blank if the file should
**                   be prompted for.
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int ud_load_layout(fname)
char *fname;
{
	int status, user_opened;
	if ((fname!=NULL)||(fname[0]!='\0'))
		user_opened = 0;
	else
		user_opened = 1;
	status = (*(ug_gksstli.wsopen[0].connid)[UW_LOAD_LAYOUT])(fname);
	if ((status==0)&&(user_opened))
		nclc_save_recent_file(fname, 7);
	return status;
}

/*********************************************************************
**
**    E_FUNCTION:ud_save_layout(fname)
**       save interface layout 
**
**    PARAMETERS   
**       INPUT  : 
**          fname  = Name of file to save, or blank if the file should
**                   be prompted for.
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_save_layout(fname)
char *fname;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_SAVE_LAYOUT])(fname);
}

/*********************************************************************
**
**    E_FUNCTION:ud_menu_design()
**       design menu file
**
**    PARAMETERS   
**       INPUT  : 
**          None
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_menu_design()
{
	(*(ug_gksstli.wsopen[0].connid)[UW_MENU_DESIGN])();
}
/*********************************************************************
**
**    E_FUNCTION:ud_open_pocket(drawing,type)
**       open a pocket window with drawing
**
**    PARAMETERS   
**       INPUT  : 
**          drawing: drawing to displayed in the
**			pocket window
**          type:    Type of pocket window to open.
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : status: 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
ud_open_pocket(title, drawing,type)
char *title;
int *drawing;
UM_pkwin_type type;
{
	int status;
	status = (*(ug_gksstli.wsopen[0].connid)[UW_OPEN_POCKET])(title, drawing,type);
	return status;
}

/*********************************************************************
**
**    E_FUNCTION:ud_close_pocket(type)
**       close a pocket window 
**
**    PARAMETERS   
**       INPUT  : 
**          type:    Type of pocket window to open.
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_close_pocket(type)
UM_pkwin_type type;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_CLOSE_POCKET])(type);
}
/*********************************************************************
**
**    E_FUNCTION:ud_open_window(line,cols)
**		This function opens a text window for output.
**	 PARAMETERS	
**		 INPUT  : line = # of lines in window
**			  col = # of columns in window
**			  
**		 OUTPUT : none.
**
**    RETURNS      : status: 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
ud_open_window(line,cols)
int line,cols;
{
	int status;
	status = (*(ug_gksstli.wsopen[0].connid)[UW_OPEN_WINDOW])(line,cols);
	return status;
}

/*********************************************************************
**
**    E_FUNCTION:ud_close_window()
**       Close the scrolling window 
**
**    PARAMETERS   
**       INPUT  : 
**          None
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int ud_close_window()
{
#if UU_COMP == UU_WIN2K
	if ((UDM_run_layout.statwin_type == 0) && (UL_clswin_flag == 0))
		return 0;
#endif
	return (*(ug_gksstli.wsopen[0].connid)[UW_CLOSE_WINDOW])();
}

/**************************************************************************
**
**  E_FUNCTION:  ud_win_out (buff)
**      write a string to the text scrolling window
**
**  PARAMETERS   
**      INPUT  :  buff	:	string to be written out
**
**      OUTPUT :  none
**  RETURNS      :  None
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**  NOTES:	 :  This routine was "borrowed" from ux_udos_out.
**************************************************************************/
void ud_win_out (buff)
char *buff;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_WIN_OUT])(buff);
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_desel_vpik()
**       delete select verify picking
**
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_desel_vpik()
{
#ifdef UU_OPENGL
	uw_gldesel_vpik();
#endif 
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_post_msg(int msg)
**       post a message
**
**    PARAMETERS   
**       INPUT  : 
**          msg.
**       OUTPUT :  
**          none.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_post_msg(msg)
int msg;
{
#if UU_COMP==UU_WIN2K
	uw_ntpost_msg(msg);
#else
	uw_mfpost_msg(msg); 
#endif
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_restore_pickarea()
**       restore the picking area
**
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_restore_pickarea()
{
#ifdef UU_OPENGL
	uw_glrestore_pickarea();
#endif
}
/*********************************************************************
**    E_FUNCTION     : ud_store_batchmsg(jobno, errno, warno, ldat, ltim)
**       Store the NCL batch message into common memory area
**    PARAMETERS
**       INPUT  :
**              jobno: batch file number
**				errno: error number 
**				warno: warning number 
**				ldat, ltim: date and time
**       OUTPUT :
**          ret: -1: writing failed
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_store_batchmsg(jobno, err, warn, ldat, ltim, ret)
int *jobno, *err, *warn;
char *ldat, *ltim, *ret;
{
#if UU_COMP == UU_WIN2K
	*ret = uw_ntstore_batchmsg(*jobno, *err, *warn, ldat, ltim);
#else
	*ret = uw_mfstore_batchmsg(*jobno, *err, *warn, ldat, ltim);
#endif
}

/*********************************************************************
**    E_FUNCTION     : ud_send_nclinfo()
**       Send a NCL information to NCQ
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_send_nclinfo()
{
#if UU_COMP == UU_WIN2K
	uw_ntsend_NCLinfo();
#else
	uw_mfsend_NCLinfo();
#endif
}

/*********************************************************************
**    E_FUNCTION     : ud_getbat_info(NCLInfo *info)
**       Get the NCL batch information
**    PARAMETERS
**       INPUT  :
**              info: NCL batch information
**			
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_getbat_info(info)
NCLInfo *info;
{
	int len1, len2;
	getbatchinfo (info->ppfile, &len1, info->macro, &len2, &(info->current), 
			&(info->highest), &(info->lines), &(info->warn), &(info->error));
	info->ppfile[len1] = '\0';
	info->macro[len2] = '\0';
}

/*********************************************************************
**    E_FUNCTION     : ud_setform_pocket(frm, flag)
**       Set form pocket window flag
**    PARAMETERS
**       INPUT  :
**              frm: form id
**				flag: flag to be set
**			
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_setform_pocket(frm, flag)
int frm, flag;
{
#if UU_COMP!=UU_WIN2K
	uw_mfset_frmpocket(frm, flag);
#else
	uw_ntset_frmpocket(frm, flag);
#endif
}
/*********************************************************************
**    E_FUNCTION     : ud_update_win_title
**       updated the window title
**    PARAMETERS
**       INPUT  : 
**			none
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_update_win_title()
{
	char title[500], tmpstr[500], ttmpstr[500];
	UX_pathname tmpfile, dir,fname;
	char sfile[256], numstr[20], *indx, pathstr[2], pptmp[20];
	double ver;
	int i, tmpnum, path = 1, pnum = 40, unum = 40;

	pathstr[0] = UX_PATH_SEP;
	pathstr[1] = '\0';

	getver(&ver);

	if (UD_window_title[0]=='\0')
	{
		sprintf(title,"NCL V%5.3f",ver);
		goto set_title;
	}
	strcpy(title, UD_window_title);
	strcpy(ttmpstr, title);
	strcpy(tmpstr, title);

	sprintf(pptmp, "%%PATH%s%%PARTPGM", pathstr);
	indx = (char*) strstr(tmpstr, pptmp);
	if (indx==NULL)
	{
		strcpy(tmpstr, title);
		indx = (char*) strstr(tmpstr, "%PARTPGM");
		path = 0;
	}
	if (indx!=NULL)
	{
		tmpnum = indx - tmpstr;
		strncpy(title, ttmpstr, tmpnum);
		title[tmpnum] = '\0';
		if (path==0)
			indx = indx + 8;
		else
			indx = indx + 14;
		i = 0;
		while ((*indx!='%')&&(*indx!=' ')&&(*indx!='\0')&&(*indx!='\t')
				&& (*indx!='\r') && (*indx!='\n')) 
		{
			numstr[i++] = *indx;
			indx++;
		}
		numstr[i] = '\0';
		strcpy(tmpfile,UL_program);
/*
.....if there is no %PATH in layout file, never display path info
*/
		ul_break_fname(tmpfile,dir,fname);
		if (fname[0]!='\0')
		{
			if (path==0)
			{
				strcpy(tmpfile, fname);
				strcat(tmpfile,".");
				strcat(tmpfile,UL_program_suffix);
			}
			else
			{
				if (dir[0]=='\0')
				{
#if (UU_COMP == UU_RIDGE) || (UU_COMP == UU_WIN2K)
					getcwd(dir, UX_MAX_PATH_LEN);
#else
					getwd(dir);
#endif
				}
				ul_build_full_fname(dir, fname,UL_program_suffix,tmpfile);
			}
			if (i!=0)
			{
				pnum = atoi(numstr);
/*
......short filename
*/
				ul_short_filename(tmpfile,sfile,pnum);
			}
			else
				strcpy(sfile, tmpfile);
		}
		else
			sfile[0] = '\0';
		if (UL_program[0]!='\0')
			strcat(title, sfile);
		if (*indx!='\0')
			strcat (title, indx);
	}
	strcpy(ttmpstr, title);
	strcpy(tmpstr, title);
	sprintf(pptmp, "%%PATH%s%%UNIBASE", pathstr);
	indx = (char*) strstr(tmpstr, pptmp);
	path = 1;
	if (indx==NULL)
	{
		strcpy(tmpstr, title);
		indx = (char*) strstr(tmpstr, "%UNIBASE");
		path = 0;
	}
	if (indx!=NULL)
	{
		tmpnum = indx - tmpstr;
		strncpy(title, ttmpstr, tmpnum);
		title[tmpnum] = '\0';
		if (path==0)
			indx = indx + 8;
		else
			indx = indx + 14;
		i = 0;
		while ((*indx!='%')&&(*indx!=' ')&&(*indx!='\0')&&(*indx!='\t')
				&& (*indx!='\r') && (*indx!='\n')) 
		{
			numstr[i++] = *indx;
			indx++;
		}
		strcpy(tmpfile, UR_dpn);
		ux_add_ftype("UR_UNB_FILE",tmpfile,UX_NPRTERRS);
/*
.....if there is no %PATH in layout file, never display path info
*/
		ul_break_fname(tmpfile,dir,fname);
		if (fname[0] != '\0' && UR_dpn[0] != '\0')
		{
			if (path==0)
			{
				strcpy(tmpfile, fname);
			}
			else
			{
				if (dir[0]=='\0')
				{
#if (UU_COMP == UU_RIDGE) || (UU_COMP == UU_WIN2K)
					getcwd(dir, UX_MAX_PATH_LEN);
#else
					getwd(dir);
#endif
				}
				ul_build_full_fname(dir, fname,"",tmpfile);
			}
			numstr[i] = '\0';
			if (i!=0)
			{
				pnum = atoi(numstr);
/*
......short filename
*/
				ul_short_filename(tmpfile,sfile,pnum);
			}
			else
				strcpy(sfile, tmpfile);
		}
		else
			sfile[0] = '\0';
		strcat(title, sfile);
		if (*indx!='\0')
			strcat (title, indx);
	}
	strcpy(ttmpstr, title);
	strcpy(tmpstr, title);
	indx = (char*) strstr(tmpstr, "%VERSION");
	if (indx!=NULL)
	{
		tmpnum = indx - tmpstr;
		strncpy(title, ttmpstr, tmpnum);
		title[tmpnum] = '\0';
		sprintf(numstr, "%7.2f", ver);
		strcat(title, numstr);
		if (*(indx+8)!='\0')
			strcat (title, (char*)(indx+8));
	}
	strcpy(ttmpstr, title);
	strcpy(tmpstr, title);
	indx = (char*) strstr(tmpstr, "%PROGRAM");
	if (indx!=NULL)
	{
		tmpnum = indx - tmpstr;
		strncpy(title, ttmpstr, tmpnum);
		title[tmpnum] = '\0';
		strcat(title, "NCL");
		if (*(indx+8)!='\0')
			strcat (title, (char*)(indx+8));
	}
set_title:;
	(*(ug_gksstli.wsopen[0].connid)[UW_SETWIN_TITLE])(title);
}
/*********************************************************************
**    E_FUNCTION     : ud_chk_comm(ret)
**       check if the current locate common area enough to run
**    PARAMETERS
**       INPUT  : 
**			none
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_chk_comm(ret)
int *ret;
{
#if UU_COMP!=UU_WIN2K
	*ret = uw_mfchk_comm();
#else
	*ret = uw_ntchk_comm();
#endif
}
