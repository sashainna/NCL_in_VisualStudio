/********************************************************************* 
**  NAME:  nclipvcfun.cpp
**
**			C functions which used C++ class functions
**			it is the interface between C and C++
**	CONTAINS: 
**			uw_ntmsg_box
**			uw_ntdispmsg (char *msg)
**			uw_nterror (char *msg)
**			uw_ntsctogc(POINT *pt)
**			uw_ntgctosc(POINT *pt)
**			uw_ntget_gwsize(int *wid, int *hgt)
**			uw_ntcolormap()
**			uw_ntset_pointer(int x, int y)
**			uw_ntinit_view()
**			uw_ntccwrplabel(char *label)
**			uw_ntccwrlabel(char *label)
**			uw_ntccwrerr(char *label)
**			uw_ntsetcom_focus(int focus)
**			uw_ntccset_cmdstr(char *msg)
**			uw_ntccget_cmdstr(char *cmdstr)
**			uw_ntget_filename
**			uw_ntget_dirname
**			uw_ntyesnocancel
**			uw_ntyes_or_no
**			uw_ntenable_cmd(int flag)
**			uw_ntccwrstat(int field, char* msg)
**			ncl_exit()
**			uw_ntget_ctrpos(int *x, int *y, int flag)
**			uw_ntgraph_toscrn
**			uw_ntset_curpos(int x, int y)
**			uw_ntfirst_resize_graphics()
**			uw_ntget_strsize
**			uw_ntapp_exit()
**			uw_ntcreate_font()
**			uw_ntopen_pocket()
**			uw_ntget_pockwin()
**			uw_ntget_mainwin()
**			uw_ntset_context()
**			uw_ntget_context()
**			uw_ntmin_pockwin()
**			uw_ntmd_filename
**			uw_ntset_gwcapture()
**			uw_ntrelease_capture()
**			uw_ntinsert_cmd()
**			uw_ntis_alphakey()
**			uw_nthide_menu(CToolBar *bar)
**			uw_ntprint_scr()
**			uw_ntsave_screenfile()
**			uw_ntgetDIBIPV()
**			uw_ntprintIPV()
**			ChangeDialogFont()
**			uw_ntupd_comfont()
**			uw_ntupdate_accel()
**			uw_ntcmd_sel()
**			uw_ntpost_closemsg()
**			uw_ntget_ipvsize()
**			uw_ntsctogc2()
**			uw_ntpost_msg()
**			uw_write_commsg()
**			uw_ntstore_batchmsg()
**			uw_ntyes_no_cancel()
**			uw_ntupd_input
**			uw_ntupd_cinput
**			uw_ntinsert_cmdstr
**			uw_ntis_inputfunc
**			uw_ntupd_statusfont
**			uw_ntupd_prmptfont
**			uw_ntupd_errorfont
**			uw_run_process()
**			uw_isform_active()
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nclipvcfun.cpp , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**       02/08/16 , 09:16:22
*********************************************************************/
#include "usysdef.h"
/*
.....have to include wsntstdafx.h which have more definitions we need
*/
#include "wsntstdafx.h"
#include <process.h>
#include <conio.h>
#include <winspool.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <io.h>
#include <direct.h>
#include "mpocket.h"
#include "nclipv.h"
#include "wsntdoc.h"
#include "NclipvView.h"
#include "NclipvFrm.h"
#include "wsgl.h"
#include "wsntform.h"
#include "wsntglfunc.h"
#include "wsntopt.h"
#include "wsntcfunc.h"
#include "wsntdir.h"
#include "wsntbitmap.h"
#include "lcom.h"
#include "wsglfun.h"
#include "wsntmmap.h"
#include "UxTheme.h"

int NT_cmap_changed = 0;
static UM_pkwin_type LastContext = UM_DRAWING_WINDOW;
int UU_SM_capture = 0;
extern DWORD UW_theme;

extern "C" int UD_cmap_changed ;
extern CWnd *NCL_Current_View;
extern CWnd *NCL_CurrentPKT_View;
extern CWnd *NCL_Main_View;
extern "C" UU_LOGICAL UR_changed;
extern "C" int UD_printipv;
extern int frm_done;
extern "C" char *UM_pocket_hwnd;
extern CMainFrame *NCL_MainFrame;
extern int NCL_savcmd_enable;
extern int NCL_KeyReturn;
extern "C" UW_glfont uw_glfont;
extern "C" int uz_ntcnvtnum_to_func(int num, char *func, char* parms, int flag);
extern "C" void uw_ntsetcursor(int cursorno);
extern "C" int uw_ntgetcur_cursor();
extern "C" void ul_ipv_set_context();
extern "C" UM_pkwin_type ul_ipv_get_context();
extern void uw_ntadd_menuborder(CRect *rect);
extern CControlBar* uw_ntget_bars(int bar_pos, int indx);
extern "C" int UV_dynview_active;
extern "C" int UM_swap_ipv;
extern "C" int PKx,PKy;
extern int NCL_common_open;
extern "C" int NCL_com_size;
extern CMemoryMappedFile NCL_common_file;
extern HWND NCL_cmdwin, NCL_statwin;
extern char UW_cur_screen[20];

extern "C" int uz_which_keydef(char*,int*,int*,short*);
HFONT UW_graphic_font;
extern "C" unsigned int NCL_subprocess;
CDialog *Pocket_Win[UM_MAX_POCKET] = {NULL,NULL,NULL};

extern "C" int uw_formupd_input(char *);
extern "C" int uw_ntpost_textmsg(char*);

extern "C" void uw_ntMDFileOpen(char* title, char* ntflt, int nfld, int *ifld, char **ftext,
								char **fdir, char* fnam, int *nc, char *descrip, CWnd *MainWin, int flag);
extern "C" char *ux_getenv(char *variable, int options);
extern "C" char * uu_malloc(int size );
static int S_form_active = 0;

/**********************************************************************
**    I_FUNCTION :  uw_ntmsg_box(CWnd* parent, char *title, 
**						char *msg, int flag)
**       Display a message box
**
**    PARAMETERS   
**       INPUT  : 
**          parent: parent window
**			title: title of message box
**			msg: message to display
**			flag: 1: error message
**				other number: information message
**		
**       OUTPUT :  
**          none
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntmsg_box(CWnd* parent, char *title, char *msg, int flag)
{
	int result, len;
	int sav_cursor = uw_ntgetcur_cursor();
/*
.....remove the trailling spaces, also to avoid the empty message
*/	
	len = strlen (msg);
	if (len==0)
		return;
	while ((len>0) && (msg[len-1]==' ')) len--; 
	if (len==0)
		return;
	if (parent==NULL)
	{
		CNclipvApp *app = (CNclipvApp*)AfxGetApp();
		if (app==NULL)
		{
			MessageBox(NULL, msg, title, MB_ICONINFORMATION|MB_OK);
			result = 1;
			goto done;
		}

		CWnd *MainDlg = (CWnd*)(app->GetMainWnd());
		if (MainDlg!=NULL)
			MainDlg = MainDlg->GetActiveWindow();
		if (MainDlg==NULL)
		{
			MessageBox(NULL, msg, title, MB_ICONINFORMATION|MB_OK);
			result = 1;
			goto done;
		}
		if (flag==1)
			result = MainDlg->MessageBox(msg, title, MB_ICONERROR|MB_OK);
		else
			result = MainDlg->MessageBox(msg, title, MB_ICONINFORMATION|MB_OK);
	}
	else
	{
		if (flag==1)
			result = parent->MessageBox(msg, title, MB_ICONERROR|MB_OK);
		else
			result = parent->MessageBox(msg, title, MB_ICONINFORMATION|MB_OK);
	}
	uw_ntsetcursor(sav_cursor);
done:;
	return;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntdispmsg (char *msg)
**       Display a information message box
**
**    PARAMETERS   
**       INPUT  : 
**			msg: message to display
**		
**       OUTPUT :  
**          none
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntdispmsg (char *msg)
{
	uw_ntmsg_box(NULL, "Ncl Message", msg, 0);
}

/**********************************************************************
**    I_FUNCTION :  uw_nterror (char *msg)
**       Display a error message box
**
**    PARAMETERS   
**       INPUT  : 
**			msg: message to display
**		
**       OUTPUT :  
**          none
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_nterror (char *msg)
{
	if (ud_is_playback())
	{
		uw_ntprmerr(msg);
		return;
	}
	uw_ntmsg_box(NULL, "Ncl Error", msg, 1);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntsctogc(POINT *pt)
**       Convert screen position to graphic client postion
**
**    PARAMETERS   
**       INPUT  : 
**          pt: screen position
**			
**       OUTPUT :  
**          pt: graphic client position
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntsctogc(POINT *pt)
{
	CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
	CNclipvView * graphic_view = (CNclipvView *)(MainDlg->GetActiveView());
	graphic_view->ScreenToClient(pt);
}
/**********************************************************************
**    I_FUNCTION : uw_ntgctosc(POINT *pt)
**       Convert graphic client postion to screen position
**
**    PARAMETERS   
**       INPUT  : 
**          pt: graphic client position
**			
**       OUTPUT :  
**          pt: screen position
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntgctosc(POINT *pt)
{
	CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
	CNclipvView * graphic_view = (CNclipvView *)(MainDlg->GetActiveView());
	graphic_view->ClientToScreen(pt);
	pt->y = (graphic_view->m_oldRect).bottom - pt->y;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntget_gwsize(int *wid, int *hgt)
**       Get graphic window size
**
**    PARAMETERS   
**       INPUT  : none
**			
**       OUTPUT :  
**          wid: graphic window width
**			hgt: graphic window height
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntget_gwsize(int *wid, int *hgt)
{
	CNclipvView * graphic_view = NULL;
	if (frm_done)
	{
		CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
		if (MainDlg!=NULL)
			graphic_view = (CNclipvView *)(MainDlg->GetActiveView());
	}
	else
		graphic_view = (CNclipvView *)NCL_Current_View;

	if (graphic_view!=NULL)
	{
		*hgt = (graphic_view->m_oldRect).bottom;
		*wid = (graphic_view->m_oldRect).right;
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_ntcolormap()
**       Set openGL color map for graphic window
**
**    PARAMETERS   
**       INPUT  : 
**          none
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntcolormap()
{
	CNclipvView * graphic_view = (CNclipvView * )NCL_Current_View;
	if (graphic_view!=NULL)
		graphic_view->inicolormap();
}

/**********************************************************************
**    I_FUNCTION :  uw_ntset_pointer(int x, int y)
**       Set cursor at client position (x,y)
**
**    PARAMETERS   
**       INPUT  : 
**          x,y: client position at which to set cursor
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntset_pointer(int x, int y)
{
	POINT pt;
	CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
	if (MainDlg==NULL) return;
	CNclipvView * graphic_view = (CNclipvView *)(MainDlg->GetActiveView());
	if (graphic_view==NULL) return;
	pt.x = x;
	pt.y = y;
	graphic_view->ClientToScreen(&pt);
	SetCursorPos(pt.x, pt.y);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntinit_view()
**		initial graphic view
**
**    PARAMETERS   
**       INPUT  : 
**          none
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntinit_view()
{
	CNclipvView * graphic_view = (CNclipvView *)NCL_Current_View;

	if (graphic_view==NULL) return;
	graphic_view->Init();
}

/**********************************************************************
**    I_FUNCTION :  uw_ntccwrplabel(char *label)
**		write a label to command line prompt
**
**    PARAMETERS   
**       INPUT  : 
**          label: label to write
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntccwrplabel(char *label)
{
	CMainFrame *MainWin = (CMainFrame*)(AfxGetMainWnd());
	if (MainWin!=NULL)
		MainWin->WritePLabel(label);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntccwrlabel(char *label)
**		write a label to promptBar
**
**    PARAMETERS   
**       INPUT  : 
**          label: label to write
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntccwrlabel(char *label)
{
	CMainFrame *MainWin = (CMainFrame*)(AfxGetMainWnd());
	if (MainWin!=NULL)
		MainWin->WriteLabel(label);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntccwrerr(char *label)
**		write a label to errorBar
**
**    PARAMETERS   
**       INPUT  : 
**          label: error to write
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntccwrerr(char *label)
{
	CMainFrame *MainWin = (CMainFrame*)(AfxGetMainWnd());
	if (MainWin!=NULL)
		MainWin->WriteError(label);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntsetcom_focus(int focus)
**		Set the command line focus
**
**    PARAMETERS   
**       INPUT  : 
**          focus: 1: set focus to command line
**					0: remove focus to command line
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntsetcom_focus(int focus)
{
	CMainFrame *MainWin = (CMainFrame*)(AfxGetMainWnd());
	if (MainWin!=NULL)
		MainWin->SetCommand_focus(focus);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntccset_cmdstr(char *msg)
**		Set the command line text
**
**    PARAMETERS   
**       INPUT  : 
**          msg: text to set into command line
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntccset_cmdstr(char *msg)
{
	CMainFrame *MainWin = (CMainFrame*)(AfxGetMainWnd());
	if (MainWin!=NULL)
		MainWin->SetCommand_Str(msg);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntccget_cmdstr(char *cmdstr)
**		Get the command line text
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          cmdstr:
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntccget_cmdstr(char *cmdstr)
{
	CString rString;
	int line, upload;
	CMainFrame *MainWin = (CMainFrame*)(AfxGetMainWnd());
	if (MainWin==NULL) return;
	MainWin->GetCommand_Str(rString, line, upload);
	int i, len = rString.GetLength();

	for (i=0; i<len; i++)
		cmdstr[i] = rString[i];
	cmdstr[i] = '\0';
/*
.....remove last '\r\n'
*/
	if (i>=2)
	{
		if (cmdstr[i-2]=='\r')
			cmdstr[i-2] = '\0';
	}
}
/**********************************************************************
**    I_FUNCTION : uw_ntget_filename(CWnd* parent, char *title, LPCTSTR filter, char* fnam, int *nc)
**			popup a file browser dialog box 
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent window
**				title:  file browser title
**				filter: file filter.
**				fname:  default file name
**				nc:     number of character of filename
**			descript = File type description
**				open_flag: TRUE: open file browser as "Open"
**							FALSE: open file browser as "SAve as"
**       OUTPUT :  
**				fname:  use enter file name
**				nc:     number of character of filename
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntget_filename2(CWnd* parent, char *title, char * filter, char* fnam, int *nc, char *descrip, int open_flag)
{
	DWORD dwFlags;
	UX_pathname ntflt, tempfilt, tempdesp, lfilter, ldescript, save_dir,def_dir, fnam_temp, dir, fnam_only;
	char *fltpt, *desppt;
	char* temp, *indx, first_exe[80];
	CString FileName ;
	int i,j, status,win_enable;
	dwFlags = OFN_NOCHANGEDIR | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_ENABLESIZING;
	CFileDialog *filedlg = NULL;
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
/*
.....Initialize routine
*/
	status = 0;
	win_enable = 0;
/*
.....save current directory in case filebrowser change it
*/
	GetCurrentDirectory(UX_MAX_PATH_LEN, save_dir);
/*
.....Get parent window
*/
	if (parent != NULL)
		MainWin = parent;
	else
	{
		MainWin = (CMainFrame *)(AfxGetMainWnd());
		if (Pocket_Win[UM_IPV_WINDOW] != NULL)
			MainWin = Pocket_Win[UM_IPV_WINDOW];
	}
/*
.....save current directory in case filebrowser change it
*/
	GetCurrentDirectory(UX_MAX_PATH_LEN, save_dir);
	int sav_cursor = uw_ntgetcur_cursor();
/*
.....the filter may include directory
.....remove directory
*/
	strcpy_s(def_dir, UX_MAX_PATH_LEN, save_dir);
	indx = strrchr(filter, '\\');
	if (indx!=0)
	{
		strcpy_s(lfilter, UX_MAX_PATH_LEN, indx+1);
		strcpy_s(save_dir, UX_MAX_PATH_LEN, filter);
		indx = strrchr(save_dir, '\\');
		indx++; *indx = '\0';
	}
	else
		strcpy_s(lfilter, UX_MAX_PATH_LEN, filter);

	strcpy_s(ldescript, UX_MAX_PATH_LEN, descrip);

	ntflt[0]= '\0';
	first_exe[0]= '\0';
	i = 0;
	while(i<100)
	{
		fltpt = strchr(lfilter, '|');
		if (fltpt!=NULL)
		{
			*fltpt = '\0';
			fltpt++;
		}
		strcpy_s(tempfilt, UX_MAX_PATH_LEN, lfilter);
		desppt = strchr(ldescript, '|');
		if (desppt!=NULL)
		{
			*desppt = '\0';
			desppt++;
		}
		strcpy_s(tempdesp, UX_MAX_PATH_LEN, ldescript);
		strcat_s(ntflt, tempdesp);
		strcat_s(ntflt, "|");
/*
.....replace "," with ";"
*/
		for (j=0; j<(int)strlen(tempfilt); j++)
		{
			if (tempfilt[j]==',')
				tempfilt[j] = ';';
		}
		if (i==0)
			strcpy(first_exe, tempfilt);
		strcat_s(ntflt, tempfilt);
		if ((fltpt==NULL)||(desppt==NULL))
			break;
		strcat_s(ntflt, "|");
		strcpy_s(lfilter, UX_MAX_PATH_LEN, fltpt);
		strcpy_s(ldescript, UX_MAX_PATH_LEN, desppt);
		i++;
	}
	strcat_s(ntflt, "|");
/*
.....Added *.* for all files. But first check if filter="*.*",
.....if yes, no need to add "*.*"
*/
	if (strcmp(filter, "*.*")!=0)
	{
		strcat_s(ntflt, "All Files (*.*)|*.*|");
	}
	strcat_s(ntflt, "|");
/*
.....Enable dialog
*/
	if (MainWin!=NCL_MainFrame)
	{
		win_enable = NCL_MainFrame->IsWindowEnabled();
		if (win_enable)
		{
			::EnableWindow(NCL_MainFrame->GetSafeHwnd(), FALSE);
		}
	}
/*
......we need enable it in order to orginal for COM common dialog to working
*/
	SetThemeAppProperties(UW_theme);
/*
.....Create new dialog
*/
	filedlg = new CFileDialog(open_flag, NULL, fnam, dwFlags,
		ntflt, MainWin);
	filedlg->m_ofn.lpstrTitle = title;
	filedlg->m_ofn.nMaxFileTitle = strlen(title);
/*
......for Window 7, the initial directory act different from XP/2000/Vista
......it can't work for set here, it must assign to init filename
......so we changed here
	if (UW_browse_dir == 0) filedlg->m_ofn.lpstrInitialDir = save_dir;
*/
	if ((fnam!=NULL)&&(fnam[0]!='\0'))
	{
		strcpy(fnam_temp, fnam);
		ul_break_fname(fnam_temp, dir, fnam_only);
	}
	else
	{
/*
.....use the first file type if file extension is not giveing
*/
		dir[0] = '\0';
		if (first_exe[0]!='\0')
		{
			strcpy(fnam_only, first_exe);
		}
		else
			strcpy(fnam_only, "*.*");
	}
	if (dir[0]=='\0')
	{
/*
.....added save_dir
*/
		strcpy(fnam_temp, save_dir);
		strcat(fnam_temp, "\\");
		strcat(fnam_temp, fnam_only);
		if (UW_browse_dir == 0) filedlg->m_ofn.lpstrFile = fnam_temp;
	}
/*
.....Get dialog input
*/
	if (filedlg->DoModal()==IDCANCEL)
	{
		*nc = 0;
		fnam[0] = '\0';
		ud_brower_endrprd(fnam, 0);
		goto done;
	}
	FileName = filedlg->GetPathName();
	*nc = FileName.GetLength();
	temp = FileName.GetBuffer(*nc);
	strcpy_s(fnam, UX_MAX_PATH_LEN, temp);
	ud_brower_endrprd(fnam, *nc);
done:;
	if (filedlg!=NULL)
		delete filedlg;
	if (win_enable)
		::EnableWindow(NCL_MainFrame->GetSafeHwnd(), TRUE);
	_chdir(save_dir);
	uw_ntsetcursor(sav_cursor);
/*
......reset back
*/
	SetThemeAppProperties(0);
}

/**********************************************************************
**    I_FUNCTION : uw_ntget_dirname(CWnd* parent, char *title, char* pnam, int *nc)
**			popup a file browser dialog box 
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent window
**				title:  file browser title
**				pname:  default path name
**				nc:     number of character of pname
**       OUTPUT :  
**				pname:  use enter path name
**				nc:     number of character of pname
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntget_dirname(CWnd* parent, char *title, char* pnam, int *nc, char *paths, char *path_des)
{
	DWORD dwFlags;
	UX_pathname ntflt,save_dir;
	char* temp;
	CString FileName ;
	int status,win_enable;
	dwFlags = OFN_NOCHANGEDIR | OFN_ENABLETEMPLATE | OFN_NONETWORKBUTTON | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT | OFN_ENABLESIZING; 
	CNCLDirDialog *filedlg = NULL;
	status = 0;
	win_enable = 0;
/*
.....save current directory in case filebrowser change it
*/
	GetCurrentDirectory(UX_MAX_PATH_LEN, save_dir);
	int sav_cursor = uw_ntgetcur_cursor();

	strcpy_s(ntflt, "All Files (*.*)|*.*||");
	
	if ((pnam==NULL)||(pnam[0]=='\0'))
	{
		strcpy_s(pnam, UX_MAX_PATH_LEN, save_dir);
	}

	if (parent!=NULL)
	{
		filedlg = new CNCLDirDialog(TRUE, NULL, pnam, dwFlags,
			ntflt, parent);
		filedlg->m_ofn.lpstrTitle = title;
		if (parent!=NCL_MainFrame)
		{
			win_enable = NCL_MainFrame->IsWindowEnabled();
			if (win_enable)
			{
				::EnableWindow(NCL_MainFrame->GetSafeHwnd(), FALSE);
			}
		}
	}
	else
	{
		CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());

		filedlg = new CNCLDirDialog(TRUE, NULL, pnam, dwFlags,
			ntflt, MainWin);
		filedlg->m_ofn.lpstrTitle = title;
		if (MainWin!=NCL_MainFrame)
		{
			win_enable = NCL_MainFrame->IsWindowEnabled();
			if (win_enable)
			{
				::EnableWindow(NCL_MainFrame->GetSafeHwnd(), FALSE);
			}
		}
	}
	filedlg->m_ofn.lpTemplateName = MAKEINTRESOURCE (IDD_DIROPEN_DLG);
	filedlg->m_ofn.Flags = OFN_NOCHANGEDIR | OFN_NONETWORKBUTTON | OFN_LONGNAMES | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT | OFN_ENABLEHOOK | OFN_ENABLETEMPLATE | OFN_ENABLESIZING; 
	if (filedlg->DoModal()==IDCANCEL)
	{
		*nc = 0;
		pnam[0] = '\0';
		goto done;
	}
	FileName = filedlg->GetPathName();
	*nc = FileName.GetLength();
	temp = FileName.GetBuffer(*nc);
	strcpy_s(pnam, UX_MAX_PATH_LEN, temp);
done:;
	if (filedlg!=NULL)
		delete filedlg;
	if (win_enable)
		::EnableWindow(NCL_MainFrame->GetSafeHwnd(), TRUE);
	_chdir(save_dir);
	uw_ntsetcursor(sav_cursor);
}
static int S_extend_path(char *extstr, char *paths, char *sub)
{
	char tempstr[UX_MAX_PATH_LEN*20], ext_dir[UX_MAX_PATH_LEN], dir[UX_MAX_PATH_LEN],
		dir1[UX_MAX_PATH_LEN], extstr2[UX_MAX_PATH_LEN*20], *svalue;
	char *indx, *tmp, *str, *last;
	int i, k, mode, status=0,ext=0;
	strcpy(tempstr, paths);

	status = ux_search_for_path(tempstr, ext_dir, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
	if (status==UU_SUCCESS)
	{
		tmp = strchr(ext_dir, '%');
		if (tmp==NULL)
		{
			strcpy(extstr, ext_dir);
			if (sub!=NULL)
			{
				strcat(extstr, "\\");
				strcat(extstr, sub);
			}
			return 0;
		}
		else
			strcpy(tempstr, ext_dir);
	}
	extstr[0] = '\0';
/*
.....the paths can be a "format of dir1;dir2;dir3;..."
.....so we get directory one by one
*/
	
	str = strtok(tempstr, ";");
	last = str + strlen(str) + 1;
	while (str!=NULL)
	{
		strcpy(dir, str);
/*
.....remove pre or trailing spaces
*/
		k=0;
		while (dir[k]==' ') k++;
		strcpy(dir, &(dir[k]));
/*
......Remove trailing spaces
*/
		for (k=strlen(dir); k>0; k--)
		{
			if (dir[k-1]==' ')
				dir[k-1] = '\0';
			else
				break;
		}
		mode = UX_EXISTS | UX_READ;
		status = ux_search_for_path(dir, ext_dir, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
		if (status==UU_SUCCESS)
		{
			strcat(extstr, ext_dir);
			if (sub!=NULL)
			{
				strcat(extstr, "\\");
				strcat(extstr, sub);
			}
		}
		else
		{
/*
.....could be a value with dir1;dir2;dir3 defined
*/
			if (dir[0]=='%')
			{
				strcpy(dir1, &(dir[1]));
			}
			else
				strcpy (dir1, dir);
			svalue = ux_getenv(dir1, UX_NPRTERRS|UX_NQUOTES);
			if (svalue != UU_NULL)
			{
//				strcat (extstr, svalue);
				ext = S_extend_path(extstr2, svalue, sub); 
				strcat(extstr, extstr2);
			}
			else
			{
				indx = strchr (dir1, '\\');
				if ((dir[0]=='%')&&(indx!=NULL))
					*indx = 0;
				ext = S_extend_path(extstr2, dir1, (char*)(indx+1)); 
				strcat(extstr, extstr2);
			}
		}
next:;
		str = strtok(last, ";");
		if (str!=NULL)
		{
			strcat (extstr, ";");
			last = str + strlen(str) + 1;
			ext = 1;
		}
	}
	tmp = strchr (extstr, '%');
	if (tmp==NULL)
		return ext;
	strcpy(extstr2, extstr);
	return S_extend_path(extstr, extstr2, NULL); 
}

static void S_add_ftext_str(char **fdir, char **ftext, int fld)
{
	char *str;
	char tempstr[UX_MAX_PATH_LEN];
	int i, start_c, len, n = fld;

	strcpy(tempstr, fdir[fld]);
	str = strrchr (tempstr, '\\');
	if (str==NULL)
	{
		for (i=0; i<n; i++)
		{
			if (strcmp(ftext[i], tempstr)==0)
			{
/*
.....in this case, we have to change the original ftext[i] to add more path
*/
				len = strlen (fdir[i]);
				start_c = len - 40;
				if (start_c<0) start_c = 0;
				strcpy(ftext[i], &(fdir[i][start_c]));
				continue;
			}
		}
		len = strlen (tempstr);
		start_c = len - 40;
		if (start_c<0) start_c = 0;
		strcpy(ftext[n], &(tempstr[start_c]));
	}
	else
	{
		strcpy(tempstr, (char*)(str+1));
		for (i=0; i<n; i++)
		{
			if (strcmp(ftext[i], tempstr)==0)
			{
				strcpy(ftext[n], fdir[n]);
				return;
			}
		}
		len = strlen (tempstr);
		start_c = len - 40;
		if (start_c<0) start_c = 0;
		strcpy(ftext[n], &(tempstr[start_c]));
	}
}
/****************************************************************
**	E_FUNCTION: uwx_get_flist(char *pathname, int farea_flag, char *ftype, char **flist, int options)
**			Attempts to construct a list of all files of UNICAD
**			type "ftype" in the file area, "pathname".
**		PARAMETERS:
**			"pathname": A full quoted system dependent path name 
**				to the file area containing the files to be listed.
**			"farea_flag": indicates files or file area to be listed.
**			"ftype": UNICAD file type to be listed; UX_ALL indicates
**				that all files in the area will be listed; may be a
**				symbol or a quoted suffix string.
**			"flist": returns a descriptor to the list structure
**				for listing files, UU_NULL if an empty list and no 
**				files were found to have this ftype extension.
**			"options": A bitwise "OR" of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					UX_CHK: check the syntax of the path name.
**					UX_NCHK: don't check the syntax of the path
**						name.
**					If incorrectly specified we will print errors and not
**					check path name syntax.
**		RETURNS:
**			UU_SUCCESS is returned if the list structure is filled
**			in; otherwise, return -1
****************************************************************/
extern "C" int uwx_get_flist(char *pathname, int farea_flag, char *ftype, char **flist, int options)
{
	char sav_dir[UX_MAX_PATH_LEN], fext[UX_SUFFIX_LEN];
	_getcwd(sav_dir, UX_MAX_PATH_LEN);

	ul_remove_quotes(pathname);
	strcpy(fext, "*.");
	strcat(fext, ftype);
	if (_access(pathname, 0)!=0)
		return -1;
	
	_chdir(pathname);
	CFileFind finder;
	char *file;
    BOOL stat = finder.FindFile(fext);

	while (stat)
	{
		stat = finder.FindNextFile();
		CString file1 = finder.GetFilePath();
		file = file1.GetBuffer(UX_MAX_PATH_LEN);
		*flist = uu_lsinsrt(*flist,UX_MAX_PATH_LEN);
		uu_move_byte(file, *flist, UX_MAX_PATH_LEN);
	}
	_chdir(sav_dir);
	return 0;
}
/**********************************************************************
**    I_FUNCTION : uw_ntyesnocancel(CWnd *parent, char* msg, char *title)
**			popup a YesNoCancel question dialog box and get answer
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent window
**				title:  dialog box title
**				msg:    dialog str.
**       OUTPUT :  
**         			none 
**    RETURNS      : 0:No
**					 1: Yes
**					-1: Cancel
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/

extern "C" int uw_ntyesnocancel(CWnd *parent, char* msg, char *title)
{
	int result;
	int sav_cursor = uw_ntgetcur_cursor();

	if (parent==NULL)
	{
		CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
		if (MainWin==NULL)
			result = MessageBox(NULL, msg, title, MB_YESNOCANCEL);
		else
			result = MainWin->MessageBox(msg, title, MB_YESNOCANCEL);
	}
	else
		result = parent->MessageBox(msg, title, MB_YESNOCANCEL);
	uw_ntsetcursor(sav_cursor);
	if (result==IDYES)
		return 1;
	else if (result==IDNO)
		return 0;
	else
		return -1;
}

/**********************************************************************
**    I_FUNCTION : uw_ntyes_or_no(CWnd *parent, char* msg, char *title)
**			popup a YesNo question dialog box and get answer
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent window
**				title:  dialog box title
**				msg:    dialog str.
**       OUTPUT :  
**         			none 
**    RETURNS      : 0:No
**					 1: Yes
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/

extern "C" int uw_ntyes_or_no(CWnd *parent, char* msg, char *title)
{
	int result;
	int sav_cursor = uw_ntgetcur_cursor();

	if (parent==NULL)
	{
		CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
		if (MainWin==NULL)
			result = MessageBox(NULL, msg, title, MB_YESNO);
		else
			result = MainWin->MessageBox(msg, title, MB_YESNO);
	}
	else
		result = parent->MessageBox(msg, title, MB_YESNO);
	uw_ntsetcursor(sav_cursor);
	if (result==IDYES)
		return 1;
	else
		return 0;
}

/**********************************************************************
**    I_FUNCTION : uw_ntenable_cmd(int flag)
**			enable/disable command bar
**			
**    PARAMETERS   
**       INPUT  : 
**				flag: 1: enable command line
**					  0: disable command line
**				
**       OUTPUT :  
**         			none 
**    RETURNS      : None
**					 
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntenable_cmd(int flag)
{
	CMainFrame *MainWin = (CMainFrame*)(AfxGetMainWnd());
	if (MainWin!=NULL)
		MainWin->Enable_cmdbar(flag);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntccwrstat(field,msg)
**       Write a message to a field in the Status Area.
**    PARAMETERS   
**       INPUT  : 
**          field   = Field number to write to.
**			msg     = Message to output.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntccwrstat(int field, char* msg)
{
	CMainFrame *MainWin = (CMainFrame*)(AfxGetMainWnd());
	if (MainWin!=NULL)
		MainWin->WriteStat(field, msg);
}
/**********************************************************************
**    I_FUNCTION :  uw_ntget_ctrpos(int *x, int *y, int flag)
**       Get center position of graphic area.
**    PARAMETERS   
**       INPUT  : 
**          flag: 0: graphic window center position
**				  1: IPV Window center position
**       OUTPUT :  
**          x:	center pos.x
**			y:  center pos.y
**    RETURNS      : -1 if no window yet
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntget_ctrpos(int *x, int *y, int flag)
{
	POINT pt;
	CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
	if (MainDlg==NULL) return -1;
	CNclipvView * graphic_view = (CNclipvView *)(MainDlg->GetActiveView());

	pt.y = (graphic_view->m_oldRect).bottom/2;
	pt.x = (graphic_view->m_oldRect).right/2;
	if (graphic_view==NULL) return -1;
	graphic_view->ClientToScreen(&pt);
	*x = pt.x;
	*y = pt.y;
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntgraph_toscrn(int xin, int yin, int *x, int *y)
**       Convert graphic client postion to screen position
**
**    PARAMETERS   
**       INPUT  : 
**          xin: Client pos.x
**			yin: Client pos.y
**       OUTPUT :  
**          x:	screen pos.x
**			y:  screen pos.y
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntgraph_toscrn(int xin, int yin, int *x, int *y)
{
	POINT pt;
	CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
	if (MainDlg==NULL) return;
	CNclipvView * graphic_view = (CNclipvView *)(MainDlg->GetActiveView());
	if (graphic_view==NULL) return;
	pt.y = (graphic_view->m_oldRect).bottom - yin;
	pt.x = xin;
	graphic_view->ClientToScreen(&pt);
	*x = pt.x;
	*y = pt.y;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntset_curpos(int x, int y)
**       Set current cursor position at (x, y)
**
**    PARAMETERS   
**       INPUT  : 
**          x, y: pos.x and pos.y
**			
**       OUTPUT :  
**          None
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntset_curpos(int x, int y)
{
	SetCursorPos(x, y);
}

class CDisplayIC : public CDC
{
public:
	CDisplayIC() { CreateIC(_T("display"), NULL, NULL, NULL); }
};

/**********************************************************************
**    I_FUNCTION :  uw_ntget_avrchr_size(int pt, char *fntname, int *wid, int *hgt)
**       Get average size of a font on Main window
**
**    PARAMETERS   
**       INPUT  : 
**          pt: font size
**			fntname: font name
**			
**       OUTPUT :  
**          wid, hgt: average size of the font
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntget_avrchr_size(int pt, char *fntname, int *wid, int *hgt)
{
	CFont aFont;
	aFont.CreatePointFont(pt,fntname, NULL);
	CWnd *picctl = NCL_MainFrame;
	if (picctl==NULL)
	{
		*wid = 4;
		*hgt = 8;
		return;
	}
	CClientDC dc(picctl);

	CFont* savfont = dc.SelectObject(&aFont );

	CSize sizeText = dc.GetTextExtent("Xx",2);
	*wid = sizeText.cx/2;
	*hgt = sizeText.cy;

	dc.SelectObject(&savfont);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntadj_strsize(char *string, int pt, char *fntname, int wid, int hgt)
**       Get string text size
**
**    PARAMETERS   
**       INPUT  : 
**			string: string to be calculated
**          pt: font size
**			fntname: font name
**			
**       OUTPUT :  
**          wid, hgt: size of the text string
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" SIZE uw_ntadj_strsize(char *string, int pt, char *fntname, int wid, int hgt)
{
	CFont aFont;
	CWnd *picctl = NCL_MainFrame;
	CClientDC dc(picctl);
	aFont.CreatePointFont(pt,fntname, &dc);
	SIZE size;

	if (strlen(string)==0)
	{
		size.cx = wid;
		size.cy = hgt;
		return size;
	}
	CFont* savfont = dc.SelectObject(&aFont );

	CSize sizeText = dc.GetTextExtent("XXXXXXXXXXxxxxxxxxxx",20);
	double basex = ((double)sizeText.cx)/20;
	double basey = (double)sizeText.cy;

	size.cx = (long)((wid*4)/basex);
	size.cy = (long)((hgt*8)/basey);

	if (strlen(string)>=20)
	{
		size.cx = size.cx + 11;
	}
	else
	{
		size.cx = size.cx + strlen(string)/2 + 1;
	}
	size.cy = size.cy + 2;	

	dc.SelectObject(&savfont);
	return size;
}


/**********************************************************************
**    I_FUNCTION :  uw_ntget_strsize(char *string, int pt, 
**						char *fntname, int *wid, int* hgt)
**       Get the text string width and height in Logic unit
**
**    PARAMETERS   
**       INPUT  : 
**          string: text string
**			pt:     text string point
**			fntname: font name
**			
**			
**       OUTPUT :  
**          wid:  text string width
**			hgt:  text string height
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntget_strsize(char *string, int pt, char *fntname, int *wid, int* hgt)
{
	CFont aFont;
	CWnd *picctl = NCL_MainFrame;
	CClientDC dc(picctl);
	aFont.CreatePointFont(pt,fntname, &dc);

	if (string[0]=='\0')
	{
		*wid = 0;
		*hgt = 0;
		return 0;
	}

	CFont* savfont = dc.SelectObject(&aFont );

	CString itemtext = ( LPCTSTR)string;
	CSize sizeText = dc.GetTextExtent(itemtext,itemtext.GetLength());
	*wid = sizeText.cx;
	*hgt = sizeText.cy;
	SIZE size = uw_ntadj_strsize(string, pt, fntname, *wid, *hgt);
	*wid = size.cx;
	*hgt = size.cy;

	dc.SelectObject(&savfont);
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntget_strscsize(char *string, int pt, 
**						char *fntname, int *wid, int* hgt)
**       Get the text string width and height in Screen cordinate
**
**    PARAMETERS   
**       INPUT  : 
**          string: text string
**			pt:     text string point
**			fntname: font name
**			
**			
**       OUTPUT :  
**          wid:  text string width
**			hgt:  text string height
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntget_strscsize(char *string, int pt, char *fntname, int *wid, int* hgt)
{
	CFont aFont;
	CWnd *picctl = NCL_MainFrame;
	CClientDC dc(picctl);
	aFont.CreatePointFont(pt,fntname, &dc);

	if (string[0]=='\0')
	{
		*wid = 0;
		*hgt = 0;
		return 0;
	}

	CFont* savfont = dc.SelectObject(&aFont );

	CString itemtext = ( LPCTSTR)string;
	CSize sizeText = dc.GetTextExtent(itemtext,itemtext.GetLength());
	*wid = sizeText.cx;
	*hgt = sizeText.cy;

	dc.SelectObject(&savfont);
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntcreate_font(int which)
**       Create openGL font for select window
**    PARAMETERS   
**       INPUT  : 
**          which: selected window type: 0: main Window
**											1: pocket window
**										-1: font for both windows
**       OUTPUT :  
**          none
**		
**    RETURNS      0: no font craeted
**					1: font created
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntcreate_font(int which)
{
	HDC hdc;
	CWnd *current_view = NULL;
	
	current_view = (CNclipvView *)NCL_Main_View;
	if (current_view==NULL)
		return 0;

	if (uw_gltext_base==0)
	{
		if ((uw_gltext_base = glGenLists_d(256))==0)
		{
			uw_ntdispmsg ("No font today");
			return 0;
		}
	}

	CDC* dc = current_view->GetDC( );
	hdc = dc->GetSafeHdc();
	UW_graphic_font = CreateFont(UW_label_size[0], UW_label_size[1],0,0,400, FALSE, FALSE, FALSE, 
				DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
				DEFAULT_QUALITY, FF_MODERN, NULL);
//				DEFAULT_QUALITY, DEFAULT_PITCH, NULL);
	SelectObject(hdc, UW_graphic_font);
	wglUseFontBitmaps(hdc, 0, 256, uw_gltext_base);
/*
......all label in graphic pprint cap letter, so use cap letter when
......calculate wid/hgt, also use double instead int
*/
	double wid, hgt;
	CSize sizeText = dc->GetTextExtent("ABCDEFGHIJKLMNOPQRSTUVWXYZ",26);
	POINT fsize;
	fsize.x = (long)sizeText.cx;
	fsize.y = sizeText.cy;
	dc->LPtoDP(&fsize);

	wid = (double)((double)fsize.x)/26.0;
	hgt = (double)fsize.y;

	ABCFLOAT abcf;
	GetCharABCWidthsFloat(hdc, 1, 1, &abcf);

	uw_glfont.chrpwid = wid;
	uw_glfont.chrphgt = hgt;
	uw_glfont.chrwid = (float)(uw_glfont.chrpwid) / uw_gl.xpixels;
	uw_glfont.chrhgt = (float)(uw_glfont.chrphgt) / uw_gl.ypixels;
	uw_glfont.poffset = (int) abcf.abcfA;
	uw_glfont.offset = (float)(uw_glfont.poffset)/ uw_gl.xpixels;
	current_view->ReleaseDC(dc);
	return 1;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntget_mainwin(win)
**       Returns the window ID of the main graphics window.
**
**    PARAMETERS   
**       INPUT  : 
**          win      = Window ID of main graphics window.
**						
**       OUTPUT :  
**          none
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntget_mainwin(char **win)
{
	CNclipvView * graphic_view;
	graphic_view = (CNclipvView *)NCL_Main_View;
	if (graphic_view!=NULL)
		*win = graphic_view->GetWin();
	else
		*win = NULL;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntset_context()
**       Sets the graphic context to the requested window.
**
**    PARAMETERS   
**       INPUT  : 
**          which  = Ignored for now.
**                   
**
**          force  = UU_TRUE - Set context even if it was last one set.
**						
**       OUTPUT :  
**          none
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntset_context(UM_pkwin_type which,UU_LOGICAL force)
{
	CNclipvView * graphic_view;
/*
.....Only change the context if different
*/
	if (which != LastContext || force)
	{
		graphic_view = (CNclipvView *)NCL_Main_View;
		if (graphic_view==NULL) return;
		graphic_view->SetContext();
		graphic_view->SetActiveWindow();
	}
/*
.....Save this context setting
*/
	LastContext = which;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntget_context()
**       Returns the current graphic context.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**		
**    RETURNS      : 
**          UM_DRAWING_WINDOW - Pocket Drawing window.
**          UM_IPV_WINDOW - NCLIPV window.
**          UM__WINDOW - NCL main window.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" UM_pkwin_type uw_ntget_context()
{
/*
.....Return current graphic context
*/
	return(LastContext);
}

/**********************************************************************
**    I_FUNCTION : uw_ntmd_filename(parent,title,filter,nfld,
**							ftext,fdir,filename,nc)
**       Opens a File Selection dialog and returns the user selected
**			filename. This function is similar to uw_ntget_filename
**			but add radio box for select directory
**    PARAMETERS   
**       INPUT  : 
**          title     = Title of File Selection dialog.
**			filter    = Filename filter to use for list of available
**				            files.
**			nfld      = Number of directory toggle fields.
**			ifld      = Default directory toggle field.
**			ftext     = Text for directory toggle fields.
**			fdir      = Actual directories for toggle fields.
**			filename  = Default filename.
**			descript = File type description
**       OUTPUT :  
**          filename  = Name of selected file.
**			nc        = Number of chars in 'filename'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntmd_filename(CWnd* parent, char *title, LPCTSTR filter, int nfld, int *ifld, char **ftext, 
							char **fdir, char *fnam, int *nc, char *descrip, int flag)
{
	int i,j;
	UX_pathname ntflt, tempfilt, tempdesp,lfilter,ldescript,save_dir;
	char *fltpt, *desppt;
	DWORD dwFlags;
	dwFlags = OFN_NOCHANGEDIR | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT | OFN_ENABLETEMPLATE |
		OFN_ENABLESIZING;
	char* temp;
	CString FileName ;
	int win_enable;
	int status = 0;
	int sav_cursor = uw_ntgetcur_cursor();
/*
.....save current directory in case filebrowser change it
*/
	GetCurrentDirectory(UX_MAX_PATH_LEN, save_dir);
/*
.....Get parent window
*/
	CWnd *MainWin;
	if (parent != NULL)
		MainWin = parent;
	else
	{
		MainWin = (CMainFrame *)(AfxGetMainWnd());
	}

	strcpy(lfilter, filter);
	strcpy(ldescript, descrip);

	ntflt[0]= '\0';
	i = 0;
	while(i<100)
	{
		fltpt = strchr(lfilter, '|');
		if (fltpt!=NULL)
		{
			*fltpt = '\0';
			fltpt++;
		}
		strcpy(tempfilt, lfilter);
		desppt = strchr(ldescript, '|');
		if (desppt!=NULL)
		{
			*desppt = '\0';
			desppt++;
		}
		strcpy(tempdesp, ldescript);
		strcat(ntflt, tempdesp);
		strcat(ntflt, "|");
/*
.....replace "," with ";"
*/
		for (j=0; j<(int)strlen(tempfilt); j++)
		{
			if (tempfilt[j]==',')
				tempfilt[j] = ';';
		}
		strcat(ntflt, tempfilt);
		if ((fltpt==NULL)||(desppt==NULL))
			break;
		strcat(ntflt, "|");
		strcpy(lfilter, fltpt);
		strcpy(ldescript, desppt);
		i++;
	}
	strcat(ntflt, "|");
/*
.....Added *.* for all files. But first check if filter="*.*",
.....if yes, no need to add "*.*"
*/
	if (strcmp(filter, "*.*")!=0)
	{
		strcat(ntflt, "All Files (*.*)|*.*|");
	}
	strcat(ntflt, "|");

/*
.....Enable dialog
*/
	if (MainWin!=NCL_MainFrame)
	{
		win_enable = NCL_MainFrame->IsWindowEnabled();
		if (win_enable)
		{
			::EnableWindow(NCL_MainFrame->GetSafeHwnd(), FALSE);
		}
	}
/*
......we need enable it in order to orginal for COM common dialog to working
*/
	SetThemeAppProperties(UW_theme);

	uw_ntMDFileOpen(title, ntflt, nfld, ifld, ftext,
								fdir, fnam, nc, descrip, MainWin, flag);
done:;
	if (win_enable)
	{
		::EnableWindow(NCL_MainFrame->GetSafeHwnd(), TRUE);
	}
	_chdir(save_dir);
	uw_ntsetcursor(sav_cursor);
/*
......reset back
*/
	SetThemeAppProperties(0);
	return 0;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntset_gwcapture()
**       Set Mouse focus to graphic window
**
**    PARAMETERS   
**       INPUT  : 
**          none
**			
**       OUTPUT :  
**          wid: graphic window width
**			hgt: graphic window height
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntset_gwcapture()
{
	CWnd * graphic_view = NULL;
	if (frm_done)
	{
		CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
		graphic_view = (CWnd*)(MainDlg->GetActiveView());
	}
	else
		graphic_view = (CWnd*)NCL_Current_View;
	if (graphic_view!=NULL)
	{
		if (graphic_view->GetCapture() != graphic_view)
			graphic_view->SetCapture();
		if (graphic_view->GetFocus() != graphic_view) graphic_view->SetFocus();
	}
	UU_SM_capture = 1;
	return 0;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntenable_window(enable)
**      Enable/Diable  Mouse  & keyboard input in graphic window
**
**    PARAMETERS   
**       INPUT  : 
**          enable : UU_TRUE : enable input
**					 UU_FALSE :disable input
**			
**       OUTPUT :  
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntenable_window(UU_LOGICAL enable)
{
	CWnd * graphic_view = NULL;

	if (frm_done)
	{
		CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
		graphic_view = (CWnd*)(MainDlg->GetActiveView());
	}
	else
		graphic_view = (CWnd*)NCL_Current_View;

	if (graphic_view!=NULL)
	{
		graphic_view->EnableWindow(enable);
	}
	if(enable) UU_SM_capture = 1;
	else 
	{
		UU_SM_capture = 0;
	}
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntrelease_capture()
**       Release Mouse focus
**
**    PARAMETERS   
**       INPUT  : 
**          none
**			
**       OUTPUT :  
**          wid: graphic window width
**			hgt: graphic window height
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

extern "C" int uw_ntrelease_capture()
{
	UU_SM_capture = 0;
	return ReleaseCapture();
}

/**********************************************************************
**    I_FUNCTION :  uw_ntinsert_cmd(int sub)
**       insert a character into command edit field
**
**    PARAMETERS   
**       INPUT  : 
**          sub: Alpha key sub indx number
**			
**       OUTPUT :  
**          None
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntinsert_cmd(int sub)
{
	int start, end;
#define NALPHAKEYS 18
	char Keycode[NALPHAKEYS][3] = {"0", "1", "2", "3", "4",
		"5", "6", "7", "8", "9", ".", ",", "+", "-",
		"/", "*", VK_BACK, "\n"};
	CEdit *editwin;
	CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
	editwin = (CEdit *)((MainDlg->m_commandBar)->GetDlgItem(IDC_ECOMMAND));
	if (editwin==NULL)
		return 0;
	if (editwin->IsWindowEnabled()) 
	{
		if ((sub!=16)&&(sub!=17))
			editwin->ReplaceSel(Keycode[sub]);
		else if (sub==16)
		{
			editwin->GetSel(start, end); 
			if (start==end)
				editwin->SetSel(start-1, end);
			editwin->ReplaceSel("");
		}
		else
		{
			NCL_KeyReturn = 1;
		}
	}
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntis_alphakey(int num)
**       check if a function ID number correspond to a alpha key
**
**    PARAMETERS   
**       INPUT  : 
**          num: function ID number
**			
**       OUTPUT :  
**          None
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntis_alphakey(int num)
{
	int stat,i;
	char func[80], parms[256];
#define NALPHAKEYS 18
	char Keycode[NALPHAKEYS][6] = {"0", "1", "2", "3", "4",
		"5", "6", "7", "8", "9", ".", ",", "+", "-",
		"/", "*", "<-", "ENTER"};
	if (num<0)
		stat = 0;
	stat = uz_ntcnvtnum_to_func(num, func, parms, 2);
	if (num<0)
		stat = stat;
	if (stat==-1)
		return 0;
	else
	{
/*
.....compared func with Keycode
*/
		for (i=0; i<NALPHAKEYS;i++)
		{
			if (strcmp(func, Keycode[i])==0)
				return 1;
		}
		return 0;
	}
}
	
/**********************************************************************
**    I_FUNCTION :  uw_nthide_menu(CToolBar *bar)
**       Hide a menu(status) bar
**    PARAMETERS
**       INPUT  :
**          bar: bar to be hide
**
**       OUTPUT :
**          None
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_nthide_menu(CToolBar *bar)
{
	if (NCL_MainFrame!=NULL)
		NCL_MainFrame->ShowControlBar(bar, FALSE, TRUE);
}
/**********************************************************************
**    I_FUNCTION : uw_ntprint_scr()
**			print current graphic into printer
**			
**    PARAMETERS   
**       INPUT  : 
**				flag: 1: open printer setup dialog
**						0: print directly
**				size: paper size (define in mdrwsize.h, use PS)
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntprint_scr(int flag, int size, int fit, int bcolor, int pcenter)
{
	((CNclipvView*)NCL_Current_View)->m_papersize = size;
	((CNclipvView*)NCL_Current_View)->m_fit = fit;
	((CNclipvView*)NCL_Current_View)->m_bcolor = bcolor;
	((CNclipvView*)NCL_Current_View)->m_pcenter = pcenter;
	if (flag==1)
		NCL_MainFrame->SendMessage(WM_COMMAND, ID_FILE_PRINT);
	else
		NCL_MainFrame->SendMessage(WM_COMMAND, ID_FILE_PRINT_DIRECT);
}
/**********************************************************************
**    I_FUNCTION : uw_ntsave_screenfile(filename)
**			save current graphic into a bitmap file
**			
**    PARAMETERS   
**       INPUT  : 
**				fname:  bitmap file name
**				
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntsave_screenfile(char *filename, int ftype, int size, int fit, int bcolor)
{
	((CNclipvView*)NCL_Current_View)->m_fit = fit;
	((CNclipvView*)NCL_Current_View)->m_bcolor = bcolor;
	((CNclipvView*)NCL_Current_View)->m_papersize = size;
	((CNclipvView *)NCL_Current_View)->SaveToBMP(filename);
}

/**********************************************************************
**    I_FUNCTION : uw_ntprint_scr()
**			print current graphic into printer
**			
**    PARAMETERS   
**       INPUT  : 
**				flag: 1: open printer setup dialog
**						0: print directly
**				size: paper size (define in mdrwsize.h, use PS)
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntgetDIBIPV()
{
	CNclipvView * pkt = (CNclipvView *)NCL_CurrentPKT_View;
	pkt->GetDIB();
}

/**********************************************************************
**    I_FUNCTION : uw_ntprintIPV(CDC* pDC)
**			print current graphic into printer
**			
**    PARAMETERS   
**       INPUT  : 
**				pDC: printer (device context)
**						
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntprintIPV(CDC* pDC)
{
	CNclipvView * pkt = (CNclipvView *)NCL_CurrentPKT_View;
	pkt->PrintDIB(pDC);
}

/***********************************************************************
c
c   SUBROUTINE:  ChangeDialogFont(CWnd* pWnd, CFont* pFont)
c
c   FUNCTION:  This function change window's font
c
c   INPUT:  CWnd* pWnd: window which font to be changed
c			CFont* pFont: new font to be set
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void ChangeDialogFont(CWnd* pWnd, CFont* pFont)
{
	pWnd->SetFont(pFont);
/*
......iterate through and change their font.
*/
	CWnd* pChildWnd = pWnd->GetWindow(GW_CHILD);

	while (pChildWnd)
	{
		pChildWnd->SetFont(pFont);
		pChildWnd = pChildWnd->GetWindow(GW_HWNDNEXT);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  uw_ntupd_comfont()
c
c   FUNCTION:  This function change update the command window font size
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void uw_ntupd_comfont()
{
/*
.....delete the command linne window and recreated
*/
/*
.....save the command window position first before delete
*/
	DWORD type = (NCL_MainFrame->m_commandBar)->GetBarStyle();
	int stype = 1;
	CControlBar* tempbar;
	int k, indx;
	for (k=1; k<=4;k++)
	{
		indx = 0;
again:;
		tempbar = (CControlBar* )uw_ntget_bars(k, indx);
		if (tempbar==NULL) continue;
		if (tempbar->IsKindOf(RUNTIME_CLASS(CNCLDialogBar)))
		{
			if ((NCL_MainFrame->m_commandBar!=NULL)
				&& ((NCL_MainFrame->m_commandBar)->m_visible==1)
				&& (NCL_MainFrame->m_commandBar==tempbar))
			{
				stype = k;
				break;
			}
/*
......assume there is less than 50 dialogbar in the top, just avoid to get into
......the loop, the if (tempbar==NULL) continue; will get out of loop supposely
*/
			else if (indx<50)
			{
				indx++;
				goto again;
			}
		}
	}
	int sav_float = (NCL_MainFrame->m_commandBar)->IsFloating();
	RECT rect;
	int bar_size[3];
	CEdit *editwin;
	CWnd *labelwin;

	CString label, text;
	(NCL_MainFrame->m_commandBar)->GetWindowRect(&rect);
	int enabled;
	int visible = (NCL_MainFrame->m_commandBar)->m_visible;
	if (visible)
	{
		editwin = (CEdit *)((NCL_MainFrame->m_commandBar)->GetDlgItem(IDC_ECOMMAND));
		enabled = editwin->IsWindowEnabled();
		editwin->GetWindowText(text);
		((NCL_MainFrame->m_commandBar)->GetDlgItem(IDC_PCOMMAND))->GetWindowText(label);
	}
	delete NCL_MainFrame->m_commandBar;

	NCL_MainFrame->m_commandBar = new CNCLCmdBar();
	NCL_MainFrame->m_commandBar->m_TitleCaption = "NCL Command Bar";
	bar_size[0] = rect.right - rect.left;
	bar_size[1] = rect.bottom - rect.top;
	bar_size[2] = 1;

	if ((bar_size[0]<=0)||(bar_size[1]<=0))
	{
		bar_size[0] = UDM_run_layout.command_size[0];
		bar_size[1] = UDM_run_layout.command_size[1];
		bar_size[2] = UDM_run_layout.command_size[2];
	}
	UDM_run_layout.command_size[0] = bar_size[0];
	UDM_run_layout.command_size[1] = bar_size[1];
	UDM_run_layout.command_size[2] = bar_size[2];
	UDM_layout.command_active = 0;
	NCL_MainFrame->m_commandBar->CreateCmdBar();
	(NCL_MainFrame->m_commandBar)->EnableDocking(CBRS_ALIGN_BOTTOM | CBRS_ALIGN_TOP);
	NCL_MainFrame->m_commandBar->m_created = 1;
	if (sav_float==0)
	{
		if (stype==1)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_TOP, &rect);
		else if (stype==2)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_BOTTOM, &rect);
		else if (stype==3)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_LEFT, &rect);
		else if (stype==4)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_commandBar, AFX_IDW_DOCKBAR_RIGHT, &rect);
	}
	else
	{
		CPoint pt(0,0);
		CRect rect1 (&rect);
		uw_ntadd_menuborder(&rect1);
		pt.Offset(rect1.left, rect1.top);	
		if (visible==1)
			NCL_MainFrame->FloatControlBar(NCL_MainFrame->m_commandBar, pt);
	}
	if (visible==1)
	{	NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_commandBar,
					TRUE, FALSE);
		(NCL_MainFrame->m_commandBar)->m_visible = 1;
		editwin = (CEdit *)((NCL_MainFrame->m_commandBar)->GetDlgItem(IDC_ECOMMAND));
		labelwin = ((NCL_MainFrame->m_commandBar)->GetDlgItem(IDC_PCOMMAND));
		editwin->SetWindowText(text);
		labelwin->SetWindowText(label);
		if (enabled)
		{
			editwin->EnableWindow(TRUE);
			labelwin->EnableWindow(TRUE);
			NCL_MainFrame->m_commandBar->m_enabled = 1;
		}
		else
		{
			editwin->EnableWindow(FALSE);
			NCL_MainFrame->m_commandBar->m_enabled = 0;
		}
	}
/*
.....need update NCL_cmdwin
*/
	CWnd *cwin = NCL_MainFrame->m_commandBar->GetDlgItem(IDC_ECOMMAND);
	NCL_cmdwin = cwin->m_hWnd;
}

/***********************************************************************
c
c   SUBROUTINE:  uw_ntupdate_accel()
c
c   FUNCTION:  recreate accel table
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void uw_ntupdate_accel()
{
///	NCL_MainFrame->Recreate_AcceleratorTable();
}
/**********************************************************************
**    I_FUNCTION :  uw_ntcmd_sel(int start, int end)
**		Select the command line text
**
**    PARAMETERS   
**       INPUT  : 
**          start: start position
**			end:	end position
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntcmd_sel(int start, int end)
{
	CMainFrame *MainWin = (CMainFrame*)(AfxGetMainWnd());
	if (MainWin!=NULL)
		MainWin->SetCommand_Select(start, end);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntpost_closemsg
**		executable the closing
**
**    PARAMETERS   
**       INPUT  : 
**			none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntpost_closemsg()
{
	CNclipvApp *app = (CNclipvApp*)AfxGetApp();
//	app->OnNCLSafeExit();
}

/**********************************************************************
**    I_FUNCTION :  uw_ntget_ipvsize(int *wid, int *hgt)
**       Get IPV window size
**
**    PARAMETERS   
**       INPUT  : 
**          none
**			
**       OUTPUT :  
**          wid: graphic window width
**			hgt: graphic window height
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntget_ipvsize(int *wid, int *hgt)
{
	*hgt = *wid = 0;
	CNclipvView * graphic_view = (CNclipvView *)NCL_CurrentPKT_View;

	*hgt = (graphic_view->m_oldRect).bottom;
	*wid = (graphic_view->m_oldRect).right;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntsctogc2(POINT *pt)
**       Convert screen position to graphic windowclient postion
**
**    PARAMETERS   
**       INPUT  : 
**          pt: screen position
**			
**       OUTPUT :  
**          pt: graphic window client position
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntsctogc2(POINT *pt)
{
	CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
	CNclipvView * graphic_view = (CNclipvView *)(MainDlg->GetActiveView());
	graphic_view->ScreenToClient(pt);
}
/**********************************************************************
**    I_FUNCTION : uw_ntyes_no_cancel(CWnd *parent, char* msg, char *title)
**			popup a YesNoCancel question dialog box and get answer
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent window
**				title:  dialog box title
**				msg:    dialog str.
**       OUTPUT :  
**         			none 
**    RETURNS      : 0:No
**					 1: Yes
**					-1: Cancel
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/

extern "C" int uw_ntyes_no_cancel(CWnd *parent, char* msg, char *title)
{
	int result;
	int sav_cursor = uw_ntgetcur_cursor();

	if (parent==NULL)
	{
		CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
		if (MainWin==NULL)
			result = MessageBox(NULL, msg, title, MB_YESNOCANCEL);
		else
			result = MainWin->MessageBox(msg, title, MB_YESNOCANCEL);
	}
	else
		result = parent->MessageBox(msg, title, MB_YESNOCANCEL);
	uw_ntsetcursor(sav_cursor);
	if (result==IDYES)
		return 1;
	else if (result==IDNO)
		return 0;
	else
		return -1;
}

/*********************************************************************
**    E_FUNCTION     : uw_ntupd_input(text)
**       Update the active input prompt (it could be
**		the input of command prompt or active form field if any of them is active).
**		
**    PARAMETERS   
**       INPUT  : text: text to be updated
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntupd_input(char *text)
{
	int stat, enable;
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
	CWnd *wnd = MainWin->GetActiveWindow();
			
	stat = -1;
	if ((wnd->IsKindOf(RUNTIME_CLASS(CNCLForm)))
		|| (wnd->IsKindOf(RUNTIME_CLASS(CNCLFormBar))))
	{
/*
.....check if there is active form edit field
*/
		stat = uw_formupd_input(text);
	}
	if (stat==0)
		return;
/*
.....now if the command/prompt mode open, just set the text
*/
	CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
	enable = MainDlg->Get_CmdEnable();
	if (enable)
	{
		MainDlg->InsertCommand_Str(text);
		return;
	}
	else
		uw_ntpost_textmsg(text);
	return;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntinsert_cmdstr(string);
**       insert a character string into command edit field
**
**    PARAMETERS   
**       INPUT  : 
**          string: a character string to be insert to command edit field
**			
**       OUTPUT :  
**          None
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntinsert_cmdstr(char *string)
{
	CEdit *editwin;
	CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
	editwin = (CEdit *)((MainDlg->m_commandBar)->GetDlgItem(IDC_ECOMMAND));
	
	if (editwin==NULL)
		return 0;
	if (editwin->IsWindowEnabled()) 
	{
		editwin->ReplaceSel(string);
	}
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntis_inputfunc(int num)
**       check if a function ID number correspond to a 'by text', 'by location'
**			and 'by pick' function
**
**    PARAMETERS   
**       INPUT  : 
**          num: function ID number
**			
**       OUTPUT :  
**          None
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntis_inputfunc(int num)
{
	int stat;
	char func[80], parms[256];
	int sub, type;
	short app;

	stat = uz_ntcnvtnum_to_func(num, func, parms, 2);
	if (stat==-1)
		return 0;
	else
	{
/*
.....compared func
*/
		if ((strcmp(func, "KEY_TEXT")==0) ||
			(strcmp(func, "KEY_LOCATE")==0) ||
			(strcmp(func, "KEY_PICK")==0))
			return 1;
		if (uz_which_keydef(func,&type,&sub,&app) == UU_SUCCESS)
		{
/*
.....all selection functions too
*/
			if ((type == 3 /*NCLKEY*/) && (sub>=112) && (sub<=131))
				return 1;
		}
	}
	return 0;
}
	
/***********************************************************************
c
c   SUBROUTINE:  uw_ntupd_prmptfont()
c
c   FUNCTION:  This function change update the prompt window font
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void uw_ntupd_prmptfont()
{
/*
.....delete the prompt window and recreated
*/
/*
.....save the prompt window position first before delete
*/
	DWORD type = (NCL_MainFrame->m_promptBar)->GetBarStyle();
	int stype = 1;
	CControlBar* tempbar;
	int k, indx;
	for (k=1; k<=4;k++)
	{
		indx = 0;
again:;
		tempbar = (CControlBar* )uw_ntget_bars(k, indx);
		if (tempbar==NULL) continue;
		if (tempbar->IsKindOf(RUNTIME_CLASS(CNCLDialogBar)))
		{
			if ((NCL_MainFrame->m_promptBar!=NULL)
				&& ((NCL_MainFrame->m_promptBar)->m_visible==1)
				&& (NCL_MainFrame->m_promptBar==tempbar))
			{
				stype = k;
				break;
			}
/*
......assume there is less than 50 dialogbar in the top, just avoid to get into
......the loop, the if (tempbar==NULL) continue; will get out of loop supposely
*/
			else if (indx<50)
			{
				indx++;
				goto again;
			}
		}
	}
	int sav_float = (NCL_MainFrame->m_promptBar)->IsFloating();
	RECT rect;
	int bar_size[2];
	CWnd *labelwin;

	CString label;
	(NCL_MainFrame->m_promptBar)->GetWindowRect(&rect);
	int visible = (NCL_MainFrame->m_promptBar)->m_visible;
	if (visible)
	{
		labelwin = (NCL_MainFrame->m_promptBar)->GetDlgItem(IDC_LPROMPT);
		labelwin->GetWindowText(label);
	}
	delete NCL_MainFrame->m_promptBar;

	NCL_MainFrame->m_promptBar = new CNCLDialogBar();
	NCL_MainFrame->m_promptBar->m_TitleCaption = "NCL Prompt Bar";
	bar_size[0] = rect.right - rect.left;
	bar_size[1] = rect.bottom - rect.top;

	if ((bar_size[0]==0)||(bar_size[1]==0))
	{
		bar_size[0] = UDM_run_layout.prompt_size[0];
		bar_size[1] = UDM_run_layout.prompt_size[1];
	}
	UDM_run_layout.prompt_size[0] = bar_size[0];
	UDM_run_layout.prompt_size[1] = bar_size[1];
	if (!(NCL_MainFrame->m_promptBar->CreateLabel(NCL_MainFrame, 
				" ", bar_size, type, IDC_LPROMPT, IDD_PROMPTBAR)))
	{
		MessageBox(NULL, "Failed to create NCL Prompt bar", "Warning", MB_ICONINFORMATION|MB_OK);
		return;      
	}
	NCL_MainFrame->m_promptBar->m_created = 1;
	(NCL_MainFrame->m_promptBar)->EnableDocking(CBRS_ALIGN_BOTTOM | CBRS_ALIGN_TOP);
	if (sav_float==0)
	{
		if (stype==1)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_promptBar, AFX_IDW_DOCKBAR_TOP, &rect);
		else if (stype==2)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_promptBar, AFX_IDW_DOCKBAR_BOTTOM, &rect);
		else if (stype==3)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_promptBar, AFX_IDW_DOCKBAR_LEFT, &rect);
		else if (stype==4)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_promptBar, AFX_IDW_DOCKBAR_RIGHT, &rect);
	}
	else
	{
		CPoint pt(0,0);
		CRect rect1 (&rect);
		uw_ntadd_menuborder(&rect1);
		pt.Offset(rect1.left, rect1.top);	
		if (visible==1)
			NCL_MainFrame->FloatControlBar(NCL_MainFrame->m_promptBar, pt);
	}
	if (visible==1)
	{	NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_promptBar,
					TRUE, FALSE);
		(NCL_MainFrame->m_promptBar)->m_visible = 1;
		labelwin = ((NCL_MainFrame->m_promptBar)->GetDlgItem(IDC_LPROMPT));
		labelwin->SetWindowText(label);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  uw_ntupd_errorfont()
c
c   FUNCTION:  This function change update the error area font
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void uw_ntupd_errorfont()
{
/*
.....delete the error window and recreated
*/
/*
.....save the error window position first before delete
*/
	DWORD type = (NCL_MainFrame->m_errorBar)->GetBarStyle();
	int stype = 1;
	CControlBar* tempbar;
	int k, indx;
	for (k=1; k<=4;k++)
	{
		indx = 0;
again:;
		tempbar = (CControlBar* )uw_ntget_bars(k, indx);
		if (tempbar==NULL) continue;
		if (tempbar->IsKindOf(RUNTIME_CLASS(CNCLDialogBar)))
		{
			if ((NCL_MainFrame->m_errorBar!=NULL)
				&& ((NCL_MainFrame->m_errorBar)->m_visible==1)
				&& (NCL_MainFrame->m_errorBar==tempbar))
			{
				stype = k;
				break;
			}
/*
......assume there is less than 50 dialogbar in the top, just avoid to get into
......the loop, the if (tempbar==NULL) continue; will get out of loop supposely
*/
			else if (indx<50)
			{
				indx++;
				goto again;
			}
		}
	}
	int sav_float = (NCL_MainFrame->m_errorBar)->IsFloating();
	RECT rect;
	int bar_size[2];
	CWnd *labelwin;

	CString label;
	(NCL_MainFrame->m_errorBar)->GetWindowRect(&rect);
	int visible = (NCL_MainFrame->m_errorBar)->m_visible;
	if (visible)
	{
		labelwin = (NCL_MainFrame->m_errorBar)->GetDlgItem(IDC_LERROR);
		labelwin->GetWindowText(label);
	}
	delete NCL_MainFrame->m_errorBar;

	NCL_MainFrame->m_errorBar = new CNCLDialogBar();
	NCL_MainFrame->m_errorBar->m_TitleCaption = "NCL Prompt Bar";
	bar_size[0] = rect.right - rect.left;
	bar_size[1] = rect.bottom - rect.top;

	if ((bar_size[0]==0)||(bar_size[1]==0))
	{
		bar_size[0] = UDM_run_layout.error_size[0];
		bar_size[1] = UDM_run_layout.error_size[1];
	}
	UDM_run_layout.error_size[0] = bar_size[0];
	UDM_run_layout.error_size[1] = bar_size[1];
	if (!(NCL_MainFrame->m_errorBar->CreateLabel(NCL_MainFrame, 
				" ", bar_size, type, IDC_LERROR, IDD_ERRORBAR)))
	{
		MessageBox(NULL, "Failed to create NCL error bar", "Warning", MB_ICONINFORMATION|MB_OK);
		return;      
	}
	NCL_MainFrame->m_errorBar->m_created = 1;
	(NCL_MainFrame->m_errorBar)->EnableDocking(CBRS_ALIGN_BOTTOM | CBRS_ALIGN_TOP);
	if (sav_float==0)
	{
		if (stype==1)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_errorBar, AFX_IDW_DOCKBAR_TOP, &rect);
		else if (stype==2)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_errorBar, AFX_IDW_DOCKBAR_BOTTOM, &rect);
		else if (stype==3)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_errorBar, AFX_IDW_DOCKBAR_LEFT, &rect);
		else if (stype==4)
			NCL_MainFrame->DockControlBar(NCL_MainFrame->m_errorBar, AFX_IDW_DOCKBAR_RIGHT, &rect);
	}
	else
	{
		CPoint pt(0,0);
		CRect rect1 (&rect);
		uw_ntadd_menuborder(&rect1);
		pt.Offset(rect1.left, rect1.top);	
		if (visible==1)
			NCL_MainFrame->FloatControlBar(NCL_MainFrame->m_errorBar, pt);
	}
	if (visible==1)
	{	NCL_MainFrame->ShowControlBar(NCL_MainFrame->m_errorBar,
					TRUE, FALSE);
		(NCL_MainFrame->m_errorBar)->m_visible = 1;
		labelwin = ((NCL_MainFrame->m_errorBar)->GetDlgItem(IDC_LERROR));
		labelwin->SetWindowText(label);
	}
}

extern "C" void uw_save_layout_pos2(int bar_type, int pos)
{
	if (bar_type==1)
		UDM_run_layout.command_pos[2] = pos;
	else if (bar_type==2)
		UDM_run_layout.statwin_pos[2] = pos;
	else if (bar_type==4)
		UDM_run_layout.error_pos[2] = pos;
	else if (bar_type==5)
		UDM_run_layout.prompt_pos[2] = pos;
}
/***********************************************************************
c
c   SUBROUTINE:  uw_run_process (char *dir, char *exe, char* cmdparm)
c
c   FUNCTION:  This function Create a sub-process without open a window
c
c       INPUT  :	dir = Full directory path which 'exe' resides in.
c					exe = Name of utility program to run. (have to be a executable, not batch file)
c					cmdparm = Command line to pass to the process.
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" int uw_run_process (char *dir, char *exe, char* cmdparm)
{
	char buf [UX_MAX_PATH_LEN+40], fulldir[UX_MAX_PATH_LEN];
	STARTUPINFO stinfo;
	PROCESS_INFORMATION info;
	int status;
	DWORD ecode;
	MSG msg;

	ZeroMemory(&stinfo, sizeof(stinfo) );
    stinfo.cb = sizeof(stinfo);
    ZeroMemory( &info, sizeof(info) );

	char command[UX_MAX_PATH_LEN+40];
/*
......we allow dir to be a envirment value, so get the fullpath first
*/
	if (dir!=NULL)
		ul_get_full_dir(dir,fulldir);
	else
		fulldir[0] = '\0';
/*
.....somehow, pass the first paremeter (application name) and second parameter (command parameter line)
.....seperately seems not working. So I pass everything in the second 
.....parameter (command line), and it works fine.
*/
	if (fulldir[0]=='\0')
		sprintf (command, "\"%s\" %s", exe, cmdparm);
	else
		sprintf (command, "\"%s\\%s\" %s",fulldir,exe, cmdparm);
	int result = CreateProcess(NULL, command, NULL, NULL, FALSE, 
		DETACHED_PROCESS, NULL, NULL, &stinfo, &info);
	if (result==0)
	{
		sprintf(buf, "Error trying to run %s", command);
		uw_ntdispmsg(buf);
		return -1;
	}
/*
......wait for the process until done.
*/
	status = 1;
/*
.....we must use this to wait for process initial, otherwise, it take long time
*/
	WaitForInputIdle(info.hProcess,INFINITE);
	while (status!=0)
	{
		status = GetExitCodeProcess(info.hProcess, &ecode);
		if (ecode!=STILL_ACTIVE)
			break;
		else
		{
			if (::PeekMessage(&msg,NULL,WM_PAINT,WM_PAINT,PM_NOREMOVE))
			{
				if (GetMessage(&msg,NULL,WM_PAINT,WM_PAINT))
				DispatchMessage(&msg);
			}
		}
	}
	CloseHandle(info.hProcess );
	CloseHandle(info.hThread );
	return 1;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntget_labelwid(string, wid)
**       get graphic label wid in pixels
**    PARAMETERS   
**       INPUT  : 
**          string: string label
**       OUTPUT :  
**          wid: width of the string in pixel
**		
**    RETURNS      -1: not success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntget_labelwid(char *string, int *wid)
{
	HDC hdc;
	CWnd *current_view = (CNclipvView *)NCL_Main_View;

	if (uw_gltext_base == 0) return -1;
	CDC* dc = current_view->GetDC( );
	if (dc == UU_NULL) return -1;
	double width;
	hdc = dc->GetSafeHdc();
	SelectObject(hdc, UW_graphic_font);
	CSize sizeText = dc->GetTextExtent(string,strlen(string));
	POINT fsize;
	fsize.x = (long)sizeText.cx;
	fsize.y = sizeText.cy;
	dc->LPtoDP(&fsize);

	width = (double)fsize.x;
	*wid = (int)(width) + 1;
	current_view->ReleaseDC(dc);
	return 0;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntsetwin_title(title)
**       Set NCL window title
**    PARAMETERS   
**       INPUT  : 
**          title: title to be set
**       OUTPUT :  
**          none
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntsetwin_title(char *title)
{
	char buf[256];
	strcpy(buf,title);
	NCL_MainFrame->SetWindowText(buf);
}

/**********************************************************************
**    E_FUNCTION :  uw_get_curdc_xy(ipv, x,y,z)
**      get current device position
**
**    PARAMETERS   
**       INPUT  : ipv: if get position of NCLIPV window
**			
**       OUTPUT :  
**          x,y: current position in device cord
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_get_curdc_xy(int ipv, int *x, int *y)
{
	CMainFrame *MainDlg = (CMainFrame *)(AfxGetMainWnd());
	CNclipvView * graphic_view = (CNclipvView *)(MainDlg->GetActiveView());
	POINT pt;
	int rast[2];

	GetCursorPos(&pt);
	graphic_view->ScreenToClient(&pt);
	*x = pt.x; *y = pt.y;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntsave_bitmap(unsigned char *buffer, int nWidth, int nHeight, char *filename)
**       save the pixel arrays into a bitmap file
**
**    PARAMETERS   
**       INPUT  : 
**			buffer: pixel arrays
**			nWidth, nHeight: pixel width and height
**			filename: bitmap filename to be saved
**       OUTPUT :  
**          none
**		
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntsave_bitmap(unsigned char *buffer, int nWidth, int nHeight, char *filename)
{
	BITMAPINFO bi; 
	ZeroMemory(&bi, sizeof(BITMAPINFO));
	bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
	bi.bmiHeader.biWidth = nWidth;
	bi.bmiHeader.biHeight = nHeight;
	bi.bmiHeader.biPlanes = 1;
	bi.bmiHeader.biBitCount = 24;

	BITMAPFILEHEADER bh;
	ZeroMemory(&bh, sizeof(BITMAPFILEHEADER));
	bh.bfType = 0x4d42; //bitmap 
	bh.bfOffBits = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);
	bh.bfSize = bh.bfOffBits + ((nWidth*nHeight)*3);

	CFile file;
	if(file.Open(filename, CFile::modeCreate | CFile::modeWrite))
	{ 
		file.Write(&bh, sizeof(BITMAPFILEHEADER));
		file.Write(&(bi.bmiHeader), sizeof(BITMAPINFOHEADER));
		file.Write(buffer, 3 * nWidth * nHeight);
		file.Close();
	}
}

extern "C" void uw_setform_active(int flag)
{
	S_form_active = flag;
}
extern "C" int uw_isform_active()
{
	return S_form_active;
}
/**********************************************************************
**    I_FUNCTION : uw_ntget_filename(CWnd* parent, char *title, LPCTSTR filter, char* fnam, int *nc)
**			popup a file browser dialog box 
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent window
**				title:  file browser title
**				filter: file filter.
**				fname:  default file name
**				nc:     number of character of filename
**			descript = File type description
**				open_flag: TRUE: open file browser as "Open"
**							FALSE: open file browser as "SAve as"
**       OUTPUT :  
**				fname:  use enter file name
**				nc:     number of character of filename
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntget_filename(CWnd* parent, char *title, char * filter, char* fnam, int *nc, char *descrip, int open_flag,
	char *paths, char *path_des)
{
	int nfld, ifld, len, len1, add;
	char *ftext[20], *fdir[20];

	char tempstr[UX_MAX_PATH_LEN*20], c_dir[UX_MAX_PATH_LEN];
	char* temp, *tok;
	int i,j, status;

	if ((paths==NULL)||(paths!=NULL)&&(paths[0]=='\0'))
		return uw_ntget_filename2(parent, title, filter, fnam, nc, descrip, open_flag);

	status = S_extend_path(tempstr, paths, NULL);
	if (tempstr[0]=='\0')
		return uw_ntget_filename2(parent, title, filter, fnam, nc, descrip, open_flag);

	temp = strrchr (tempstr, ';');
	if (temp==NULL)
	{
		if ((path_des==NULL)||(path_des!=NULL)&&(path_des[0]=='\0'))
			return uw_ntget_filename2(parent, title, filter, fnam, nc, descrip, open_flag);
	}
	tok = strtok (tempstr, ";");
	if (tok==NULL)
		return uw_ntget_filename2(parent, title, filter, fnam, nc, descrip, open_flag);
	for (i=0; i<20;i++)
	{
		fdir[i] = NULL;
		ftext[i] = (char*) uu_malloc((41)*sizeof (char));
	}
	nfld = 0;
	while (tok!=NULL)
	{
/*
......remove ending spaces
*/
		len = strlen (tok);
		while (len>0)
		{
			if (tok[len-1]==' ')
				len--;
			else
				break;
		}
		if (len<=0) break;
		fdir[nfld] = (char*) uu_malloc((len+1)*sizeof (char));
		strncpy(fdir[nfld], tok, len);
		fdir[nfld][len] = '\0';
		nfld++;
		tok = strtok (NULL, ";");
	}
	ifld = 0;
	if (path_des!=NULL)
	{
		strcpy(tempstr, path_des);
		tok = strtok (tempstr, ";");
		while (tok!=NULL)
		{
/*
......remove ending spaces
*/
			len = strlen (tok);
			while (len>0)
			{
				if (tok[len-1]==' ')
					len--;
				else
					break;
			}
			if (len<=0) break;
			if (len>40) len = 40;
			strncpy(ftext[ifld], tok, len);
			ftext[ifld][len] = '\0';
			ifld++;
			tok = strtok (NULL, ";");
		}
	}
	if ((status==1)&&(ifld<nfld))
	{
/*
......if the paths is extended, then we just use the path name as description 
......since it might have matching problem
*/
		ifld = 0;
	}
	while (ifld<nfld)
	{
		S_add_ftext_str(fdir, ftext, ifld);
		ifld++;
	}
/*
.....if the 'Local' is not in the decription, add in the end
*/
	GetCurrentDirectory(UX_MAX_PATH_LEN, c_dir);
	add = 1;
	for (i=0; i<nfld; i++)
	{
		if (stricmp(ftext[i], "Local")==0)
		{
			len = strlen (fdir[i]);
			len1 = len - 5;
			if (len1<0) len1 = 0;
			if (strcmp(&(fdir[i][len1]), "Local")==0)
			{
				len1 = len - 40;
				if (len1<0) len1 = 0;
				strcpy(ftext[i], &(fdir[i][len1]));
				add = 1;
			}
			else
				add = 0;
		}
	}
	if (add)
	{
		strcpy(ftext[nfld], "Local");
		len = strlen(c_dir);
		fdir[nfld] = (char*) uu_malloc((len+1)*sizeof (char));
		strncpy(fdir[nfld], c_dir, len);
		fdir[nfld][len] = '\0';
		nfld++;
	}
	ifld = 0;
	uw_ntmd_filename(parent, title, filter, nfld, &ifld, ftext, fdir, fnam, nc, descrip, open_flag);
	for (i=0; i<20;i++)
	{
		if (fdir[i] != NULL)
			uu_free(fdir[i]);
		uu_free(ftext[i]);
	}
}
extern "C" int uw_ntset_ipv_capture()
{
	return uw_ntset_gwcapture();
}
extern "C" void uw_ntcurdc_toscrn(int ipv, int xin, int yin, int *x, int *y)
{
	POINT pt;
	CNclipvView * pocket_view = (CNclipvView *) NCL_CurrentPKT_View;
	if (pocket_view==NULL) return;
	pt.y = yin;
	pt.x = xin;
	pocket_view->ClientToScreen(&pt);
	*x = pt.x;
	*y = pt.y;
}
extern "C" int uw_glget_context()
{
	return UM_IPV_WINDOW;
}
