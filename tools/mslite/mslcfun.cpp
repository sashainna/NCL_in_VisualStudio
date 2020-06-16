/********************************************************************* 
**  NAME:  mslcfunc.cpp
**
**			C functions which used C++ class functions
**			it is the interface between C and C++
**			This file is similar to wsntcfun.cpp but using different
**			application classes
**	CONTAINS: 
**			uw_ntmsg_box
**			uw_ntdispmsg (char *msg)
**			uw_nterror (char *msg)
**			uw_ntsctogc(POINT *pt)
**			uw_ntgctosc(POINT *pt)
**			uw_ntget_gwsize(int *wid, int *hgt)
**			uw_ntset_pointer(int x, int y)
**			uw_ntget_filename
**			uw_ntget_dirname
**			uw_ntyesnocancel
**			uw_ntyes_or_no
**			uw_ntget_ctrpos(int *x, int *y, int flag)
**			uw_ntgraph_toscrn
**			uw_ntset_curpos(int x, int y)
**			uw_ntget_strsize
**			uw_ntcreate_font()
**			uw_ntmd_filename
**			uw_ntset_gwcapture()
**			uw_ntrelease_capture()
**			uw_nthide_menu(CToolBar *bar)
**			ChangeDialogFont()
**			uw_ntsctogc2()
**			uw_ntpost_msg()
**			uw_ntyes_no_cancel()
**			uw_ntupd_input
**			uw_ntis_inputfunc
**
**    COPYRIGHT 2008 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       mslcfun.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       10/27/16 , 15:29:10
*********************************************************************/
#include "usysdef.h"
/*
.....have to include wsntstdafx.h which have more definitions we need
*/
//#include "toolstdafx.h"
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
#include "mslite.h"
#include "wsntdoc.h"
#include "msliteview.h"
#include "mslMainFrm.h"
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

int NT_cmap_changed = 0;
static UM_pkwin_type LastContext = UM_DRAWING_WINDOW;
int UU_SM_capture = 0;

extern "C" int UD_cmap_changed ;
extern CWnd *NCL_Current_View;
extern CWnd *NCL_CurrentPKT_View;
extern "C" UU_LOGICAL UR_changed;
extern "C" int UW_pocket_mode;
extern "C" int UD_printipv;
extern int frm_done;
extern CMainFrame *NCL_MainFrame;
CDialog *Pocket_Win[UM_MAX_POCKET] = {NULL,NULL,NULL};
extern int NCL_savcmd_enable;
extern int NCL_KeyReturn;
extern "C" int uz_ntcnvtnum_to_func(int num, char *func, char* parms, int flag);
extern "C" void uw_ntsetcursor(int cursorno);
extern "C" int uw_ntgetcur_cursor();
extern "C" void ul_ipv_set_context();
extern "C" UM_pkwin_type ul_ipv_get_context();
extern "C" int uw_ntsave_prect(UM_pkwin_type type, int left, int top, int right, int bottom);
extern void uw_ntadd_menuborder(CRect *rect);
extern CControlBar* uw_ntget_bars(int bar_pos, int indx);
extern "C" char * um_get__win();
extern "C" int UV_dynview_active;
extern "C" int UM_swap_ipv;
extern "C" int NCL_swap_changed;
extern "C" int PKx,PKy;
extern int NCL_common_open;
extern CMemoryMappedFile NCL_common_file;
extern "C" int NCQ_running;
static int NCQ_comm_pos = 0;
extern HWND NCL_cmdwin;
extern "C" char *um_get_pocket_window(int);
extern char UW_cur_screen[20];
extern DWORD UW_theme;
typedef struct
{
	int flag;
	char ppfile[256];
	int current, highest, lines;
	char macro[8];
	int warn, error;
} NCLInfo;

extern "C" int uw_formupd_input(char *text);
extern "C" int uw_ntpost_textmsg(char *text);
extern "C" int uz_which_keydef(char*,int*,int*,short*);
extern "C" int MSLite;
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
		CMsliteApp *app = (CMsliteApp*)AfxGetApp();
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
	uw_ntmsg_box(NULL, "MS Lite Message", msg, 0);
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
	uw_ntmsg_box(NULL, "MSLite Error", msg, 1);
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
	if (MainDlg==NULL) return;
	CMsliteView * graphic_view = (CMsliteView *)(MainDlg->GetActiveView());
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
	if (MainDlg==NULL) return;
	CMsliteView * graphic_view = (CMsliteView *)(MainDlg->GetActiveView());
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
	CMsliteView * graphic_view = NULL;
	if (frm_done)
	{
		CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
		if (MainDlg!=NULL)
			graphic_view = (CMsliteView *)(MainDlg->GetActiveView());
	}
	else
		graphic_view = (CMsliteView *)NCL_Current_View;

	if (graphic_view!=NULL)
	{
		*hgt = (graphic_view->m_oldRect).bottom;
		*wid = (graphic_view->m_oldRect).right;
	}
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
	CMsliteView * graphic_view = (CMsliteView *)(MainDlg->GetActiveView());
	if (graphic_view==NULL) return;
	pt.x = x;
	pt.y = y;
	graphic_view->ClientToScreen(&pt);
	SetCursorPos(pt.x, pt.y);
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
**          flag: not used here, we have this flag here just to
**			use same parameters as NCL function
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
	CMsliteView * graphic_view = (CMsliteView *)(MainDlg->GetActiveView());

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
	CMsliteView * graphic_view = (CMsliteView *)(MainDlg->GetActiveView());
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
	HFONT font;
	CWnd *current_view = NULL;

	current_view = (CMsliteView *)NCL_Current_View;
	if (current_view==NULL)
		return 0;

	CClientDC dc(current_view);
	hdc = dc.GetSafeHdc();
/*
.....if uw_gltext_base is already located, don't locate again
*/
	if (uw_gltext_base==0)
	{
		if ((uw_gltext_base = glGenLists_d(256))==0)
		{
			uw_ntdispmsg ("No font today");
			return 0;
		}
	}

	font = CreateFont(UW_label_size[0], UW_label_size[1],0,0,400, FALSE, FALSE, FALSE, 
				DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
				DEFAULT_QUALITY, DEFAULT_PITCH, NULL);
	
	SelectObject(hdc, font);
	wglUseFontBitmaps(hdc, 0, 256, uw_gltext_base);

	ABCFLOAT abcf;
	GetCharABCWidthsFloat(hdc, 1, 1, &abcf);

	POINT fsize;

	int wid;
	GetCharWidth(hdc, 1, 1, &wid);
	fsize.x = (long)(abcf.abcfA + abcf.abcfB);
	fsize.y = 12;

	dc.LPtoDP(&fsize);
	uw_glfont.chrpwid = fsize.x;
	uw_glfont.chrphgt = fsize.y;
	uw_glfont.chrwid = (float)(uw_glfont.chrpwid) / uw_gl.xpixels;
	uw_glfont.chrhgt = (float)(uw_glfont.chrphgt) / uw_gl.ypixels;
	uw_glfont.poffset = (int) abcf.abcfA;
	uw_glfont.offset = (float)(uw_glfont.poffset)/ uw_gl.xpixels;
	return 1;
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
/*
.....Reset pocket capture
*/
	return ReleaseCapture();
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
	CMsliteView * graphic_view = (CMsliteView *)(MainDlg->GetActiveView());
	graphic_view->ScreenToClient(pt);
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
			
	if (wnd->IsKindOf(RUNTIME_CLASS(CNCLForm)))
	{
/*
.....check if there is active form edit field
*/
		stat = uw_formupd_input(text);
	}
	if (stat==0)
		return;
	return;
}
/*********************************************************************
**    E_FUNCTION     : uw_ntupd_cinput(text)
**       Update the active command input prompt
**		
**    PARAMETERS   
**       INPUT  : text: text to be updated
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntupd_cinput(char *text)
{
	int enable;
	CWnd *MainWin = (CMainFrame*)(AfxGetMainWnd());
	CWnd *wnd = MainWin->GetActiveWindow();
			
	if ((text!=NULL) && (text[0]!='\0'))
	{
/*
.....check if the command prompt is active
*/
		CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
		enable = MainDlg->Get_CmdEnable();
		if (enable)
		{
			MainDlg->InsertCommand_Str(text);
			return;
		}
/*
.....check if there is active form edit field
*/
		uw_formupd_input(text);
	}

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
	char func[80], parms[40];
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
extern "C" void uw_save_layout_pos2(int bar_type, int pos)
{
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
	CMsliteView * graphic_view;
	graphic_view = (CMsliteView *)NCL_Current_View;
	if (graphic_view!=NULL)
		*win = graphic_view->GetWin();
	else
		*win = NULL;
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
	int upload;
	CMainFrame *MainWin = (CMainFrame*)(AfxGetMainWnd());
	if (MainWin!=NULL)
		MainWin->SetCommand_Str(msg,upload);
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
	int line,upload;
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
	CMsliteView * graphic_view = (CMsliteView *)(MainDlg->GetActiveView());
	POINT pt;
	int rast[2];

	GetCursorPos(&pt);
	graphic_view->ScreenToClient(&pt);
	*x = pt.x; *y = pt.y;
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
	CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
	if (MainDlg==NULL) return;
	CMsliteView * graphic_view = (CMsliteView *)(MainDlg->GetActiveView());

	if (graphic_view!=NULL)
	{
		*hgt = (graphic_view->m_oldRect).bottom;
		*wid = (graphic_view->m_oldRect).right;
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_ntsetwin_title(title)
**       Set MSLITE window title
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
	NCL_MainFrame->SetWindowText(title);
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
	int i,status;

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
/*
......as long as there is description there, display folder option
*/
/*
		strcpy(tempstr, path_des);
		temp = strrchr (tempstr, ';');
		if (temp==NULL)
			return uw_ntget_filename2(parent, title, filter, fnam, nc, descrip, open_flag);
*/
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

extern "C" void uw_setform_active(int flag)
{
	S_form_active = flag;
}

extern "C" int uw_isform_active()
{
	return S_form_active;
}

extern "C" int uw_ntset_ipv_capture()
{
	CMsliteView * graphic_view = NULL;
	if (frm_done)
	{
		CMainFrame *MainDlg = (CMainFrame*)(AfxGetMainWnd());
		if (MainDlg!=NULL)
			graphic_view = (CMsliteView *)(MainDlg->GetActiveView());
	}
	else
		graphic_view = (CMsliteView *)NCL_Current_View;


	if (graphic_view != UU_NULL)
	{
		if (graphic_view->GetCapture() != graphic_view)
			graphic_view->SetCapture();
		if (graphic_view->GetFocus() != graphic_view) graphic_view->SetFocus();
		UU_SM_capture = 0;
	}
	return 0;
}

extern "C" void uw_ntcurdc_toscrn(int ipv, int xin, int yin, int *x, int *y)
{
	POINT pt;
	CMsliteView * graphic_view = (CMsliteView *)NCL_Current_View;
	if (graphic_view==NULL) return;

	pt.y = (graphic_view->m_oldRect).bottom - yin;
	pt.x = xin;
	graphic_view->ClientToScreen(&pt);
	*x = pt.x;
	*y = pt.y;
}

