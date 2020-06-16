/************************************************************************
c
c   FILE NAME: toolglobal.cpp
c
c	 CONTAINS: 
c		Defines the functions that can call from C function
c			but these function here will use MFC
c		getmcr (char *ldat, int*nc)
c		tool_mfmsg_box
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c        toolglobal.cpp , 25.5
c     DATE AND TIME OF LAST  MODIFICATION
c        11/10/16 , 10:20:40
c
c**********************************************************************
*/
#include "toolibstdafx.h"
#include <conio.h>
#include <winspool.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include "toolib.h"
#include "ToolPromptDlg.h"
#include "toolibcc.h"
#include "usysdef.h"
//#include "xenv1.h"
#include "xfsys1.h"

extern "C" char * uu_malloc(int);
extern "C" void uu_free( char* );
extern "C" void uw_ntMDFileOpen(char* title, char* ntflt, int nfld, int *ifld, char **ftext,
								char **fdir, char* fnam, int *nc, char *descrip, CWnd *MainWin, int flag);
extern "C" void uw_ntMDGetDir(char* title, int nfld, int *ifld, char **ftext,
								char **fdir, char* dnam, int *nc, CWnd *MainWin);
extern "C" int ux_search_for_path(char*, char*pathname, int options);
extern "C" char *ux_getenv(char *env, int options);
extern "C" void ul_break_fname(char *fullname,char *dir,char *fname);
extern "C" void ul_remove_quotes(char *buf);
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)

static int S_extend_path(char *extstr, char *paths, char *sub)
{
	char tempstr[UX_MAX_PATH_LEN*20], ext_dir[UX_MAX_PATH_LEN], dir[UX_MAX_PATH_LEN],
		dir1[UX_MAX_PATH_LEN], extstr2[UX_MAX_PATH_LEN*20], *svalue;
	char *indx, *tmp, *str, *last;
	int i, k, mode, status=0,ext=0;
	strcpy(tempstr, paths);

	status = ux_search_for_path(tempstr, ext_dir, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
	if (status==0)
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
	int len;
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
/*
.....add this to avoid recursive call to itself
*/
				if (stricmp(svalue, dir)==0)
					break;
				ext = S_extend_path(extstr2, svalue, sub); 
				strcat(extstr, extstr2);
			}
			else
			{
				indx = strchr (dir1, '\\');
				if ((dir[0]=='%')&&(indx!=NULL))
					*indx = 0;
/*
.....add this to avoid recursive call to itself
*/
				if (stricmp(dir1, dir)==0)
					break;
				ext = S_extend_path(extstr2, dir1, (char*)(indx+1)); 
				strcat(extstr, extstr2);
			}
		}
next:;
		if (last==NULL)
			break;
		len = strlen(last);
		str = strtok(last, ";");
		if (str!=NULL)
		{
			strcat (extstr, ";");
			last = str + strlen(str) + 1;
			ext = 1;
			if (strlen(str)>=len)
				last = NULL;
		}
	}
	tmp = strchr (extstr, '%');
	if (tmp==NULL)
		return ext;
	strcpy(extstr2, extstr);
/*
.....add this to avoid recursive call to itself
*/
	if (stricmp(extstr2, paths)==0)
	{
		return ext;
	}
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

/***********************************************************************
c
c   FUNCTION: getmcr (char *ldat, int*nc)
c
c         This function Get command input line
c
c   INPUT:  None
c
c   OUTPUT  ldat: command line string
c			nc: number of character of command line
c   RETURN:  None
c
**********************************************************************/
extern "C" int getmcr (char *ldat, int*nc)
{
	CWinApp* pApp;
	pApp = AfxGetApp();
	LPTSTR lpCmdLine;
	lpCmdLine = pApp->m_lpCmdLine;
#ifdef _UNICODE	
	*nc = wcslen (lpCmdLine);
	if (*nc>0)
	{
		wcstombs(ldat, lpCmdLine, *nc);
		ldat[*nc] = '\0';
	}
	else
		ldat[0] = '\0';
#else
	*nc = strlen (lpCmdLine);
	strcpy(ldat, lpCmdLine);
#endif
	return 0;
}

/***********************************************************************
c
c   FUNCTION: tool_mfmsg_box(CWnd* parent, char *title, char *msg, int flag)
c
c         This function display a message box
c
c   INPUT:  parent: parent window
c			title: title of the window
c			msg: message to be displayed
c			flag: 1: error message
c				  other: information message
c
c   OUTPUT  None
c			
c   RETURN:  None
c
**********************************************************************/
extern "C" int tool_mfmsg_box(CWnd* parent, char *title, char *msg, int flag)
{
	int result;
#ifdef _UNICODE	
	WCHAR *wtitle, *wmsg;
	int len1 = strlen (title) + 1;
	wtitle = new WCHAR[len1];
	int wlen1 = MultiByteToWideChar(CP_ACP, 0, title, -1, 
							wtitle, len1);
	int len2 = strlen (msg) + 1;
	wmsg = new WCHAR[len2];
	int wlen2 = MultiByteToWideChar(CP_ACP, 0, msg, -1, 
							wmsg, len2);
	if (parent==NULL)
	{
		CToolibApp *app = (CToolibApp*)AfxGetApp();
		CWnd *MainDlg = (CWnd*)(app->GetMainWnd());
		if (MainDlg==NULL)
		{
			MessageBox(NULL, wmsg, wtitle, MB_ICONINFORMATION|MB_OK);
			return 1;
		}
		if (flag==1)
			result = MainDlg->MessageBox(wmsg, wtitle, MB_ICONERROR|MB_OK);
		else
			result = MainDlg->MessageBox(wmsg, wtitle, MB_ICONINFORMATION|MB_OK);
	}
	else
	{
		if (flag==1)
			result = parent->MessageBox(wmsg, wtitle, MB_ICONERROR|MB_OK);
		else
			result = parent->MessageBox(wmsg, wtitle, MB_ICONINFORMATION|MB_OK);
	}
	delete wtitle; 
	delete wmsg;
#else
	if (parent==NULL)
	{
		CToolibApp *app = (CToolibApp*)AfxGetApp();
		CWnd *MainDlg = (CWnd*)(app->GetMainWnd());
		if (MainDlg==NULL)
		{
			MessageBox(NULL, msg, title, MB_ICONINFORMATION|MB_OK);
			return 1;
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
#endif
	return result;
}

/***********************************************************************
c
c   FUNCTION: dispmsg (char *msg, int *len)
c
c         This function display a message box
c
c   INPUT:  
c			msg: message to be displayed
c			len: number of character of message
c
c   OUTPUT  None
c			
c   RETURN:  None
c
**********************************************************************/
extern "C" int dispmsg (char *msg, int *len)
{
	msg[*len] = '\0';
	tool_mfmsg_box(NULL, "Tool Message", msg, 0);
	return 0;
}
		
/**********************************************************************
**    I_FUNCTION : tool_mfyesno(char *title, char* msg, CWnd *parent)
**			popup a YesNo question dialog box and get answer
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent widget
**				title:  dialog box title
**				msg:    dialog str.
**       OUTPUT :  
**         			none 
**    RETURNS      : 1:No
**					 0: Yes
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/

extern "C" int tool_mfyesno(char *title, char* msg,CWnd *parent)
{
	int result;
#ifdef _UNICODE	
	WCHAR *wtitle, *wmsg;
	int len1 = strlen (title) + 1;
	wtitle = new WCHAR[len1];
	int wlen1 = MultiByteToWideChar(CP_ACP, 0, title, -1, 
							wtitle, len1);
	int len2 = strlen (msg) + 1;
	wmsg = new WCHAR[len2];
	int wlen2 = MultiByteToWideChar(CP_ACP, 0, msg, -1, 
							wmsg, len2);
	if (parent==NULL)
	{
		CToolibApp *app = (CToolibApp*)AfxGetApp();
		CWnd *MainDlg = (CWnd*)(app->GetMainWnd());		
		result = MainDlg->MessageBox(wmsg, wtitle, MB_YESNO);
	}
	else
		result = parent->MessageBox(wmsg, wtitle, MB_YESNO);
	delete wtitle; 
	delete wmsg;
	if (result==IDYES)
		return 0;
	else 
		return 1;
#else
	if (parent==NULL)
	{
		CToolibApp *app = (CToolibApp*)AfxGetApp();
		CWnd *MainDlg = (CWnd*)(app->GetMainWnd());		
		result = MainDlg->MessageBox(msg, title, MB_YESNO);
	}
	else
		result = parent->MessageBox(msg, title, MB_YESNO);
	if (result==IDYES)
		return 0;
	else 
		return 1;
#endif
}

/**********************************************************************
**    I_FUNCTION : tool_mfyesnocancel(char *title, char* msg, CWnd *parent)
**			popup a YesNo question dialog box and get answer
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent widget
**				title:  dialog box title
**				msg:    dialog str.
**       OUTPUT :  
**         			none 
**    RETURNS      : 1:No
**					 0: Yes
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/

extern "C" int tool_mfyesnocancel(char *title, char* msg,CWnd *parent)
{
	int result;
#ifdef _UNICODE	
	WCHAR *wtitle, *wmsg;
	int len1 = strlen (title) + 1;
	wtitle = new WCHAR[len1];
	int wlen1 = MultiByteToWideChar(CP_ACP, 0, title, -1, 
							wtitle, len1);
	int len2 = strlen (msg) + 1;
	wmsg = new WCHAR[len2];
	int wlen2 = MultiByteToWideChar(CP_ACP, 0, msg, -1, 
							wmsg, len2);
	if (parent==NULL)
	{
		CToolibApp *app = (CToolibApp*)AfxGetApp();
		CWnd *MainDlg = (CWnd*)(app->GetMainWnd());		
		result = MainDlg->MessageBox(wmsg, wtitle, MB_YESNOCANCEL);
	}
	else
		result = parent->MessageBox(wmsg, wtitle, MB_YESNOCANCEL);
	delete wtitle; 
	delete wmsg;
	if (result==IDYES)
		return 0;
	else if (result==IDNO)
		return 1;
	else
		return -1;
#else
	if (parent==NULL)
	{
		CToolibApp *app = (CToolibApp*)AfxGetApp();
		CWnd *MainDlg = (CWnd*)(app->GetMainWnd());		
		result = MainDlg->MessageBox(msg, title, MB_YESNOCANCEL);
	}
	else
		result = parent->MessageBox(msg, title, MB_YESNOCANCEL);
	if (result==IDYES)
		return 0;
	else if (result==IDNO)
		return 1;
	else
		return -1;
#endif
}

/**********************************************************************
**    I_FUNCTION : tool_mfprompt(CWnd *parent, char *title, char *msg, int lns, int cols, char *ans_str)
**			popup a prompt dialog box 
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent widget
**				title:  prompt box title
**				msg:    prompt str.
**				ln: not used.
**				str_ans: user input in text field.
**       OUTPUT :  
**         			none 
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
extern "C" int tool_mfprompt(CWnd *parent, char *title, char *msg, int lns, int cols, char *ans_str)
{
	int result;
	CToolPromptDlg *promptdlg;
	if (parent==NULL)
	{
		CToolibApp *app = (CToolibApp*)AfxGetApp();
		CWnd *MainDlg = (CWnd*)(app->GetMainWnd());	
		promptdlg = new CToolPromptDlg(MainDlg);
	}
	else
		promptdlg = new CToolPromptDlg(parent);
	promptdlg->SetDlgValue(title, msg, ans_str);
	result = promptdlg->DoModal();
	promptdlg->GetInput(ans_str, 1000);
	return result;
}

/**********************************************************************
**    I_FUNCTION : tool_mf_filename(CWnd* parent, char *title, char *ext, LPCTSTR filter, char* fnam, int *nc)
**			popup a prompt dialog box 
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent widget
**				title:  prompt box title
**				ext:    file extension, if user input a name without
**						an extension, if will add the extension
**				filter: file filter.
**				fname:  default file name
**				nc:     number of character of filename
**       OUTPUT :  
**				fname:  use enter file name
**				nc:     number of character of filename
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
extern "C" int tool_mf_filename(CWnd* parent, char *title, char *ext, LPCTSTR filter, char* fnam, int *nc)
{
	DWORD dwFlags;
	dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT |
		OFN_ENABLESIZING;
	CFileDialog *filedlg;
#ifdef _UNICODE	
	WCHAR *wtitle, *wext;
	int len1 = strlen (title) + 1;
	wtitle = new WCHAR[len1];
	int wlen1 = MultiByteToWideChar(CP_ACP, 0, title, -1, 
							wtitle, len1);
	int len2 = strlen (ext) + 1;
	wext = new WCHAR[len2];
	int wlen2 = MultiByteToWideChar(CP_ACP, 0, ext, -1, 
							wext, len2);
	if (parent!=NULL)
	{
		filedlg = new CFileDialog(TRUE, wext, NULL, dwFlags,
			filter, parent);
		filedlg->m_ofn.lpstrTitle = wtitle;
	}
	else
	{
		CToolibApp *app = (CToolibApp*)AfxGetApp();
		CWnd *MainDlg = (CWnd*)(app->GetMainWnd());	
		filedlg = new CFileDialog(TRUE, wext, NULL, dwFlags,
			filter, MainDlg);
		filedlg->m_ofn.lpstrTitle = wtitle;
	}
	if (filedlg->DoModal()==IDCANCEL)
	{
		*nc = 0;
		delete wtitle; 
		delete wext;
		delete filedlg;
		return 0;
	}
	CString FileName = filedlg->GetPathName();
	*nc = FileName.GetLength();
	if (*nc>0)
	{
		WCHAR *wfile = FileName.GetBuffer(*nc);
		wcstombs(fnam, wfile, *nc);
		fnam[*nc] = '\0';
	}
	else
		fnam[0] = '\0';
	delete wtitle; 
	delete wext;
	delete filedlg;
#else
	if (parent!=NULL)
	{
		filedlg = new CFileDialog(TRUE, ext, NULL, dwFlags,
			filter, parent);
		filedlg->m_ofn.lpstrTitle = title;
	}
	else
	{
		CToolibApp *app = (CToolibApp*)AfxGetApp();
		CWnd *MainDlg = (CWnd*)(app->GetMainWnd());	
		filedlg = new CFileDialog(TRUE, ext, NULL, dwFlags,
			filter, MainDlg);
		filedlg->m_ofn.lpstrTitle = title;
	}
	if (filedlg->DoModal()==IDCANCEL)
	{
		*nc = 0;
		return 0;
	}
	CString FileName = filedlg->GetPathName();
	*nc = FileName.GetLength();
	char* temp = FileName.GetBuffer(*nc);
	strcpy(fnam, temp);
	delete filedlg;
#endif
	return 0;
}
extern "C" int tool_md_filename(CWnd* parent, char *title, LPCTSTR filter, int nfld, int *ifld, char **ftext, 
							char **fdir, char *fnam, int *nc, char *descrip, int flag)
{
	int i,j;
	UX_pathname ntflt, tempfilt, tempdesp,lfilter,ldescript, ofilter;
	char *fltpt, *desppt;
	char* temp;
	CString FileName ;
	int win_enable;
	int status = 0;

	win_enable = 0;
/*
.....save current directory in case filebrowser change it
*/
	WCHAR save_dir[UX_MAX_PATH_LEN];
	GetCurrentDirectory(UX_MAX_PATH_LEN, save_dir);

	CWnd *MainWin;
	if (parent != NULL)
		MainWin = parent;
	else
	{
		MainWin = AfxGetMainWnd();
	}
	int len = wcslen (filter);
	wcstombs(lfilter, filter, len+1);
	lfilter[len] = '\0';
	strcpy_s(ldescript, UX_MAX_PATH_LEN, descrip);

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
	if (filter!=_T("*.*"))
	{
		strcat_s(ntflt, "All Files (*.*)|*.*|");
	}
	strcat_s(ntflt, "|");

	strcpy(lfilter, ntflt);
/*
.....Enable dialog
*/
	CWnd *framewin = AfxGetMainWnd();
	if (MainWin!=framewin)
	{
		win_enable = framewin->IsWindowEnabled();
		if (win_enable)
		{
			::EnableWindow(framewin->GetSafeHwnd(), FALSE);
		}
	}
	uw_ntMDFileOpen(title, lfilter, nfld, ifld, ftext,
								fdir, fnam, nc, descrip, MainWin, flag);
done:;
	if (win_enable)
	{
		::EnableWindow(framewin->GetSafeHwnd(), TRUE);
	}
	_wchdir(save_dir);
	return 0;
}
extern "C" int tool_mfopt_filename(CWnd* parent, char *title, LPCTSTR filter, char *indlocal, char *indsys, 
		char* fnam, int *nc, int flag)
{
	int ierror;
	int i,j;
	UX_pathname ntflt, tempfilt, tempdesp,lfilter,ldescript;
	char *fltpt, *desppt;

	char *p;
	UX_pathname direc, dlocal, dsys;
	WCHAR save_dir[UX_MAX_PATH_LEN];
	UX_pathname defdirec;
	GetCurrentDirectory(UX_MAX_PATH_LEN, save_dir);
	int len = wcslen (save_dir);
	wcstombs(defdirec, save_dir, UX_MAX_PATH_LEN);
	defdirec[len] = '\0';
	if (indlocal!=NULL)
	{
		p = tool_getenv(indlocal);
		if (p != 0) 
			strcpy(direc,p);
		else
			strcpy(direc,indlocal);
		if ((tool_get_dir(direc,dlocal)==0)
			|| ((dlocal[0] == '\0')||(dlocal[0] == '.')))
			strcpy(dlocal, defdirec);
	}
	else
		strcpy(dlocal, defdirec);

	if (indsys!=NULL)
	{
		p = tool_getenv(indsys);
		if (p != 0) 
			strcpy(direc,p);
		else 
			strcpy(direc,indsys);
		if ((tool_get_dir(direc, dsys)==0)
			|| ((dsys[0] == '\0')||(dsys[0] == '.')))
		strcpy(dsys, defdirec);
	}
	else
		strcpy(dsys, defdirec);

	CWnd *MainWin;
	if (parent != NULL)
		MainWin = parent;
	else
	{
		MainWin = AfxGetMainWnd();
	}
	int ifld = 0;
	char *ftext[20], *fdir[20];
	ftext[0] = (char*) uu_malloc((41)*sizeof (char));
	ftext[1] = (char*) uu_malloc((41)*sizeof (char));
	strcpy(ftext[0], "System");
	strcpy(ftext[1], "Local");
	
	len = strlen(dsys);
	fdir[0] = (char*) uu_malloc((len+1)*sizeof (char));
	strncpy(fdir[0], dsys, len);
	fdir[0][len] = '\0';
	
	len = strlen(dlocal);
	fdir[1] = (char*) uu_malloc((len+1)*sizeof (char));
	strncpy(fdir[1], dlocal, len);
	fdir[1][len] = '\0';

	tool_md_filename(MainWin, title, filter, 2, &ifld, ftext, fdir, fnam, nc, "Tool Library Files (*.TLB)", flag);
	uu_free(ftext[0]);
	uu_free(ftext[1]);
	uu_free(fdir[0]);
	uu_free(fdir[1]);
	_wchdir(save_dir);
	return 0;
}

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
**    I_FUNCTION : uw_ntmd_dirname(parent,title,int nfld, int *ifld, ftext,fdir,dirname,nc)
**       Opens a File Selection dialog and returns the user selected
**			directory name. 
**    PARAMETERS   
**       INPUT  : 
**          title     = Title of File Selection dialog.
**			nfld      = Number of directory toggle fields.
**			ifld      = Default directory toggle field.
**			ftext     = Text for directory toggle fields.
**			fdir      = Actual directories for toggle fields.
**			dirname  = Default directory name.
**       OUTPUT :  
**          dirname  = Name of selected directory.
**			nc        = Number of chars in 'dirname'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int tool_md_dirname(CWnd* parent, char *title, int nfld, int *ifld, char **ftext, 
							char **fdir, char *dirname, int *nc)
{
	int i,j;
	UX_pathname ntflt, tempfilt, tempdesp,lfilter,ldescript;
	char *fltpt, *desppt;
	char* temp;
	CString FileName ;
	int win_enable;
	int status = 0;

	win_enable = 0;
/*
.....save current directory in case filebrowser change it
*/
	WCHAR save_dir[UX_MAX_PATH_LEN];
	GetCurrentDirectory(UX_MAX_PATH_LEN, save_dir);
/*
.....Get parent window
*/
	CWnd *MainWin;
	if (parent != NULL)
		MainWin = parent;
	else
	{
		MainWin = AfxGetMainWnd();
	}
/*
.....Enable dialog
*/
	CWnd *framewin = AfxGetMainWnd();
	if (MainWin!=framewin)
	{
		win_enable = framewin->IsWindowEnabled();
		if (win_enable)
		{
			::EnableWindow(framewin->GetSafeHwnd(), FALSE);
		}
	}
	uw_ntMDGetDir(title, nfld, ifld, ftext,
								fdir, dirname, nc, MainWin);
done:;
	if (win_enable)
	{
		::EnableWindow(framewin->GetSafeHwnd(), TRUE);
	}
	_wchdir(save_dir);
	return 0;
}
void tool_get_dirname2(CWnd* parent, char *title, char* dirname, int *nc)
{
	int win_enable;
	int status = 0;

	win_enable = 0;
/*
.....save current directory in case filebrowser change it
*/
	WCHAR save_dir[UX_MAX_PATH_LEN];
	GetCurrentDirectory(UX_MAX_PATH_LEN, save_dir);
/*
.....Get parent window
*/
	CWnd *MainWin;
	if (parent != NULL)
		MainWin = parent;
	else
	{
		MainWin = AfxGetMainWnd();
	}
/*
.....Enable dialog
*/
	CWnd *framewin = AfxGetMainWnd();
	if (MainWin!=framewin)
	{
		win_enable = framewin->IsWindowEnabled();
		if (win_enable)
		{
			::EnableWindow(framewin->GetSafeHwnd(), FALSE);
		}
	}
	uw_ntMDGetDir(title, 0, NULL, NULL, NULL, dirname, nc, MainWin);
done:;
	if (win_enable)
	{
		::EnableWindow(framewin->GetSafeHwnd(), TRUE);
	}
	_wchdir(save_dir);
}

/**********************************************************************
**    I_FUNCTION : tool_get_dirname(CWnd* parent, char *title, char* dnam, int *nc)
**			popup a file browser dialog box 
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent window
**				title:  file browser title
**				dname:  default dir name
**				nc:     number of character of dname
**			    paths: paths put into directory dropbox of file browser
**					this paths allow include the env value (add % before the env value)
**		    	path_des: description of paths in directory dropbox of file browser
**       OUTPUT :  
**				dname:  directory name
**				nc:     number of character of filename
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
extern "C" void tool_get_dirname(CWnd* parent, char *title, char* dnam, int *nc,
	char *paths, char *path_des)
{
	int nfld, ifld, len, len1, add;
	char *ftext[20], *fdir[20];

	char tempstr[UX_MAX_PATH_LEN*20], c_dir[UX_MAX_PATH_LEN];
	char* temp, *tok;
	int i,j, status;

	if ((paths==NULL)||(paths!=NULL)&&(paths[0]=='\0'))
		return tool_get_dirname2(parent, title, dnam, nc);

	status = S_extend_path(tempstr, paths, NULL);
	if (tempstr[0]=='\0')
		return tool_get_dirname2(parent, title, dnam, nc);

	temp = strrchr (tempstr, ';');
	if (temp==NULL)
	{
		if ((path_des==NULL)||(path_des!=NULL)&&(path_des[0]=='\0'))
			return tool_get_dirname2(parent, title, dnam, nc);
	}
	tok = strtok (tempstr, ";");
	if (tok==NULL)
		return tool_get_dirname2(parent, title, dnam, nc);
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
	WCHAR wdir[UX_MAX_PATH_LEN];
	GetCurrentDirectory(UX_MAX_PATH_LEN, wdir);
	len = wcslen (wdir);
	wcstombs(c_dir, wdir, len);
	c_dir[len] = '\0';

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
	tool_md_dirname(parent, title, nfld, &ifld, ftext, fdir, dnam, nc);
	for (i=0; i<20;i++)
	{
		if (fdir[i] != NULL)
			uu_free(fdir[i]);
		uu_free(ftext[i]);
	}
}
/**********************************************************************
**    I_FUNCTION : tool_get_filename2(CWnd* parent, char *title, LPCTSTR filter, char* fnam, int *nc)
**			popup a simple file browser dialog box 
**			
**    PARAMETERS   
**       INPUT  : 
**				parent: parent window
**				title:  file browser title
**				filter: file filter.
**				fname:  default file name
**				nc:     number of character of filename
**				descript = File type description
**				open_flag: TRUE: open file browser as "Open"
**							FALSE: open file browser as "SAve as"
**       OUTPUT :  
**				fname:  use enter file name
**				nc:     number of character of filename
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
extern "C" void tool_get_filename2(CWnd* parent, char *title, char * filter, char* fnam, int *nc, char *descrip, int open_flag)
{
	DWORD dwFlags;
	UX_pathname ntflt, tempfilt, tempdesp, lfilter, ldescript, fnam_temp, dir, fnam_only,filt_dir;
	char *fltpt, *desppt;
	char* temp, *indx, first_exe[80];
	CString FileName ;
	int i,j, len, status,win_enable;
	dwFlags = OFN_NOCHANGEDIR | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_ENABLESIZING;
	CFileDialog *filedlg = NULL;
	win_enable = 0;
/*
.....save current directory in case filebrowser change it
*/
	WCHAR save_dir[UX_MAX_PATH_LEN];
	GetCurrentDirectory(UX_MAX_PATH_LEN, save_dir);
/*
.....Get parent window
*/
	CWnd *MainWin;
	if (parent != NULL)
		MainWin = parent;
	else
	{
		MainWin = AfxGetMainWnd();
	}
	filt_dir[0] = '\0';
/*
.....the filter may include directory
.....remove directory
*/
	indx = strrchr(filter, '\\');
	if (indx!=0)
	{
		strcpy_s(lfilter, UX_MAX_PATH_LEN, indx+1);
		strcpy_s(filt_dir, UX_MAX_PATH_LEN, filter);
		indx = strrchr(filt_dir, '\\');
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
	CWnd *framewin = AfxGetMainWnd();
	if (MainWin!=framewin)
	{
		win_enable = framewin->IsWindowEnabled();
		if (win_enable)
		{
			::EnableWindow(framewin->GetSafeHwnd(), FALSE);
		}
	}
/*
.....Create new dialog
*/
/*	filedlg = new CFileDialog(open_flag, NULL, fnam, dwFlags,
		ntflt, MainWin);
	filedlg->m_ofn.lpstrTitle = title;
	filedlg->m_ofn.nMaxFileTitle = strlen(title);
*/
	WCHAR *wfnam, *wtitle, *wntflt;
	len = strlen (fnam) + 1;
	wfnam = new WCHAR[UX_MAX_PATH_LEN];
	MultiByteToWideChar(CP_ACP, 0, fnam, -1, 
							wfnam, len);

	len = strlen (ntflt) + 1;
	wntflt = new WCHAR[len];
	MultiByteToWideChar(CP_ACP, 0, ntflt, -1, 
							wntflt, len);

	len = strlen (title) + 1;
	wtitle = new WCHAR[len];
	MultiByteToWideChar(CP_ACP, 0, title, -1, 
							wtitle, len);

	filedlg = new CFileDialog(open_flag, NULL, wfnam, dwFlags,
		wntflt, MainWin);
	filedlg->m_ofn.lpstrTitle = wtitle;
	filedlg->m_ofn.nMaxFileTitle = wcslen(wtitle);

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
		if (filt_dir[0]!='\0')
		{
			strcpy(fnam_temp, filt_dir);
			strcat(fnam_temp, "\\");
			strcat(fnam_temp, fnam_only);
			len = strlen (fnam_only) + 1;
			MultiByteToWideChar(CP_ACP, 0, fnam_only, -1, 
							wfnam, len);
			filedlg->m_ofn.lpstrFile = wfnam;		
		}
		else
			filedlg->m_ofn.lpstrFile = save_dir;
	}
/*
.....Get dialog input
*/
	if (filedlg->DoModal()==IDCANCEL)
	{
		*nc = 0;
		fnam[0] = '\0';
		goto done;
	}
	FileName = filedlg->GetPathName();
	*nc = FileName.GetLength();
	if (*nc>0)
	{
		WCHAR *wfile = FileName.GetBuffer(*nc);
		wcstombs(fnam, wfile, *nc);
		fnam[*nc] = '\0';
	}
	else
		fnam[0] = '\0';
done:;
	if (filedlg!=NULL)
		delete filedlg;
	delete wfnam;
	delete wtitle;
	delete wntflt;
	if (win_enable)
		::EnableWindow(framewin->GetSafeHwnd(), TRUE);
	_wchdir(save_dir);
}
/**********************************************************************
**    I_FUNCTION : tool_get_filename(CWnd* parent, char *title, char * filter, char* fnam, int *nc, char *descrip, int open_flag,
**				char *paths, char *path_des)
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
**			paths: paths put into directory dropbox of file browser
**					this paths allow include the env value (add % before the env value)
**			path_des: description of paths in directory dropbox of file browser
**       OUTPUT :  
**				fname:  use enter file name
**				nc:     number of character of filename
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
extern "C" void tool_get_filename(CWnd* parent, char *title, char * filter, char* fnam, int *nc, char *descrip, int open_flag,
	char *paths, char *path_des)
{
	int nfld, ifld, len, len1, add;
	char *ftext[20], *fdir[20];

	char tempstr[UX_MAX_PATH_LEN*20], c_dir[UX_MAX_PATH_LEN];
	char* temp, *tok;
	int i,j, status;

	if ((paths==NULL)||(paths!=NULL)&&(paths[0]=='\0'))
		return tool_get_filename2(parent, title, filter, fnam, nc, descrip, open_flag);

	status = S_extend_path(tempstr, paths, NULL);
	if (tempstr[0]=='\0')
		return tool_get_filename2(parent, title, filter, fnam, nc, descrip, open_flag);

	temp = strrchr (tempstr, ';');
	if (temp==NULL)
	{
		if ((path_des==NULL)||(path_des!=NULL)&&(path_des[0]=='\0'))
			return tool_get_filename2(parent, title, filter, fnam, nc, descrip, open_flag);
	}
	tok = strtok (tempstr, ";");
	if (tok==NULL)
		return tool_get_filename2(parent, title, filter, fnam, nc, descrip, open_flag);
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
	WCHAR wdir[UX_MAX_PATH_LEN];
	GetCurrentDirectory(UX_MAX_PATH_LEN, wdir);
	len = wcslen (wdir);
	wcstombs(c_dir, wdir, len);
	c_dir[len] = '\0';

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

	WCHAR *wfilter;
	len = strlen (filter) + 1;
	wfilter = new WCHAR[len];
	MultiByteToWideChar(CP_ACP, 0, filter, -1, 
							wfilter, len);

	tool_md_filename(parent, title, wfilter, nfld, &ifld, ftext, fdir, fnam, nc, descrip, open_flag);
	for (i=0; i<20;i++)
	{
		if (fdir[i] != NULL)
			uu_free(fdir[i]);
		uu_free(ftext[i]);
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
	char fext[UX_SUFFIX_LEN];
	
	WCHAR save_dir[UX_MAX_PATH_LEN];
	GetCurrentDirectory(UX_MAX_PATH_LEN, save_dir);

	ul_remove_quotes(pathname);
	strcpy_s(fext, "*.");
	strcat_s(fext, ftype);

	WCHAR wpath[UX_MAX_PATH_LEN], wext[40];
	int len = strlen (pathname) + 1;
	MultiByteToWideChar(CP_ACP, 0, pathname, -1, 
							wpath, len+1);	
	len = strlen (fext) + 1;
	MultiByteToWideChar(CP_ACP, 0, fext, -1, 
							wext, len+1);	
	if (_waccess(wpath, 0)!=0)
		return -1;
	
	_wchdir(wpath);
	CFileFind finder;
	char file[UX_MAX_PATH_LEN];
	WCHAR *wfile;
    BOOL stat = finder.FindFile(wext);

	while (stat)
	{
		stat = finder.FindNextFile();
		CString file1 = finder.GetFilePath();
		*flist = uu_lsinsrt(*flist,UX_MAX_PATH_LEN);
		len = file1.GetLength();
		if (len>0)
		{
			wfile = file1.GetBuffer(UX_MAX_PATH_LEN);
			wcstombs(file, wfile, len);
			file[len] = '\0';
		}
		else
			file[0] = '\0';
		uu_move_byte(file, *flist, UX_MAX_PATH_LEN);
	}
	_wchdir(save_dir);
	return 0;
}


