/*********************************************************************
**  NAME:  SwUniOpt.cpp
**
**       File browser routines.
**
** CONTAINS: CUniOpt class functions
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwUniOpt.cpp , 21.1
**    DATE AND TIME OF LAST  MODIFICATION
**       12/10/09 , 18:02:16
*********************************************************************/
// UniOpt.cpp : Implementation of CUniOpt
#include "SwStdAfx.h"
#include "SwUniOpt.h"
#include "commdlg.h"

/*********************************************************************
**    I_FUNCTION     :  GetFileName(dir,file,nc,h_wnd,flag)
**       Displays a file browser and returns the filename selected.
**    PARAMETERS
**       INPUT  :
**          dir      = Default directory for file browser.
**          file     = Default filename for file browser.
**          h_wnd    = Parent window.
**          flag     = 0 = Get file for reading, 1 = Writing.
**       OUTPUT :
**          file     = User selected filename.
**          nc       = Number of characters in 'file'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/////////////////////////////////////////////////////////////////////////////
// CUniOpt

void CUniOpt::GetFileName(char *dir, char *file,int *nc,HWND h_wnd,int flag)
{
	int stat,i,nc1;
	OPENFILENAME ofn;
	TCHAR btitle[80],bfilter[80],bfile[1024],bdir[1024];
//
//...Define the Filter
//
//	static char szFilter[] = {"Unibase files (*.u)\0*.u\0Textual Unibase files (*.ud)\0*.ud\0All Files (*.*)\0*.*\0\0"};
	static char szFilter[] = {"Unibase files (*.u)|*.u|Textual Unibase files (*.ud)|*.ud|All Files (*.*)|*.*||"};
//
//...Initialize the filename structure
//
	ofn.lStructSize=sizeof(ofn);
	ofn.hwndOwner=h_wnd;
	nc1 = strlen(szFilter);
	mbstowcs(bfilter,szFilter,nc1+1);
	for (i=0;i<nc1;i++) if (bfilter[i] == '|') bfilter[i] = '\0';
	ofn.lpstrFilter=bfilter;
	ofn.lpstrCustomFilter=NULL;
	ofn.nMaxCustFilter = 0;
	ofn.nFilterIndex=0;
// UNICODE
//	ofn.lpstrFile=file;
	mbstowcs(bfile,file,strlen(file)+1);
	ofn.lpstrFile=bfile;
	ofn.nMaxFile=1024;
	ofn.lpstrFileTitle=NULL;
// UNICODE
//	if (strlen(file) == 0) ofn.lpstrInitialDir=dir;
	if (strlen(dir) == 0)
	{
		mbstowcs(bdir,dir,strlen(dir)+1);
		ofn.lpstrInitialDir=bdir;
	}
	else ofn.lpstrInitialDir=NULL;
	ofn.lpstrDefExt=NULL;
//
//...Display the browser
//......Open for reading
//
	if (flag == 0)
	{
		mbstowcs(btitle,rtitle,strlen(rtitle)+1);
		ofn.lpstrTitle = btitle;
		ofn.nMaxFileTitle = strlen(rtitle);
		ofn.Flags = OFN_FILEMUSTEXIST | OFN_HIDEREADONLY | OFN_PATHMUSTEXIST;
		stat = GetOpenFileName(&ofn);
	}
//
//......Open for saving
//
	else
	{
		mbstowcs(btitle,wtitle,strlen(wtitle)+1);
		ofn.lpstrTitle = btitle;
		ofn.nMaxFileTitle = strlen(wtitle);
		ofn.Flags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY;
		stat = GetSaveFileName(&ofn);
	}
//
//...User cancelled dialog
//
	if (stat == 0)
	{
		*nc = 0;
	}
//
//...Return the filename
//...Make sure it has a valid extension
//
	else
	{
		*nc = wcslen(ofn.lpstrFile);
		wcstombs(file,ofn.lpstrFile,*nc+1);
		if (ofn.nFileExtension == 0 || file[ofn.nFileExtension] == '\0')
		{
			if (ofn.nFilterIndex == 2) strcat(file,".ud");
			else strcat(file,".u");
		}
		*nc = strlen(file);
	}
//
//...End of routine
//
	return;
}

int CUniOpt::FileExists(char *title, char *file)
{
	char sbuf[1024];
	TCHAR bstr[1024],btitle[80];
	sprintf(sbuf,"%s already exists.\nDo you want to replace it?",file);
	mbstowcs(bstr,sbuf,strlen(sbuf)+1);
	mbstowcs(btitle,title,strlen(title)+1);
	return(MessageBox(bstr,btitle,MB_YESNO|MB_ICONEXCLAMATION));
}
