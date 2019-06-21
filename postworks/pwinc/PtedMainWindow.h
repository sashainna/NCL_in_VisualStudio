/************************************************************************
c
c   FILE NAME: PtedMainWindow.h
c
c	 Description - Functions and struct declarations for
c		CDialogTemplate class (Dynamic Dialog)
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PtedMainWindow.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:34
c
c**********************************************************************
*/


#ifndef PTEDMAINWINDOW_H
#define PTEDMAINWINDOW_H

#include <AFXPRIV.H>
#include "pwenv.h"
#include "PtedWindow.h"

class CPtedMainWindow : public CPtedWindow
{
public:
	CPtedMainWindow(char *filename, char flist[5][UX_MAX_PATH], int actcom = 0);
	~CPtedMainWindow();

	void Remove_Child(int id);
	void Remove_Child(CDialog *dlg);
	void Add_Child(CDialog* dlg);
	void SetCutterIndex(CDialog* dlg);
	void ResetCutterIndex(CDialog* dlg);
	void LoadCutterData();
	void Decre_untitle();
	int Get_subwindow_name(char *ofile, char *infile, char *fname, char *fext);
	void ViewStatus(CWnd *parent);
	void SetPreFile();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPtedMainWindow)
	protected:
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL
protected:

	CEdit m_Ecommand;
	int m_flist;
	char m_filelist[5][UX_MAX_PATH];

	CDialog **m_pModeless;
	int m_CutterDialogIndex;
	static int m_num;

	// Generated message map functions
	//{{AFX_MSG(CPtedMainWindow)
	virtual void OnCancel();
	afx_msg void OnFileNew();
	afx_msg void DlgQuit();
	afx_msg void On_Window_Backup();
	afx_msg void OnHelpContents();
	afx_msg void OnHelpAbout();
	afx_msg void OnFileRunmpost();

	afx_msg void OnFileEditinput();
	afx_msg void OnFileEditoutput();
	afx_msg void OnEditEditinput();
	afx_msg void OnEditEditoutput();
	afx_msg void OnFileLoadinputmdf();
	afx_msg void OnFileLoadoutputmdf();
	afx_msg void OnFileLoadmdf();

	virtual void OnFileOpen();
	afx_msg void OnFileClose();
	afx_msg void OnOptionPworksoption();
	afx_msg void OnConvertRunpworks();
	afx_msg void OnRunCommand();
	afx_msg void OnFileFileList(UINT id);
	virtual void OnClose();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif

