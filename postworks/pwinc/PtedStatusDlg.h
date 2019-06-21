 /************************************************************************
c
c   FILE NAME: PtedStatusDlg.h
c
c	 Description - Functions and struct declarations for
c		CPtedStatusDlg class (Windows for display status)
c		
c     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PtedStatusDlg.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:35
c
c**********************************************************************
*/

#ifndef PTDSTATUSDLG_H
#define PTDSTATUSDLG_H
#include "pwenv.h"

typedef struct 
{
	int type;
	char filename[UX_MAX_PATH];
	int wtype;
} Ptd_File_status;

class CPtedStatusDlg : public CDialog
{
public:
	CPtedStatusDlg(CWnd* pParent = NULL, Ptd_File_status *files = NULL, int file_num=0, char * input = NULL, char* output=NULL);
	~CPtedStatusDlg();

// Dialog Data
	//{{AFX_DATA(CPtedStatusDlg)
	enum { IDD = IDD_STATUS_DIALOG };
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPtedStatusDlg)
	protected:
	//}}AFX_VIRTUAL
public:
	Ptd_File_status *m_files;
	int m_filenum; 
	char m_input_mdf[UX_MAX_PATH], m_output_mdf[UX_MAX_PATH];

protected:

	HICON m_hIcon;
	// Generated message map functions
	//{{AFX_MSG(CPtedStatusDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif

