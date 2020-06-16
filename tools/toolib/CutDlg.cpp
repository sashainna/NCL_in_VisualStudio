/************************************************************************
c
c   FILE NAME: CutDlg.cpp
c
c	 CONTAINS: 
c		Functions for CCutDlg class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c       CutDlg.cpp , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/12/15 , 17:22:03
c
c**********************************************************************
*/
// CutDlg.cpp : implementation file
//

#include "toolibstdafx.h"
#include "toolib.h"
#include "CutDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CCutDlg dialog

/***********************************************************************
c
c   FUNCTION: CCutDlg(CWnd* pParent)
c
c              Constructor of class CCutDlg
c
c   INPUT:  pParent: parent window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

CCutDlg::CCutDlg(CWnd* pParent /*=NULL*/, int type)
	:CDialog(CCutDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CCutDlg)
	m_type = type;
	m_def1 = _T("");
	m_def2 = _T("");
	m_def3 = _T("");
	m_def4 = _T("");
	m_def5 = _T("");
	m_def6 = _T("");
	m_parm1 = _T("");
	m_parm2 = _T("");
	m_parm3 = _T("");
	m_parm4 = _T("");
	m_parm5 = _T("");
	m_parm6 = _T("");
	//}}AFX_DATA_INIT
}

/***********************************************************************
c
c   FUNCTION: DoDataExchange(CDataExchange* pDX)
c
c         Called by the framework to exchange and validate dialog data.
c
c   INPUT:  pDX   A pointer to a CDataExchange object.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CCutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CCutDlg)
	DDX_Text(pDX, IDC_EDIT_DEF1, m_def1);
	DDX_Text(pDX, IDC_EDIT_DEF2, m_def2);
	DDX_Text(pDX, IDC_EDIT_DEF3, m_def3);
	DDX_Text(pDX, IDC_EDIT_DEF4, m_def4);
	DDX_Text(pDX, IDC_EDIT_DEF5, m_def5);
	DDX_Text(pDX, IDC_EDIT_DEF6, m_def6);
	DDX_Text(pDX, IDC_EDIT_PARM1, m_parm1);
	DDX_Text(pDX, IDC_EDIT_PARM2, m_parm2);
	DDX_Text(pDX, IDC_EDIT_PARM3, m_parm3);
	DDX_Text(pDX, IDC_EDIT_PARM4, m_parm4);
	DDX_Text(pDX, IDC_EDIT_PARM5, m_parm5);
	DDX_Text(pDX, IDC_EDIT_PARM6, m_parm6);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CCutDlg, CDialog)
	//{{AFX_MSG_MAP(CCutDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CCutDlg message handlers

/***********************************************************************
c
c   FUNCTION: OnInitDialog() 
c
c         This member function is called in response to 
c		the WM_INITDIALOG message. This message is sent to 
c		the dialog box during the Create, CreateIndirect, 
c		or DoModal calls, which occur immediately before 
c		the dialog box is displayed. 
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CCutDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
/*
......changed to "All fields are enabled when 
......defining Pseudo Cutter"
*/
/*
......enable/disable fields
*/
/*************************
	if ((m_type==0)||(m_type==1))
/*
......Face Mill or End Mill
......Diam   Cr   Height
*/
/*************************
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF3))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM3))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC3))->EnableWindow(TRUE);

		((CWnd*)GetDlgItem(IDC_EDIT_DEF4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF7))->EnableWindow(FALSE);

		((CWnd*)GetDlgItem(IDC_EDIT_PARM4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM7))->EnableWindow(FALSE);
		
		((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC7))->EnableWindow(FALSE);
	}
	else if (m_type==2)
/*
......Barrel
......Diam   Cr   Height   Side rad    Z Height    Flat ang
*/
/*************************
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF3))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM3))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC3))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF5))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF6))->EnableWindow(TRUE);		
		((CWnd*)GetDlgItem(IDC_EDIT_DEF7))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM5))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM6))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM7))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC5))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC7))->EnableWindow(TRUE);

		((CWnd*)GetDlgItem(IDC_EDIT_DEF4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(FALSE);
	}
	else if ((m_type==3)||(m_type==4)||(m_type==9))
/*
......Cone, Bell
......Diam   Cr   Height   Side ang
......Blade          
......Width(Diam)  Chizel(Cr)  Height   Angle
*/
/*************************
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF3))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM3))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC3))->EnableWindow(TRUE);

		((CWnd*)GetDlgItem(IDC_EDIT_DEF4))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF7))->EnableWindow(FALSE);

		((CWnd*)GetDlgItem(IDC_EDIT_PARM4))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM7))->EnableWindow(FALSE);
		
		((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC7))->EnableWindow(FALSE);
	}
	else if ((m_type==5)||(m_type==8))
/*
......Drill, Chamfer
......Diam          Height   Side ang
*/
/*************************
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF2))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF3))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM2))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM3))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC3))->EnableWindow(TRUE);

		((CWnd*)GetDlgItem(IDC_EDIT_DEF4))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF7))->EnableWindow(FALSE);

		((CWnd*)GetDlgItem(IDC_EDIT_PARM4))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM7))->EnableWindow(FALSE);
		
		((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC7))->EnableWindow(FALSE);
	}
	else if ((m_type==6)||(m_type==7))
/*
......Boring, Reamer
......Diam          Height
*/
/*************************
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF3))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM3))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC3))->EnableWindow(FALSE);

		((CWnd*)GetDlgItem(IDC_EDIT_DEF4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF7))->EnableWindow(FALSE);

		((CWnd*)GetDlgItem(IDC_EDIT_PARM4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM7))->EnableWindow(FALSE);
		
		((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC7))->EnableWindow(FALSE);
	}
	else if ((m_type==10)||(m_type==11)||(m_type==12))
/*
......Lathe Bit, Threading, Grooving          
......Diam
*/
/*************************
	{
		((CWnd*)GetDlgItem(IDC_EDIT_DEF1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF2))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF3))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM2))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM3))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC3))->EnableWindow(FALSE);

		((CWnd*)GetDlgItem(IDC_EDIT_DEF4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_DEF7))->EnableWindow(FALSE);

		((CWnd*)GetDlgItem(IDC_EDIT_PARM4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_EDIT_PARM7))->EnableWindow(FALSE);
		
		((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC5))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_STATIC7))->EnableWindow(FALSE);
	}	
***********/	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
