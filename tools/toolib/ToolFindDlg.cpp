
/********************************************************************* 
**  NAME:  ToolFindDlg.cpp
**
**			Contains the Search dialog class.
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			ToolFindDlg.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			10/12/15 , 17:27:20
*********************************************************************/
// ToolFindDlg.cpp : implementation file
//

#include "toolibstdafx.h"
#include "toolib.h"
#include "ToolFindDlg.h"
#include "MainFrm.h"
#include "toolibcc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern "C" int tool_findtool(char *find_str, int *pos);
/////////////////////////////////////////////////////////////////////////////
// CToolFindDlg dialog


CToolFindDlg::CToolFindDlg(CWnd* pParent, CString defstr)
	: CDialog(CToolFindDlg::IDD, pParent)
{
	m_parent = pParent;
	m_find_str = defstr;
	//{{AFX_DATA_INIT(CToolFindDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CToolFindDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CToolFindDlg)
		DDX_Text(pDX, IDC_SEARCH_STRING, m_find_str);
	// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CToolFindDlg, CDialog)
	//{{AFX_MSG_MAP(CToolFindDlg)
	ON_BN_CLICKED(IDD_TOOL_FIND, OnToolFind)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CToolFindDlg message handlers

void CToolFindDlg::OnToolFind() 
{
	int stat, pos = ((CMainFrame *)m_parent)->get_current_toolpos();
	int savpos = pos = pos+1;
	GetDlgItem(IDC_SEARCH_STRING)->GetWindowText(m_find_str);
	int nc = m_find_str.GetLength();
	if (nc==0)
		return;
	((CMainFrame *)m_parent)->SaveFindStr(m_find_str);
	char cmsg[82], find_str[80];
	int i, enc, err = 0;
	for (i=0; i<nc; i++)
		find_str[i] =m_find_str[i];
	find_str[i] = '\0';
	stat = tool_findtool(find_str, &pos);
	if ((stat==0)&&(pos!=0)&&(pos!=savpos)&&(pos!=-1))
	{
		((CMainFrame *)m_parent)->SetFindPos(pos);
	}
}

void CToolFindDlg::OnCancel() 
{	
	CDialog::OnCancel();
}
