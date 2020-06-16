/************************************************************************
c
c   FILE NAME: OperateDlg.cpp
c
c	 CONTAINS: 
c		Functions for COperateDlg class 
c		which display a Operator dialog and
c		define a operator command record
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       OperateDlg.cpp , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/12/15 , 17:24:28
c
c**********************************************************************
*/
// OperateDlg.cpp : implementation file
//

#include "toolibstdafx.h"
#include "toolib.h"
#include "OperateDlg.h"
#include "toolibcc.h"
#include "toolibdata.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" struct TL_toolhead_rec Tool_head;
extern "C" struct TL_tooldata_rec Tool_current_data;
extern "C" char * uu_malloc(int);
extern "C" void uu_free( char* );

/////////////////////////////////////////////////////////////////////////////
// COperateDlg dialog

/***********************************************************************
c
c   FUNCTION: COperateDlg(CWnd* pParent)
c
c              Constructor of class COperateDlg
c
c   INPUT:  pParent: parent window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
COperateDlg::COperateDlg(CWnd* pParent /*=NULL*/)
	: CDialog(COperateDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(COperateDlg)
		// NOTE: the ClassWizard will add member initialization here
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

void COperateDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(COperateDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(COperateDlg, CDialog)
	//{{AFX_MSG_MAP(COperateDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COperateDlg message handlers

/***********************************************************************
c
c   FUNCTION: COperateDlg::OnOK()
c
c         This is callback function for OK button.
c			This function will add a Operator command record
c			according user input
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/
void COperateDlg::OnOK() 
{
	char cstr[512], cstr2[80];
	char comstrs[50000];
	int i, tlen, len, cont, kadd, kerr, rtype, clen,start, finit;
	CString tempstr;	
	GetDlgItem(IDC_EDIT1)->GetWindowText(tempstr);
//	GetDlgItem(IDC_EDIT1)->GetWindowText(comstrs, 50000);
//	Tool_current_data.no_command = strlen (comstrs);
	Tool_current_data.no_command = tempstr.GetLength();
	if (Tool_current_data.command!=NULL)
		uu_free (Tool_current_data.command);
	Tool_current_data.command = (char *)uu_malloc 
							( (Tool_current_data.no_command+1)*sizeof(char) );
#ifdef _UNICODE	
	if (Tool_current_data.no_command>0)
	{
		WCHAR *wcomstrs = tempstr.GetBuffer(Tool_current_data.no_command);
		wcstombs(Tool_current_data.command, wcomstrs, Tool_current_data.no_command);
		Tool_current_data.command[Tool_current_data.no_command] = '\0';
	}
	else
		Tool_current_data.command[0] = '\0';
#else	
	strcpy(Tool_current_data.command, comstrs);
#endif
done:;
	CDialog::OnOK();
}
/***********************************************************************
c
c   FUNCTION: OnInitDialog() 
c
c		This function get the initialized Operator command
c		and fill the text field
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
BOOL COperateDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	((CEdit*)GetDlgItem(IDC_EDIT1))->LimitText(50000);

	char comstrs[50000], Pcomstrs[50000];
	char cstr[512];
	int i, j, tlen, len, cont, kadd, kerr, iniflg, rtype;

	i = j = 0;
	if ((Tool_current_data.no_command!=0) && 
		(strlen (Tool_current_data.command)!=0))
	{
		strcpy (comstrs, Tool_current_data.command);
/*
.....add '\r' if there are '\n'
*/
		for (i=0,j=0; i<Tool_current_data.no_command;i++)
		{
			if (comstrs[i]!='\n')
				Pcomstrs[j++] = comstrs[i];
			else
			{
				Pcomstrs[j++] = '\r';
				Pcomstrs[j++] = comstrs[i];
			}
		}
	}
	Pcomstrs[j] = '\0';
	CString tempstr;
#ifdef _UNICODE	
	tempstr = Pcomstrs;
	GetDlgItem(IDC_EDIT1)->SetWindowText(tempstr);
#else
	GetDlgItem(IDC_EDIT1)->SetWindowText(Pcomstrs);
#endif
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
