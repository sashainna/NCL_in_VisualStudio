/************************************************************************
c
c   FILE NAME: ParmsDlg.cpp
c
c	 CONTAINS: 
c		Functions for CParmsDlg class 
c		which display a Parameter dialog and
c		define a parameter record
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       ParmsDlg.cpp , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/12/15 , 17:24:54
c
c**********************************************************************
*/
// ParmsDlg.cpp : implementation file
//

#include "toolibstdafx.h"
#include "toolib.h"
#include "ParmsDlg.h"
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
// CParmsDlg dialog

/***********************************************************************
c
c   FUNCTION: CParmsDlg(CWnd* pParent)
c
c              Constructor of class CParmsDlg
c
c   INPUT:  pParent: parent window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

CParmsDlg::CParmsDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CParmsDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CParmsDlg)
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

void CParmsDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CParmsDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CParmsDlg, CDialog)
	//{{AFX_MSG_MAP(CParmsDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CParmsDlg message handlers
/***********************************************************************
c
c   FUNCTION: CParmsDlg::OnOK()
c
c         This is callback function for OK button.
c			This function will add a parameter record
c			according user input
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/

void CParmsDlg::OnOK() 
{	
	CString parm[20], plabel;
	int i, tlen, len, cont, kadd, kerr, rtype, finit;
		
	rtype = 1;
	GetDlgItem(IDC_EDIT1)->GetWindowText(parm[0]);
	GetDlgItem(IDC_EDIT2)->GetWindowText(parm[1]);
	GetDlgItem(IDC_EDIT3)->GetWindowText(parm[2]);
	GetDlgItem(IDC_EDIT4)->GetWindowText(parm[3]);
	GetDlgItem(IDC_EDIT5)->GetWindowText(parm[4]);
	GetDlgItem(IDC_EDIT6)->GetWindowText(parm[5]);
	GetDlgItem(IDC_EDIT7)->GetWindowText(parm[6]);
	GetDlgItem(IDC_EDIT8)->GetWindowText(parm[7]);
	GetDlgItem(IDC_EDIT9)->GetWindowText(parm[8]);
	GetDlgItem(IDC_EDIT10)->GetWindowText(parm[9]);
	GetDlgItem(IDC_EDIT11)->GetWindowText(parm[10]);
	GetDlgItem(IDC_EDIT12)->GetWindowText(parm[11]);
	GetDlgItem(IDC_EDIT13)->GetWindowText(parm[12]);
	GetDlgItem(IDC_EDIT14)->GetWindowText(parm[13]);
	GetDlgItem(IDC_EDIT15)->GetWindowText(parm[14]);
	GetDlgItem(IDC_EDIT16)->GetWindowText(parm[15]);
	GetDlgItem(IDC_EDIT17)->GetWindowText(parm[16]);
	GetDlgItem(IDC_EDIT18)->GetWindowText(parm[17]);
	GetDlgItem(IDC_EDIT19)->GetWindowText(parm[18]);
	GetDlgItem(IDC_EDIT20)->GetWindowText(parm[19]);
	tlen = 0;
	finit = 1;
	plabel = "";
	for (i=0; i<20; i++)
	{
		plabel =  plabel +  parm[i];
		plabel =  plabel +  _T("\n");
	}
	tlen = plabel.GetLength();
	Tool_current_data.no_plabel = tlen;
	if (Tool_current_data.plabel!=NULL)
		uu_free (Tool_current_data.plabel);
	if (tlen>0)
	{
		Tool_current_data.plabel = (char *)uu_malloc ( (tlen+1)*sizeof(char) );
#ifdef _UNICODE	
		WCHAR *wlabel = plabel.GetBuffer(tlen);
		wcstombs(Tool_current_data.plabel, wlabel, tlen);
		Tool_current_data.plabel[tlen] = '\0';
#else
		char *parmsstr = plabel.GetBuffer(tlen);
		strcpy(Tool_current_data.plabel, parmsstr);
#endif
	}
	else
		Tool_current_data.plabel[0] = '\0';
done:;
	CDialog::OnOK();
}

/***********************************************************************
c
c   FUNCTION: OnInitDialog() 
c
c		This function get the initialized parameter command
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

BOOL CParmsDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	((CEdit*)GetDlgItem(IDC_EDIT1))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT2))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT3))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT4))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT5))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT6))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT7))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT8))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT9))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT10))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT11))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT12))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT13))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT14))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT15))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT16))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT17))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT18))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT19))->LimitText(80);
	((CEdit*)GetDlgItem(IDC_EDIT20))->LimitText(80);

	char *tok;
	char parm[20][80], parmsstr[1800];
	int i, j, k, tlen, len, cont, kadd, kerr, iniflg, rtype;
	for (i=0; i<20; i++)
		parm[i][0] = '\0';
	i = 0;
	j = 0;
	k = 0;
	if ((Tool_current_data.no_plabel!=0) && 
		(strlen (Tool_current_data.plabel)!=0))
	{
		strcpy (parmsstr, Tool_current_data.plabel);
		tlen = strlen (parmsstr);
		while(j<tlen)
		{
			if (parmsstr[j]=='\0')
				break;
			if (parmsstr[j]!='\n')
			{
				if (parmsstr[j]!='\r')
					parm[i][k++] = parmsstr[j++];
				else
				{
					j++;
					continue;
				}
			}
			else 
			{
				parm[i][k++] = '\0';
				j++;
				i++;
				k = 0;
				continue;
			}
		}
	}
	parm[i][k] = '\0'; 
#ifdef _UNICODE	
	WCHAR wtmpstr[80];
	int wlen;
	len = strlen (parm[0]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[0], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT1)->SetWindowText(wtmpstr);

	len = strlen (parm[1]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[1], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT2)->SetWindowText(wtmpstr);

	len = strlen (parm[2]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[2], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT3)->SetWindowText(wtmpstr);

	len = strlen (parm[3]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[3], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT4)->SetWindowText(wtmpstr);

	len = strlen (parm[4]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[4], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT5)->SetWindowText(wtmpstr);

	len = strlen (parm[5]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[5], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT6)->SetWindowText(wtmpstr);

	len = strlen (parm[6]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[6], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT7)->SetWindowText(wtmpstr);

	len = strlen (parm[7]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[7], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT8)->SetWindowText(wtmpstr);

	len = strlen (parm[8]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[8], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT9)->SetWindowText(wtmpstr);

	len = strlen (parm[9]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[9], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT10)->SetWindowText(wtmpstr);

	len = strlen (parm[10]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[10], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT11)->SetWindowText(wtmpstr);

	len = strlen (parm[11]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[11], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT12)->SetWindowText(wtmpstr);

	len = strlen (parm[12]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[12], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT13)->SetWindowText(wtmpstr);

	len = strlen (parm[13]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[13], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT14)->SetWindowText(wtmpstr);

	len = strlen (parm[14]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[14], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT15)->SetWindowText(wtmpstr);

	len = strlen (parm[15]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[15], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT16)->SetWindowText(wtmpstr);

	len = strlen (parm[16]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[16], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT17)->SetWindowText(wtmpstr);
	
	len = strlen (parm[17]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[17], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT18)->SetWindowText(wtmpstr);

	len = strlen (parm[18]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[18], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT19)->SetWindowText(wtmpstr);
		
	len = strlen (parm[19]) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, parm[19], -1, 
							wtmpstr, len);
	GetDlgItem(IDC_EDIT20)->SetWindowText(wtmpstr);
#else
	GetDlgItem(IDC_EDIT1)->SetWindowText(parm[0]);
	GetDlgItem(IDC_EDIT2)->SetWindowText(parm[1]);
	GetDlgItem(IDC_EDIT3)->SetWindowText(parm[2]);
	GetDlgItem(IDC_EDIT4)->SetWindowText(parm[3]);
	GetDlgItem(IDC_EDIT5)->SetWindowText(parm[4]);
	GetDlgItem(IDC_EDIT6)->SetWindowText(parm[5]);
	GetDlgItem(IDC_EDIT7)->SetWindowText(parm[6]);
	GetDlgItem(IDC_EDIT8)->SetWindowText(parm[7]);
	GetDlgItem(IDC_EDIT9)->SetWindowText(parm[8]);
	GetDlgItem(IDC_EDIT10)->SetWindowText(parm[9]);
	GetDlgItem(IDC_EDIT11)->SetWindowText(parm[10]);
	GetDlgItem(IDC_EDIT12)->SetWindowText(parm[11]);
	GetDlgItem(IDC_EDIT13)->SetWindowText(parm[12]);
	GetDlgItem(IDC_EDIT14)->SetWindowText(parm[13]);
	GetDlgItem(IDC_EDIT15)->SetWindowText(parm[14]);
	GetDlgItem(IDC_EDIT16)->SetWindowText(parm[15]);
	GetDlgItem(IDC_EDIT17)->SetWindowText(parm[16]);
	GetDlgItem(IDC_EDIT18)->SetWindowText(parm[17]);
	GetDlgItem(IDC_EDIT19)->SetWindowText(parm[18]);
	GetDlgItem(IDC_EDIT20)->SetWindowText(parm[19]);
#endif	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
