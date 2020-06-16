/************************************************************************
**
**   FILE NAME: NcqOptDlg.cpp
**
**	 CONTAINS: 
**		NcqOptDlg::NcqOptDlg()
**		NcqOptDlg::OnInitDialog()
**		NcqOptDlg::OnOk() 
**
**     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
**          All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncqoptdlg.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:03
**
***********************************************************************
*/
// NcqOptDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ncq.h"
#include "NcqOptDlg.h"
#include "ncqcom.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int getoption(char *optstr, int *ipglen, char* lpri);
extern "C" int setoption(char *optstr, int *ipglen, char* lpri);
extern "C" int ncq_saveopt_ini (char *linbuf);
extern "C" int NCQ_monitor;
extern "C" int NCQ_runver;
/////////////////////////////////////////////////////////////////////////////
// NcqOptDlg dialog


NcqOptDlg::NcqOptDlg(CWnd* pParent /*=NULL*/)
	: CDialog(NcqOptDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(NcqOptDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void NcqOptDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(NcqOptDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(NcqOptDlg, CDialog)
	//{{AFX_MSG_MAP(NcqOptDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// NcqOptDlg message handlers

/***********************************************************************
c
c   SUBROUTINE:  OnOK
c
c   FUNCTION:  This function called when pushed 'Ok' button 
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void NcqOptDlg::OnOK() 
{	
	int tmp;

	tmp = ((CButton*)GetDlgItem(ID_CLF))->GetCheck();
	if (tmp)
		ncq_linbuf[0] = '1';
	else
		ncq_linbuf[0] = '0';

	tmp = ((CButton*)GetDlgItem(ID_ASF))->GetCheck();
	if (tmp)
		ncq_linbuf[1] = '1';
	else
		ncq_linbuf[1] = '0';

	tmp = ((CButton*)GetDlgItem(ID_PRF))->GetCheck();
	if (tmp)
		ncq_linbuf[4] = '1';
	else
		ncq_linbuf[4] = '0';

	tmp = ((CButton*)GetDlgItem(ID_PP))->GetCheck();
	if (tmp)
		ncq_linbuf[5] = '1';
	else
		ncq_linbuf[5] = '0';

	tmp = ((CButton*)GetDlgItem(ID_LSF))->GetCheck();
	if (tmp)
	{
		ncq_linbuf[4] = '1';
		ncq_linbuf[2] = '0';
	}
	else
		ncq_linbuf[2] = '1';

	tmp = ((CButton*)GetDlgItem(ID_UPD))->GetCheck();
	if (tmp)
		ncq_linbuf[3] = '1';
	else
		ncq_linbuf[3] = '0';

	NCQ_monitor = ((CButton*)GetDlgItem(ID_OPT_MONITOR))->GetCheck();

	char buffer[40];
	ncq_ipglen = GetDlgItemInt(ID_PGLN);
	_itoa(ncq_ipglen, buffer, 10 );
	int nc = strlen(buffer);
	ncq_linbuf[9] = buffer[nc-1];
	if (nc-2>=0) 
	{
		ncq_linbuf[8] = buffer[nc-2];
	}
	else
	{
		ncq_linbuf[8] = '0';
	}
	if (nc-3>=0) 
	{
		ncq_linbuf[7] = buffer[nc-3];
	}
	else
	{
		ncq_linbuf[7] = '0';
	}
	CString tstr;
	GetDlgItemText(ID_PRI, tstr);
	ncq_lpri = tstr[0];
	ncq_lpri = toupper(ncq_lpri);
	ncq_linbuf[6] = ncq_lpri;
	ncq_saveopt_ini (ncq_linbuf);

	GetDlgItemText(IDC_VERSION, tstr);
	NCQ_runver = atoi(tstr);
	CDialog::OnOK();
}

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize 
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL NcqOptDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
/*
.......init option data
*/
	if (ncq_linbuf[0] == '1')
		((CButton*)GetDlgItem(ID_CLF))->SetCheck(1);
	else
		((CButton*)GetDlgItem(ID_CLF))->SetCheck(0);

	if (ncq_linbuf[1] == '1')
		((CButton*)GetDlgItem(ID_ASF))->SetCheck(1);
	else
		((CButton*)GetDlgItem(ID_ASF))->SetCheck(0);

	if (ncq_linbuf[4] == '1')
		((CButton*)GetDlgItem(ID_PRF))->SetCheck(1);
	else
		((CButton*)GetDlgItem(ID_PRF))->SetCheck(0);
		
	if (ncq_linbuf[5] == '1')
		((CButton*)GetDlgItem(ID_PP))->SetCheck(1);
	else
		((CButton*)GetDlgItem(ID_PP))->SetCheck(0);
  
	if (ncq_linbuf[2] == '1')
		((CButton*)GetDlgItem(ID_LSF))->SetCheck(0);
	else
		((CButton*)GetDlgItem(ID_LSF))->SetCheck(1);

	if (ncq_linbuf[3] == '1')
		((CButton*)GetDlgItem(ID_UPD))->SetCheck(1);
	else
		((CButton*)GetDlgItem(ID_UPD))->SetCheck(0);

	if (NCQ_monitor)
		((CButton*)GetDlgItem(ID_OPT_MONITOR))->SetCheck(1);
	else
		((CButton*)GetDlgItem(ID_OPT_MONITOR))->SetCheck(0);

	SetDlgItemInt(ID_PGLN, ncq_ipglen);
	char tmp[2];
	tmp[0] = ncq_lpri;
	tmp[1] = '\0';
	SetDlgItemText(ID_PRI,tmp);

	char verstr[40];
	sprintf (verstr, "%d", NCQ_runver);
	SetDlgItemText(IDC_VERSION, verstr);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
