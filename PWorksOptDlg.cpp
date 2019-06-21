/************************************************************************
c
c   FILE NAME: PworksOptDlg.cpp
c
c	 CONTAINS: 
c		PWorksOptDlg::PWorksOptDlg()
c		PWorksOptDlg::OnOptionBrowse1()
c		CPworksDlg::OnInitDialog()
c		CPworksDlg::OnCancel()
c		CPworksDlg::OnOk() 
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PWorksOptDlg.cpp , 26.2
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/18 , 13:52:43
c
c**********************************************************************
*/

#include "pwstdafx.h"
#include "Pworks.h"
#include "PWorksOptDlg.h"
#include "pwfunc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

int Save_optint = 0, Save_usrint = 0;
char Save_option[256], Save_uservar[1000];
static void Sget_optint_str(char *opt_str);
static void Sget_usropt_str(char *opt_str);
extern "C" int Pw_dispmsg(char *msg, int flag);
extern "C" int geti4nc (char cbuf[256], int *knc, int knum[20], int *knwds, int *kerr);
extern "C" int getr8nc (char lbuff[1000], int *len, double rary[100], int *knwds, int *kerr);
/////////////////////////////////////////////////////////////////////////////
// PWorksOptDlg dialog


/***********************************************************************
c
c   SUBROUTINE: PWorksOptDlg(CWnd* pParent)	
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
PWorksOptDlg::PWorksOptDlg(CWnd* pParent /*=NULL*/)
	: CDialog(PWorksOptDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(PWorksOptDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void PWorksOptDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PWorksOptDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(PWorksOptDlg, CDialog)
	//{{AFX_MSG_MAP(PWorksOptDlg)
	ON_BN_CLICKED(ID_OPTION_BROWSE1, OnOptionBrowse1)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// PWorksOptDlg message handlers

static void Sget_optint_str(char *opt_str)
{
	int i, indx, iopt[48];
	double rusr[50];
	char copt[21][256], tmpstr[40];

	pw_getoptions(iopt, rusr, copt);
	opt_str[0] = '\0';
	for (i=20;i<40;i++)
	{
		if (iopt[i]==-987654321)
		{
			i++;
			continue;
		}
		indx = i - 19;
		sprintf(tmpstr, "%d,%d", indx, iopt[i]);
		if (opt_str[0]!='\0')
			strcat (opt_str, ",");
		strcat(opt_str, tmpstr);
	}
}
static void Sget_usropt_str(char *opt_str)
{
	int i, indx, iopt[48];
	double rusr[50];
	char copt[21][256], tmpstr[40];

	pw_getoptions(iopt, rusr, copt);
	opt_str[0] = '\0';
	for (i=0;i<50;i++)
	{
		if ((rusr[i]==-987654321)||(rusr[i]==0))
		{
			i++;
			continue;
		}
		indx = i+1;
		sprintf(tmpstr, "%d,%f", indx, rusr[i]);
		if (opt_str[0]!='\0')
			strcat (opt_str, ",");
		strcat(opt_str, tmpstr);
	}
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
BOOL PWorksOptDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
/*
.....get options and initialize
*/
	int iopt[48];
	double rusr[50];
	char copt[21][256];
	char opt_str[1000];

	pw_getoptions(iopt, rusr, copt);
/*
.....adjust
*/
	if (iopt[8]==1)
		SetDlgItemText(ID_OPTION_EDIT1, copt[8]);
/*
.....Warning
*/
	if(iopt[43]>=1) 
	{
		CheckDlgButton(ID_WARN,1);
		SetDlgItemInt(ID_OPTION_EDIT3, iopt[43]);
	}
/*
......ERROR
*/
	if (iopt[44]>=1)
	{
		CheckDlgButton(ID_ERROR,1 );
		SetDlgItemInt(ID_OPTION_EDIT4, iopt[44]);
	}
/*
......FATAL
*/
	if (iopt[45]>=1)
	{
		CheckDlgButton(ID_FINAL,1);
		SetDlgItemInt(ID_OPTION_EDIT5, iopt[45]);
	}
/*
......APT Source errors
*/
/*
......Modify iopt[10] value from 1 to 2 so not to display APT errors
      to the PWorks status window and only output to the pr1 file.
	  Previously a value of "1" also displays the APT errors to the
	  PWorks status window.
*/
	if (iopt[10] == 2)
		CheckDlgButton(ID_APTERR, 1);
/*
......PRINT
*/
	if (iopt[3]>=1)
	{
		CheckDlgButton(ID_PRINT,1);
		SetDlgItemText(ID_OPTION_EDIT10,copt[3]);
	}
/*
......PUNCH
*/
	if (iopt[6]==1)
	{
		CheckDlgButton(ID_PUNCH, 1);
		SetDlgItemText(ID_OPTION_EDIT11,copt[6]);
	}
/*
......SIMULATE
*/
	if (iopt[9] != 0)
	{
		CheckDlgButton(ID_SIMUL, 1);
	}
	if (strlen(copt[9])==0)
		SetDlgItemText(ID_OPTION_EDIT12, ".sim");
	else
		SetDlgItemText(ID_OPTION_EDIT12,copt[9]);
/*
......option pulldown list
......CLFILE
*/
	CComboBox* cmbbx = (CComboBox*)GetDlgItem(ID_CLFILE);
	cmbbx->AddString("Automatic");
	cmbbx->AddString("NCL Binary Clfile");
	cmbbx->AddString("APT Source Textual File");
	cmbbx->AddString("Catia V4 Binary Clfile");
	cmbbx->AddString("APT Source Textual, UG circles");
	cmbbx->AddString("Catia V5 Binary Clfile");
	cmbbx->AddString("MasterCam NCI File");
	cmbbx->SetCurSel(iopt[41]);
	SetDlgItemInt(ID_OPTION_EDIT2, iopt[42]);
/*
......PAGE_LEN
*/
	SetDlgItemInt(ID_OPTION_EDIT9,iopt[2]);
/*
......IDENT
*/
	if (iopt[5] == 1)
		CheckDlgButton(ID_IDENT, 1);
/*
......LISTING
*/
	if (iopt[1]==1)
	{
		CheckDlgButton(ID_LIST, 1);
	}
	if (strlen(copt[1])==0)
		SetDlgItemText(ID_OPTION_EDIT6, ".lis");
	else
		SetDlgItemText(ID_OPTION_EDIT6,copt[1]);
/*
......MACHINE
*/
	if (iopt[4]==1)
		SetDlgItemText(ID_OPTION_EDIT7,copt[20]);		

/*
......Option
*/
	if (Save_optint==1)
		SetDlgItemText(ID_OPTION_EDIT8, Save_option);		
	else
	{
		Sget_optint_str(opt_str);
		SetDlgItemText(ID_OPTION_EDIT8, opt_str);
	}
/*
......FILL
*/
	if (iopt[11]==1)
	{
		CheckDlgButton(ID_FILL, 1);
		SetDlgItemText(ID_OPTION_EDIT13,copt[11]);
		
	}
/*
......USERVAR
*/
	if (Save_usrint==1)
		SetDlgItemText(ID_OPTION_EDIT14, Save_uservar);
	else
	{
		Sget_usropt_str(opt_str);
		SetDlgItemText(ID_OPTION_EDIT14, opt_str);
	}
	return TRUE;
}

/***********************************************************************
c
c   SUBROUTINE:  OnOptionBrowse1
c
c   FUNCTION:  This function called when pushed 'BROWSE' button 
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void PWorksOptDlg::OnOptionBrowse1() 
{
	LPCTSTR filter = "Adjust Files(*.maf)|*.maf|All Files (*.*)|*.*||";		
	DWORD dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;

	CFileDialog *filedlg = new CFileDialog(TRUE, "Pworks", NULL, dwFlags,
			filter, this);
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	GetDlgItem(ID_OPTION_EDIT1)->SetWindowText(FileName);
	delete filedlg;		
}

/***********************************************************************
c
c   SUBROUTINE:  OnCancel
c
c   FUNCTION:  This function called when pushed 'Cancel' button 
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void PWorksOptDlg::OnCancel() 
{
	CDialog::OnCancel();
}

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
void PWorksOptDlg::OnOK() 
{	
	int number, len, opmn[8], opmx[8];
	BOOL trans;
	int i;
/*
.....get options and save
*/
	int iopt[48],iary[20],nwds,ierr;
	double rusr[50], rary[100];
	char copt[21][256], buff[256], lbuff[1000];

	for (i=0; i<256;i++)
		buff[i] = ' ';

	for (i=0; i<1000;i++)
		lbuff[i] = ' ';

	opmn[0] = 1;
	opmn[1] = 1;
	opmn[2] = 0;
	opmn[3] = -999999999;
	opmn[4] = 10;
	opmn[5] = 1;
	opmn[6] = 0;
	opmn[7] = 0;
	opmx[0] = 2;
	opmx[1] = 2;
	opmx[2] = 999999999;
	opmx[3] = 999999999;
	opmx[4] = 512;
	opmx[5] = 3;
	opmx[6] = 3;
	opmx[7] = 1;
/*
.....default to value now
*/
	pw_getoptions(iopt, rusr, copt);
/*
.....adjust
*/
	len = GetDlgItemText(ID_OPTION_EDIT1, copt[8], 256);
	if (len>0)
		iopt[8] = 1;
	else
		iopt[8] = 0;
/*
.....Apt source line length
*/
	iopt[42] = GetDlgItemInt(ID_OPTION_EDIT2, &trans);
/*
.....Warning
*/
	if (((CButton*)GetDlgItem(ID_WARN))->GetCheck())
	{
		iopt[43] = GetDlgItemInt(ID_OPTION_EDIT3, &trans);
		if (trans==0)
			iopt[43] = 9999;
		if (iopt[33] <= 0)
			iopt[43] = 9999;
	}
	else
		iopt[43] = -1;
/*
......ERROR
*/
	if (((CButton*)GetDlgItem(ID_ERROR))->GetCheck())
	{
		iopt[44] = GetDlgItemInt(ID_OPTION_EDIT4, &trans);
		if (trans==0)
			iopt[44] = 9999;
		if (iopt[34] <= 0)
			iopt[44] = 9999;
	}
	else
		iopt[44] = -1;

/*
......FATAL
*/
	if (((CButton*)GetDlgItem(ID_FINAL))->GetCheck())
	{
		iopt[45] = GetDlgItemInt(ID_OPTION_EDIT5, &trans);
		if (trans==0)
			iopt[45] = 1;
		if (iopt[45] <= 0)
			iopt[45] = 1;
	}
	else
		iopt[34] = -1;
/*
......APT Source errors
*/
	if (((CButton*)GetDlgItem(ID_APTERR))->GetCheck())
/*
......Previious value = 1, modify so PWorks interface window not
      display APT error message.
	  */
		iopt[10] = 2;
	else
		iopt[10] = 0;
/*
......PRINT
*/
	if (((CButton*)GetDlgItem(ID_PRINT))->GetCheck())
	{
		GetDlgItemText(ID_OPTION_EDIT10,copt[3],80);
		iopt[3] = 1;
	}
	else
		iopt[3] = 0;
/*
......PUNCH
*/
	if (((CButton*)GetDlgItem(ID_PUNCH))->GetCheck())
	{
		iopt[6] = 1;
		GetDlgItemText(ID_OPTION_EDIT11,copt[6],80);
	}
	else
		iopt[6] = 0;
/*
......SIMULATE
*/
	if (((CButton*)GetDlgItem(ID_SIMUL))->GetCheck())
	{
		iopt[9] = 2;
		GetDlgItemText(ID_OPTION_EDIT12,copt[9],80);
	}
	else
		iopt[9] = 0;
/*
......option pulldown list
......CLFILE
*/
	iopt[41] = ((CComboBox*)GetDlgItem(ID_CLFILE))->GetCurSel();
/*
......PAGE_LEN
*/
	number = GetDlgItemInt(ID_OPTION_EDIT9, &trans);
	if (trans)
	{
		if (number>0)
			iopt[2] = number;
	}
/*
......IDENT
*/
	if (((CButton*)GetDlgItem(ID_IDENT))->GetCheck())
		iopt[5] = 1;
	else
		iopt[5] = 0;
/*
......LISTING
*/
	if (((CButton*)GetDlgItem(ID_LIST))->GetCheck())
	{
		iopt[1] = 1;
		GetDlgItemText(ID_OPTION_EDIT6,copt[1],80);
	}
	else
		iopt[1] = 0;
/*
......MACHINE
*/
	len = GetDlgItemText(ID_OPTION_EDIT7,copt[20],40);
	if (len == 0)
	{
		iopt[4] = 0;
		iopt[47] = 0;
	}
	else
	{
		iopt[4] = 1;
		iopt[47] = 1;
	}
/*
......FILL
*/
	if (((CButton*)GetDlgItem(ID_FILL))->GetCheck())
	{
		GetDlgItemText(ID_OPTION_EDIT13,copt[11],80);
		iopt[11] = 1;
	}
	else
		iopt[11] = 0;
/*
......Option
*/
	len = GetDlgItemText(ID_OPTION_EDIT8, Save_option, 255);
	if (len>0)
	{
		for (i=20;i<40;i++)
			iopt[i] = -987654321;
		Save_optint = 1;
		strncpy(buff, Save_option,len);
		geti4nc (buff, &len, iary, &nwds, &ierr);
		if ((ierr != 0) || (nwds == 0) || (nwds/2*2 != nwds))
		{
			Pw_dispmsg("Option is not valid in <OPTION> text field",0);
			return;
		}
		for (i=0; i<nwds; i=i+2)
		{
			if ((iary[i] <= 0) || (iary[i] > 6)) 
			{
				Pw_dispmsg("Option is not valid in <OPTION> text field",0);
				return;
			}
			if ((iary[i+1] < opmn[iary[i]-1]) || 
					(iary[i+1] > opmx[iary[i]-1]))
			{
				Pw_dispmsg("Option is not valid in <OPTION> text field",0);
				return;
			}
/*
iopt 20 - 40 is for options 
*/
            iopt[19+iary[i]] = iary[i+1];
		}
	}
	else
	{
		Save_optint = 0;
/*
.....set everything to default value
*/
		for (i=20;i<40;i++)
			iopt[i] = -987654321;
	}
/*
......USERVAR
*/
	len = GetDlgItemText(ID_OPTION_EDIT14, Save_uservar, 1000);
	int indx = 0;
	if (len>0)
	{
		for (i=0;i<50;i++)
			rusr[i] = -987654321;
		Save_usrint = 1;
		strncpy(lbuff, Save_uservar,len);
		getr8nc (lbuff, &len, rary, &nwds, &ierr);
		if ((ierr != 0) || (nwds == 0) || (nwds/2*2 != nwds))
		{
			Pw_dispmsg("Option is not valid in <USERVAR> text field",0);
			return;
		}
		for (i=0; i<nwds; i=i+2)
		{
			indx =  rary[i];
			if ((indx <= 0) || (indx > 50)) 
			{
				Pw_dispmsg("Option is not valid in <USERVAR> text field",0);
				return;
			}
			rusr[indx-1] = rary[i+1];
		}
	}
	else
	{
		Save_usrint = 0;
/*
.....set everything to default value
*/
		for (i=0;i<50;i++)
			rusr[i] = -987654321;
	}
done:;
	pw_saveoptions(iopt, rusr, copt);
	CDialog::OnOK();
}
