/*********************************************************************
**  NAME:  SwUniOpt.cpp
**
**       Implementation of NCL/Solid form functions.
**
** CONTAINS: CUniOpt class functions
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwUniOpt.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 13:00:38
*********************************************************************/
// UniOpt.h : Declaration of the CUniOpt

#ifndef __UNIOPT_H_
#define __UNIOPT_H_

#include "Swresource.h"       // main symbols
#include <io.h>
#include <atlhost.h>
#include "nclxunib.h"
#include "SwUniAttr.h"
#include "SwUniSketch.h"

extern "C" char *uu_lsnext(char *);
extern "C" void  uu_lsdel (char *);
extern "C" int ncl_check_label_prefix(char *,int,int);
extern "C" int UIG_unmatch_sec;
extern "C" int UIG_start_unmatch;
extern "C" int UIG_regressive;
extern "C" NCLX_sw_options gl_sw_opt;
/////////////////////////////////////////////////////////////////////////////
// CUniOpt
class CUniOpt : 
	public CAxDialogImpl<CUniOpt>
{
private:
	NCLX_sw_options m_sw_opt;
	NCLX_sw_options *m_sw_opt_ptr;
	double m_version;
	char c1str[3][30];
	char c2str[2][20];
	char c3str[5][20];
	char c6str[3][20];
	char c7str[2][20];
	char wtitle[40],rtitle[40];
	char *mxptr;

	void GetFileName(char *,char *,int *,HWND,int);
	int FileExists(char *title, char *file);

public:
	CUniOpt(NCLX_sw_options *swopt, double ver, char *mxlist)
	{
		int i;
//
//...Store form structure
//
		m_sw_opt_ptr = swopt;
		m_sw_opt = *swopt;
		m_version = ver;
//
//initialize form form mod file
//

   m_sw_opt.label_opts = gl_sw_opt.label_opts;
   m_sw_opt.import_curves = gl_sw_opt.import_curves;
   m_sw_opt.shade_surfs = gl_sw_opt.shade_surfs;
   m_sw_opt.import_sketch = gl_sw_opt.import_sketch;
   m_sw_opt.sflab_type = gl_sw_opt.sflab_type;
   m_sw_opt.cvlab_type = gl_sw_opt.cvlab_type;
   m_sw_opt.ptlab_type = gl_sw_opt.ptlab_type;
   m_sw_opt.lnlab_type = gl_sw_opt.lnlab_type;
   m_sw_opt.cilab_type = gl_sw_opt.cilab_type;
   m_sw_opt.kvlab_type = gl_sw_opt.kvlab_type;
   strcpy(m_sw_opt.cvlab,gl_sw_opt.cvlab);
   strcpy(m_sw_opt.sflab,gl_sw_opt.sflab);
   strcpy(m_sw_opt.ptlab,gl_sw_opt.ptlab);
   strcpy(m_sw_opt.lnlab,gl_sw_opt.lnlab);
   strcpy(m_sw_opt.cilab,gl_sw_opt.cilab);
   strcpy(m_sw_opt.kvlab,gl_sw_opt.kvlab);
   m_sw_opt.exact_match = gl_sw_opt.exact_match;
   m_sw_opt.toler = gl_sw_opt.toler;
   for(i=0;i<6;i++)
      m_sw_opt.layer[i] = gl_sw_opt.layer[i];

   for(i=0;i<6;i++)
      m_sw_opt.color[i] = gl_sw_opt.color[i];
   m_sw_opt.cvopt = gl_sw_opt.cvopt;
//
//...Initialize variables
//
		strcpy(c1str[0],"Generate");
		strcpy(c1str[1],"Using Existing Unibase");

		strcpy(c2str[0],"Prefix");
		strcpy(c2str[1],"Subscript");

		strcpy(c3str[0],"Exact Match");
		strcpy(c3str[1],"Level 1");
		strcpy(c3str[2],"Level 2");
		strcpy(c3str[3],"Level 3");
		strcpy(c3str[4],"Level 4");

		strcpy(wtitle,"Save Unibase As");
		strcpy(rtitle,"Label Matching Unibase");
		mxptr = mxlist;

		strcpy(c6str[0],"Separate Curves");
		strcpy(c6str[1],"Composite Curves");
		strcpy(c6str[2],"No Duplicates");

		strcpy(c7str[0],"Next");
		strcpy(c7str[1],"Secondary");
	}

	~CUniOpt()
	{
	}

	enum { IDD = IDD_UNIOPT };

BEGIN_MSG_MAP(CUniOpt)
	MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
	COMMAND_ID_HANDLER(IDOK, OnOK)
	COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
	COMMAND_HANDLER(IDC_UNIOPT_CHECK1, BN_CLICKED, OnClickedUniopt)
	COMMAND_HANDLER(IDC_UNIOPT_CHECK2, BN_CLICKED, OnClickedUniopt)
	COMMAND_HANDLER(IDC_UNIOPT_CHECK3, BN_CLICKED, OnClickedUniopt)
	COMMAND_HANDLER(IDC_UNIOPT_CHECK4, BN_CLICKED, OnClickedUniopt)
	COMMAND_HANDLER(IDC_UNIOPT_COMBO1, CBN_SELCHANGE, OnSelchangeUniopt)
	COMMAND_HANDLER(IDC_UNIOPT_COMBO2, CBN_SELCHANGE, OnSelchangeUniopt)
	COMMAND_HANDLER(IDC_UNIOPT_COMBO3, CBN_SELCHANGE, OnSelchangeUniopt)
	COMMAND_HANDLER(IDC_UNIOPT_COMBO4, CBN_SELCHANGE, OnSelchangeUniopt)
	COMMAND_HANDLER(IDC_UNIOPT_COMBO5, CBN_SELCHANGE, OnSelchangeUniopt)
	COMMAND_HANDLER(IDC_UNIOPT_COMBO6, CBN_SELCHANGE, OnSelchangeUniopt)
	COMMAND_HANDLER(IDC_UNIOPT_COMBO7, CBN_SELCHANGE, OnSelchangeUniopt)
	COMMAND_HANDLER(IDC_UNIOPT_BUTTON1, BN_CLICKED, OnClickedbutton1)
	COMMAND_HANDLER(IDC_UNIOPT_BUTTON2, BN_CLICKED, OnClickedbutton2)
	COMMAND_HANDLER(IDC_UNIOPT_BUTTON3, BN_CLICKED, OnClickedbutton3)
	COMMAND_HANDLER(IDC_UNIOPT_BUTTON4, BN_CLICKED, OnClickedbutton4)
END_MSG_MAP()
// Handler prototypes:
//  LRESULT MessageHandler(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
//  LRESULT CommandHandler(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled);
//  LRESULT NotifyHandler(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);

private:
	LRESULT OnInitDialog(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		BOOL flag;
		char cstr[40];

//
//...Set the window title
//...to state the NCL version number
//
	sprintf(cstr,"Create NCL V%5.3f Unibase",m_version);
	SetWindowText((CComBSTR)cstr);
//
//...Initialize form
//......Unibase
//
		SetDlgItemText(IDC_UNIOPT_EDIT1,(CComBSTR)m_sw_opt.unibase);
//
//......Entity label options
//
		CWindow combo(GetDlgItem(IDC_UNIOPT_COMBO1));
		TCHAR bstr[1024];
		mbstowcs(bstr,c1str[0],strlen(c1str[0])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		mbstowcs(bstr,c1str[1],strlen(c1str[1])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		combo.SendMessage(CB_SETCURSEL,gl_sw_opt.label_opts,0);
		flag = FALSE;
		if (gl_sw_opt.label_opts == 1) flag = TRUE;
		::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT8),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT7),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_EDIT4),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_BUTTON2),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_BUTTON3),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_COMBO4),flag);
		::EnableWindow(GetDlgItem(IDC_CHK_UNMATCH_SEC),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_CHECK3),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT12),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_COMBO7),flag);
		if (gl_sw_opt.cvopt == 2) flag = TRUE;
		::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT9),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_EDIT5),flag);
//
//.......Export curves
//
		flag = gl_sw_opt.import_curves;
		CheckDlgButton(IDC_UNIOPT_CHECK1,flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT6),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_COMBO3),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_EDIT3),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT11),flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_COMBO6),flag);
//
//......Shade surfaces
//
		flag = gl_sw_opt.shade_surfs;
		CheckDlgButton(IDC_UNIOPT_CHECK2,flag);
//
//.......Export sketch geometry
//
		flag = gl_sw_opt.import_sketch;
		CheckDlgButton(IDC_UNIOPT_CHECK4,flag);
		::EnableWindow(GetDlgItem(IDC_UNIOPT_BUTTON4),flag);
//
//......Labels
//
		combo = GetDlgItem(IDC_UNIOPT_COMBO2);
		mbstowcs(bstr,c2str[0],strlen(c2str[0])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		mbstowcs(bstr,c2str[1],strlen(c2str[1])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		combo.SendMessage(CB_SETCURSEL,gl_sw_opt.sflab_type,0);
		SetDlgItemText(IDC_UNIOPT_EDIT2,(CComBSTR)gl_sw_opt.sflab);
		combo = GetDlgItem(IDC_UNIOPT_COMBO3);
		mbstowcs(bstr,c2str[0],strlen(c2str[0])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		mbstowcs(bstr,c2str[1],strlen(c2str[1])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		combo.SendMessage(CB_SETCURSEL,gl_sw_opt.cvlab_type,0);
		SetDlgItemText(IDC_UNIOPT_EDIT3,(CComBSTR)gl_sw_opt.cvlab);
		CheckDlgButton(IDC_UNIOPT_CHECK2,flag);
//
//......Creating labels fom unmatched entities in the secondary unibase.
//
		flag = UIG_unmatch_sec;
		CheckDlgButton(IDC_CHK_UNMATCH_SEC,flag);
//
//......Regressive matching
//
		flag = UIG_regressive;
		CheckDlgButton(IDC_UNIOPT_CHECK3,flag);
//
//......Start Unmatched entities form:
//
		combo = GetDlgItem(IDC_UNIOPT_COMBO7);
		mbstowcs(bstr,c7str[0],strlen(c7str[0])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		mbstowcs(bstr,c7str[1],strlen(c7str[1])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		combo.SendMessage(CB_SETCURSEL,UIG_start_unmatch,0);
//
//......Matching Unibase
//
		SetDlgItemText(IDC_UNIOPT_EDIT4,(CComBSTR)m_sw_opt.unimatch);
//
//......Matching level
//
		combo = GetDlgItem(IDC_UNIOPT_COMBO4);
		mbstowcs(bstr,c3str[0],strlen(c3str[0])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		mbstowcs(bstr,c3str[1],strlen(c3str[1])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		mbstowcs(bstr,c3str[2],strlen(c3str[2])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		mbstowcs(bstr,c3str[3],strlen(c3str[3])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		mbstowcs(bstr,c3str[4],strlen(c3str[4])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		combo.SendMessage(CB_SETCURSEL,gl_sw_opt.exact_match,0);
//
//......Curve Options
//
		combo = GetDlgItem(IDC_UNIOPT_COMBO6);
		mbstowcs(bstr,c6str[0],strlen(c6str[0])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		mbstowcs(bstr,c6str[1],strlen(c6str[1])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		mbstowcs(bstr,c6str[2],strlen(c6str[2])+1);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		combo.SendMessage(CB_SETCURSEL,gl_sw_opt.cvopt,0);
//
//......Tolerance
//
		sprintf(cstr,"%f",gl_sw_opt.toler);
		SetDlgItemText(IDC_UNIOPT_EDIT5,(CComBSTR)cstr);
//
//.....Coordinate systems
//
		combo = GetDlgItem(IDC_UNIOPT_COMBO5);
		mbstowcs(bstr,"-- default --",13);
		combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
		m_sw_opt.mxname[0] = '\0';
		if (mxptr)
		{
			char *p1 = uu_lsnext(mxptr);
			while (p1)
			{
				mbstowcs(bstr,p1,strlen(p1)+1);
				combo.SendMessage(CB_ADDSTRING,0,(LPARAM)bstr);
				p1 = uu_lsnext(p1);
			}
			uu_lsdel(mxptr);
		}
		combo.SendMessage(CB_SETCURSEL,0,0);

		return 1;  // Let the system set the focus
	}

	LRESULT OnOK(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		int i,nc,stat;
		UINT flag;
		char cstr[20],sbuf[1024],fname[1024],*p;
		TCHAR bstr[1024];

		GetDlgItemText(IDC_UNIOPT_EDIT1,bstr,1023);
		nc = wcslen(bstr);
		wcstombs(m_sw_opt.unibase,bstr,nc+1);
		GetDlgItemText(IDC_UNIOPT_EDIT2,bstr,63);
		nc = wcslen(bstr);
		wcstombs(m_sw_opt.sflab,bstr,nc+1);
		GetDlgItemText(IDC_UNIOPT_EDIT3,bstr,63);
		nc = wcslen(bstr);
		wcstombs(m_sw_opt.cvlab,bstr,nc+1);
		GetDlgItemText(IDC_UNIOPT_EDIT4,bstr,1023);
		nc = wcslen(bstr);
		wcstombs(m_sw_opt.unimatch,bstr,nc+1);
		GetDlgItemText(IDC_UNIOPT_EDIT5,bstr,20);
		nc = wcslen(bstr);
		wcstombs(cstr,bstr,nc+1);
		flag = IsDlgButtonChecked(IDC_CHK_UNMATCH_SEC) ;
		UIG_unmatch_sec = flag;
		flag = IsDlgButtonChecked(IDC_UNIOPT_CHECK3) ;
		UIG_regressive = flag;
//
//...Make sure labels are valid
//
		ncl_check_label_prefix(m_sw_opt.sflab,m_sw_opt.sflab_type,TRUE);
		ncl_check_label_prefix(m_sw_opt.cvlab,m_sw_opt.cvlab_type,TRUE);
		ncl_check_label_prefix(m_sw_opt.ptlab,m_sw_opt.ptlab_type,TRUE);
		ncl_check_label_prefix(m_sw_opt.lnlab,m_sw_opt.lnlab_type,TRUE);
		ncl_check_label_prefix(m_sw_opt.cilab,m_sw_opt.cilab_type,TRUE);
		ncl_check_label_prefix(m_sw_opt.kvlab,m_sw_opt.kvlab_type,TRUE);
//
//...Make sure tolerance is valid number
//
		sscanf(cstr,"%lf",&m_sw_opt.toler);
		if (m_sw_opt.toler <= 0. || m_sw_opt.toler > 1.0)
		{
			AfxMessageBox(_T("Tolerance value is out of range."));
			return 1;
		}
//
//...Check to see if file exists
//
		nc = strlen(m_sw_opt.unibase);
		if (nc == 0)
		{
			AfxMessageBox(_T("A file must be specified."));
			return 1;
		}
		else
		{
			if (_access(m_sw_opt.unibase,0) == 0)
			{
				if (FileExists(wtitle,m_sw_opt.unibase) == IDYES)
				{
					if (_access(m_sw_opt.unibase,2) == -1)
					{
						sprintf(sbuf,"%s\nDoes not have write access.",
							m_sw_opt.unibase);
						mbstowcs(bstr,sbuf,strlen(sbuf)+1);
						AfxMessageBox(bstr);
						return 1;
					}
				}
				else return 1;
			}
			else
			{
				strcpy(fname,m_sw_opt.unibase);
				p = strrchr(fname,'\\');
				if (p != NULL)
				{
					*p = '\0';
					if (_access(fname,2) == -1)
					{
						sprintf(sbuf,"%s\nDoes not have write access.",
							fname);
						mbstowcs(bstr,sbuf,strlen(sbuf)+1);
						AfxMessageBox(bstr);
						return 1;
					}
				}
			}
		}
//
//...Check to see if matching file exists
//
		if (m_sw_opt.label_opts == 1)
		{
			nc = strlen(m_sw_opt.unimatch);
			if (nc == 0)
			{
				AfxMessageBox(_T("A Matching Unibase must be specified."));
				return 1;
			}
			else
			{
				if (_access(m_sw_opt.unimatch,4) != 0)
				{
					sprintf(sbuf,"%s\nDoes not exist.",m_sw_opt.unimatch);
					mbstowcs(bstr,sbuf,strlen(sbuf)+1);
					AfxMessageBox(bstr);
					return 1;
				}
			}
		}
//
//...Return from form and save the form variables
//
		gl_sw_opt.label_opts = m_sw_opt.label_opts;
		gl_sw_opt.import_curves = m_sw_opt.import_curves;
		gl_sw_opt.shade_surfs = m_sw_opt.shade_surfs;
		gl_sw_opt.import_sketch = m_sw_opt.import_sketch;
		gl_sw_opt.sflab_type = m_sw_opt.sflab_type;
		gl_sw_opt.cvlab_type = m_sw_opt.cvlab_type;
		gl_sw_opt.ptlab_type = m_sw_opt.ptlab_type;
		gl_sw_opt.lnlab_type = m_sw_opt.lnlab_type;
		gl_sw_opt.cilab_type = m_sw_opt.cilab_type;
		gl_sw_opt.kvlab_type = m_sw_opt.kvlab_type;
		strcpy(gl_sw_opt.cvlab,m_sw_opt.cvlab);
		strcpy(gl_sw_opt.sflab,m_sw_opt.sflab);
		strcpy(gl_sw_opt.ptlab,m_sw_opt.ptlab);
		strcpy(gl_sw_opt.lnlab,m_sw_opt.lnlab);
		strcpy(gl_sw_opt.cilab,m_sw_opt.cilab);
		strcpy(gl_sw_opt.kvlab,m_sw_opt.kvlab);
		gl_sw_opt.exact_match = m_sw_opt.exact_match;
		gl_sw_opt.toler = m_sw_opt.toler;
		for(i=0;i<6;i++)
			gl_sw_opt.layer[i] = m_sw_opt.layer[i];

		for(i=0;i<6;i++)
			gl_sw_opt.color[i] = m_sw_opt.color[i];
		gl_sw_opt.cvopt = m_sw_opt.cvopt;
		*m_sw_opt_ptr = m_sw_opt;
		EndDialog(wID);
		return 0;
	}

	LRESULT OnCancel(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		EndDialog(wID);
		return 0;
	}
	LRESULT OnClickedUniopt(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		BOOL flag;

		switch (wID)
		{
		case IDC_UNIOPT_CHECK1:
			m_sw_opt.import_curves = IsDlgButtonChecked(wID);
			if (m_sw_opt.import_curves) flag = TRUE;
			else flag = FALSE;
			::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT6),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_COMBO3),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_EDIT3),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT11),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_COMBO6),flag);
			break;
		case IDC_UNIOPT_CHECK2:
			m_sw_opt.shade_surfs = IsDlgButtonChecked(wID);
			break;
		case IDC_UNIOPT_CHECK3:
			UIG_regressive = IsDlgButtonChecked(wID);
			break;
		case IDC_UNIOPT_CHECK4:
			m_sw_opt.import_sketch = IsDlgButtonChecked(wID);
			if (m_sw_opt.import_sketch) flag = TRUE;
			else flag = FALSE;
			::EnableWindow(GetDlgItem(IDC_UNIOPT_BUTTON4),flag);
			break;
		}
		return 0;
	}

	LRESULT OnSelchangeUniopt(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		// TODO : Add Code for control notification handler.
		int i;
		TCHAR bstr[30];
		char cstr[30];
		UINT flag;
//
//...Get the toggle text
//
		GetDlgItemText(wID,bstr,30);
		i = wcslen(bstr);
		wcstombs(cstr,bstr,i+1);
		switch (wID)
		{
		case IDC_UNIOPT_COMBO1:
			for (i=0;i<2;i++)
			{
				if (strcmp(cstr,c1str[i]) == 0) m_sw_opt.label_opts = i;
			}
			flag = FALSE;
			if (m_sw_opt.label_opts == 1) flag = TRUE;
			::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT7),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT8),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_EDIT4),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_BUTTON2),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_BUTTON3),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_COMBO4),flag);
			if (m_sw_opt.cvopt == 2) flag = TRUE;
			::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT9),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_EDIT5),flag);
			::EnableWindow(GetDlgItem(IDC_CHK_UNMATCH_SEC),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_CHECK3),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT12),flag);
			::EnableWindow(GetDlgItem(IDC_UNIOPT_COMBO7),flag);
//			DlgDirSelectComboBox(lpstr,20,IDC_UNIOPT_COMBO1);
			break;
		case IDC_UNIOPT_COMBO2:
			for (i=0;i<2;i++)
			{
				if (strcmp(cstr,c2str[i]) == 0) m_sw_opt.sflab_type = i;
			}
			break;
		case IDC_UNIOPT_COMBO3:
			for (i=0;i<2;i++)
			{
				if (strcmp(cstr,c2str[i]) == 0) m_sw_opt.cvlab_type = i;
			}
			break;
		case IDC_UNIOPT_COMBO4:
			for (i=0;i<5;i++)
			{
				if (strcmp(cstr,c3str[i]) == 0) m_sw_opt.exact_match = i;
			}
			break;
		case IDC_UNIOPT_COMBO5:
			m_sw_opt.mxname[0] = '\0';
			if (strcmp(cstr,"-- default --")) strcpy (m_sw_opt.mxname,cstr);
			break;
		case IDC_UNIOPT_COMBO6:
			for (i=0;i<3;i++)
			{
				if (strcmp(cstr,c6str[i]) == 0)
				{
					m_sw_opt.cvopt = i;
					flag = FALSE;
					if (m_sw_opt.cvopt == 2 ||
					    m_sw_opt.label_opts == 1) flag = TRUE;
					::EnableWindow(GetDlgItem(IDC_UNIOPT_TEXT9),flag);
					::EnableWindow(GetDlgItem(IDC_UNIOPT_EDIT5),flag);
					break;
				}
			}
			break;
		case IDC_UNIOPT_COMBO7:
			for (i=0;i<2;i++)
			{
				if (strcmp(cstr,c7str[i]) == 0) UIG_start_unmatch = i;
			}
			break;
		}
		return 0;
	}


	LRESULT OnClickedbutton1(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		int nc;
		// TODO : Add Code for control notification handler.
		GetFileName(m_sw_opt.dir,m_sw_opt.unibase,&nc,hWndCtl,1);
		if (nc > 0) SetDlgItemText(IDC_UNIOPT_EDIT1,(CComBSTR)m_sw_opt.unibase);
//
//...End of routine
//
		return 0;
	}
	LRESULT OnClickedbutton2(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		int nc;
		GetFileName(m_sw_opt.dir,m_sw_opt.unimatch,&nc,hWndCtl,0);
		if (nc > 0) SetDlgItemText(IDC_UNIOPT_EDIT4,(CComBSTR)m_sw_opt.unimatch);
		// TODO : Add Code for control notification handler.
		return 0;
	}
	LRESULT OnClickedbutton3(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		CUniAttr *unip = new CUniAttr(m_sw_opt.layer,m_sw_opt.color);
		unip->DoModal();
		delete unip;
		return 0;
	}
	LRESULT OnClickedbutton4(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
	{
		CUniSketch *unip = new CUniSketch(&m_sw_opt);
		unip->DoModal();
		delete unip;
		return 0;
	}
};

#endif //__UNIOPT_H_
