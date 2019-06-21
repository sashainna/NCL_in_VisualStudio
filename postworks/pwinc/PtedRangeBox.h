/************************************************************************
c
c   FILE NAME: PtedRangeBox.cpp.cpp
c
c	 CONTAINS: 
c	 all PtedRangeBox.cpp class: a dialog with all the Range input
c			varibles and functions
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        PtedRangeBox.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:34
c
c**********************************************************************
*/
#if !defined(AFX_PTEDRANGEBOX_H__76F2A242_3F2C_11D3_BA4F_444553540000__INCLUDED_)
#define AFX_PTEDRANGEBOX_H__76F2A242_3F2C_11D3_BA4F_444553540000__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// PtedRangeBox.h : header file
//
#include "ptedres.h"
#define TED_RANGE_BEGIN1 0x00000001
#define TED_RANGE_BEGIN2 0x00000002
#define TED_RANGE_BEGIN3 0x00000004
#define TED_RANGE_BEGIN4 0x00000008
#define TED_RANGE_BEGIN5 0x00000010
#define TED_RANGE_BEGIN6 0x00000020

#define TED_RANGE_END1 0x00000040
#define TED_RANGE_END2 0x00000080
#define TED_RANGE_END3 0x00000100
#define TED_RANGE_END4 0x00000200
#define TED_RANGE_BEGIN7   0x00000400
#define TED_RANGE_END5     0x00000800

/////////////////////////////////////////////////////////////////////////////
// PtedRangeBox dialog

typedef struct
{
	int begin;
	int end;
	char baddress[256];
	char bstring[256];
	char enumber[256];
	char eaddress[256];
	char estring[256];
	int bline, eline;
} PtedRangeStruct;

class PtedRangeBox : public CDialog
{
// Construction
public:
	CString m_estring;
	CString m_bstring;
	CString m_eaddress;
	CString m_baddress;
	CString m_enumber;
	int m_end1,m_end2,m_end3,m_end4,m_end5;
	int m_begin1,m_begin2,m_begin3,m_begin4,m_begin5,m_begin6,m_begin7;
	int m_bline, m_eline;
	PtedRangeBox(CWnd* pParent = NULL, DWORD flags = 0, PtedRangeStruct *range=NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(PtedRangeBox)
	enum { IDD = IDD_RANGE_DIALOG };
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(PtedRangeBox)
	protected:
		virtual BOOL OnInitDialog();
		virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	DWORD         m_flags;
	void Disable_End();
	void Enable_End();
	// Generated message map functions
	//{{AFX_MSG(PtedRangeBox)
	virtual void OnOK();
	virtual void OnCancel();
	afx_msg void OnRadio1();
	afx_msg void OnRadio3();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PTEDRANGEBOX_H__76F2A242_3F2C_11D3_BA4F_444553540000__INCLUDED_)
