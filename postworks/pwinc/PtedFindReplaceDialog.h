/************************************************************************
c
c   FILE NAME: PtedFindReplaceDialog.h
c
c	 CONTAINS: 
c	 all PtedFindReplaceDialog class: CFindReplaceDialog with "Range" and
c			some extra fields 
c			variables and function definition
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        PtedFindReplaceDialog.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:34 
c
c**********************************************************************
*/
#ifndef PTEDFINDREPLACEDIALOG_H
#define PTEDFINDREPLACEDIALOG_H


#include <DLGS.h>
#include "ptedres.h"
#include "PtedRangeBox.h"

// PtedFindReplaceDialog.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// PtedFindReplaceDialog dialog

class PtedFindReplaceDialog : public CFindReplaceDialog
{
	DECLARE_DYNAMIC(PtedFindReplaceDialog)

public:

	PtedRangeStruct m_fRange;
	int m_letter;
	int m_up, m_down;
	PtedFindReplaceDialog();
// Dialog Data
	//{{AFX_DATA(PtedFindReplaceDialog)
	
	//}}AFX_DATA

protected:
	PtedRangeBox *m_fRangeBox;
	//{{AFX_MSG(PtedFindReplaceDialog)
		// NOTE - the ClassWizard will add and remove member functions here.
		afx_msg void OnRange();
		virtual void DoDataExchange(CDataExchange* pDX);   
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
#endif
