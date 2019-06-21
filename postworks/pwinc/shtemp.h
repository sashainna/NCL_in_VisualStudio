/************************************************************************
c
c   FILE NAME: Shtemp.h
c
c	 Description - Functions and struct declarations for
c		CShDialogTemplate class (For Dynamic PropertySheet)
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        shtemp.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:41
c
c**********************************************************************
*/

#ifndef SHTEMP_H
#define SHTEMP_H
#include "propsht.h"
#include "DialogItem.h"

class CShDialogTemplate
{
	friend class  CDialogPrompt;
public:
	~CShDialogTemplate();
	void SetParent(CWnd* pParent = NULL);
	void init(CWnd* pParent = NULL, int *level = NULL, int cur_stage = 1, 
		int id = -1,NpwDynWinStruct* winStruct=NULL);
	void CreateIt(int act_page = 0); 
	int m_pagenum;
	CDialog *m_pHelpDialog;
	NpwDynWinStruct m_DynWinStruct;
	DLGTEMPLATE m_dlgTempl[10];
	CDialogItem	m_rgDlgItem[10][80];  
	CWnd* m_pParent;
	int m_ChildId;
	int m_pLevel[10];
	int m_curStage;
	CDlgSheet* sheet;
};

#endif
