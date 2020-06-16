/************************************************************************
**
**   FILE NAME: NcqMonitor.cpp
**
**	 CONTAINS: 
**		NcqMonitor::NcqMonitor()
**		NcqMonitor::OnInitDialog()
**		NcqMonitor::OnClose() 
**		NcqMonitor::UpdateField() 
**		NcqMonitor::UpdateNCLinfo(char *info, int ver) 
**
**     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncqmonitor.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:03
**
***********************************************************************
*/
// NcqMonitor.cpp : implementation file
//

#include "stdafx.h"
#include "ncq.h"
#include "ncqDoc.h"
#include "NcqMonitor.h"
#include "NcqView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern CNcqView* NCQ_mainview;
/////////////////////////////////////////////////////////////////////////////
// NcqMonitor dialog


NcqMonitor::NcqMonitor(CWnd* pParent /*=NULL*/)
	: CDialog(NcqMonitor::IDD, pParent)
{
	m_ppfile[0] = '\0';
	m_current = m_highest = m_lines = 0;
	m_macro[0] = '\0';
	m_warn = m_error = 0;
}


BEGIN_MESSAGE_MAP(NcqMonitor, CDialog)
	//{{AFX_MSG_MAP(NcqMonitor)
	ON_WM_CLOSE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// NcqMonitor message handlers

/***********************************************************************
c
c   SUBROUTINE:  OnClose
c
c   FUNCTION:  This function called when pushed 'close' button 
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void NcqMonitor::OnClose() 
{
	CDialog::OnClose();
	((CNcqView*)NCQ_mainview)->m_monitor = NULL;
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
BOOL NcqMonitor::OnInitDialog() 
{
	CDialog::OnInitDialog();
	UpdateField();	
	return TRUE;  
}
/***********************************************************************
c
c   SUBROUTINE:  UpdateNCLinfo(char *info, int ver)
c
c   FUNCTION:  This function update the momitor with NCLInfo
c
c   INPUT:  info: ncl information used to update monitor
c			ver: NCL version
c
c   OUTPUT: none
c
c***********************************************************************
*/
int NcqMonitor::UpdateNCLinfo(char *info, int ver) 
{
	NCLInfo *info97;
	NCLInfo96 *info96;
	if (info==NULL)
		return 0;
	if (ver>=97)
	{
		info97 = (NCLInfo *)info;
		if (strlen(info97->ppfile)>0)
			strcpy(m_ppfile, info97->ppfile);
		else
			m_ppfile[0] = '\0';
		if (strlen(info97->macro)>0)
			strcpy(m_macro, info97->macro);
		else
			m_macro[0] = '\0';
		m_current = info97->current;
		m_highest = info97->highest;
		m_lines = info97->lines;
		m_warn = info97->warn;
		m_error = info97->error;
	}
	else
	{
		info96 = (NCLInfo96 *)info;
		if (strlen(info96->ppfile)>0)
			strcpy(m_ppfile, info96->ppfile);
		else
			m_ppfile[0] = '\0';
		if (strlen(info96->macro)>0)
			strcpy(m_macro, info96->macro);
		else
			m_macro[0] = '\0';
		m_current = info96->current;
		m_highest = info96->highest;
		m_lines = info96->lines;
		m_warn = info96->warn;
		m_error = info96->error;
	}
	UpdateField();
	return 1;
}

/***********************************************************************
c
c   SUBROUTINE:  UpdateField()
c
c   FUNCTION:  This function update the momitor with data member
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void NcqMonitor::UpdateField() 
{
	SetDlgItemText(ID_MONITOR_FILE, m_ppfile);
	SetDlgItemText(ID_MONITOR_MACRO, m_macro);
	SetDlgItemInt(ID_MONITOR_CUR, m_current);
	SetDlgItemInt(ID_MONITOR_HIGH, m_highest);
	SetDlgItemInt(ID_MONITOR_LINES, m_lines);
	SetDlgItemInt(ID_MONITOR_WARN, m_warn);
	SetDlgItemInt(ID_MONITOR_ERROR, m_error);
	UpdateWindow();
}
