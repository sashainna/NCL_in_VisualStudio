/************************************************************************
c
c   FILE NAME: PtedWindow3.cpp
c
c   CONTAINS:
c		CPtedWindow::ConvertRange(PtedRangeStruct cRange)
c		CPtedWindow::OnConvertConvert2() 
c		CPtedWindow::OnConvertConvert() 
c		CPtedWindow::OnConvertBadblocks() 
c		CPtedWindow::OnConvertBadblocks2() 
c		CPtedWindow::OnConvertFormat()
c		CPtedWindow::OnConvertFormat2()
c		CPtedWindow::FormatRange(PtedRangeStruct cRange)
c		CPtedWindow::OnConvertUnformat() 
c		CPtedWindow::OnConvertUnformat2() 
c		CPtedWindow::UnformatRange(PtedRangeStruct cRange)
c		CPtedWindow::OnConvertLength()
c		CPtedWindow::OnConvertLength2()
c		CPtedWindow::OnConvertSetregister() 
c		CPtedWindow::ReseqRange(PtedRangeStruct sRange, int bseq, int seqinc,
c				int seqn, int nonly)
c		CPtedWindow::OnConvertResequence()
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c           PtedWindow3.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c           09/11/13 , 12:59:30
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include "Pted.h"
#include "PtedWindow.h"
#include "PtedChildWindow.h"
#include "PtedMainWindow.h"
#include "PtdGlobal.h"
#include "Ptedres.h"
#include "PtdFunc.h"
#include "PtedReseqDialog.h"
#include "PtedSetDialog.h"
#include <sys/stat.h>
#include <sys/types.h>
#include "PtedProcessDlg.h"

extern CPtedMainWindow *Pted_MainDlg;
extern "C" void Pted_disply_ProcessWindow(char *title, CPtedWindow* parent);
extern "C" void Pted_Display_as_percent(int num, CPtedWindow* parent);
extern "C" void Pted_Close_ProcessWindow(CPtedWindow *parent);
/***********************************************************************
c
c   SUBROUTINE:  OnConvertConvert
c
c   FUNCTION:  This function called when user select "Convert"
c				from Convert Menu (convert whole file)
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertConvert() 
{
	if (m_processor)
	{
		return;
	}
	else
	{
		Pted_disply_ProcessWindow("Pted Input/Output Conversion", this);
		Pted_Display_as_percent(1, this);
	}

	CWaitCursor wait;
	PtedRangeStruct cRange;
	cRange.begin = 1;
	cRange.end = 1;
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&cRange, ptStart, ptEnd);
	if (ptStart!=ptEnd)
	{
		m_TextView->ConvertRange(ptStart, ptEnd);	
		Update_undo_redomenu(1,0);
	}
	Pted_Close_ProcessWindow(this);
}


/***********************************************************************
c
c   SUBROUTINE:  OnConvertConvert2
c
c   FUNCTION:  This function called when user select "Convert"
c				from Convert Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertConvert2() 
{
	int i;
	DWORD flags = TED_RANGE_BEGIN1 | TED_RANGE_BEGIN6 | TED_RANGE_BEGIN7
				| TED_RANGE_BEGIN2 | TED_RANGE_BEGIN3 | TED_RANGE_END4
				| TED_RANGE_BEGIN4 | TED_RANGE_BEGIN5 | TED_RANGE_END1
				| TED_RANGE_END2 | TED_RANGE_END3 | TED_RANGE_END4;
	if (m_cRangeBox!=0)
	{
		m_cRangeBox->SetActiveWindow();
		m_cRangeBox->ShowWindow(SW_SHOW);
		return;
	}

	PtedRangeStruct cRange;
	if (m_TextView->IsSelection())
		cRange.begin = 2;
	else
		cRange.begin = 1;
	cRange.end = 1;
	cRange.baddress[0] = '\0';
	cRange.bstring[0] = '\0';
	cRange.enumber[0] = '\0';
	cRange.eaddress[0] = '\0';
	cRange.estring[0] = '\0';

	m_cRangeBox = new PtedRangeBox(this, flags, &cRange);
	if (m_cRangeBox->DoModal()==IDCANCEL)
	{
		delete m_cRangeBox;
		m_cRangeBox = NULL;
		return;
	}
	if (m_processor)
	{
		return;
	}
	else
	{
		Pted_disply_ProcessWindow("Pted Input/Output Conversion", this);
		Pted_Display_as_percent(1, this);
	}

	CWaitCursor wait;

	if (m_cRangeBox->m_begin1==1)
		cRange.begin = 1;
	if (m_cRangeBox->m_begin2==1)
		cRange.begin = 2;
	if (m_cRangeBox->m_begin3==1)
		cRange.begin = 3;
	if (m_cRangeBox->m_begin4==1)
		cRange.begin = 4;
	if (m_cRangeBox->m_begin5==1)
		cRange.begin = 5;
	if (m_cRangeBox->m_begin6==1)
		cRange.begin = 6;
	if (m_cRangeBox->m_begin7==1)
		cRange.begin = 7;
	if (m_cRangeBox->m_end1==1)
		cRange.end = 1;
	if (m_cRangeBox->m_end2==1)
		cRange.end = 2;
	if (m_cRangeBox->m_end3==1)
		cRange.end = 3;
	if (m_cRangeBox->m_end4==1)
		cRange.end = 4;
	if (m_cRangeBox->m_end5==1)
		cRange.end = 5;
	
	if (m_cRangeBox->m_estring!="")
	{
		for (i=0; i<(m_cRangeBox->m_estring).GetLength(); i++)
			cRange.estring[i] = m_cRangeBox->m_estring[i];
		cRange.estring[i] = '\0';
	}
	if (m_cRangeBox->m_bstring!="")
	{
		for (i=0; i<(m_cRangeBox->m_bstring).GetLength(); i++)
			cRange.bstring[i] = m_cRangeBox->m_bstring[i];
		cRange.bstring[i] = '\0';
	}
	if (m_cRangeBox->m_enumber!="")
	{
		for (i=0; i<(m_cRangeBox->m_enumber).GetLength(); i++)
			cRange.enumber[i] = m_cRangeBox->m_enumber[i];
		cRange.enumber[i]= '\0';	
	}
	if (m_cRangeBox->m_eaddress!="")
	{
		for (i=0; i<(m_cRangeBox->m_eaddress).GetLength(); i++)
			cRange.eaddress[i] = m_cRangeBox->m_eaddress[i];
		cRange.eaddress[i] = '\0';
	}
	if (m_cRangeBox->m_baddress!="")
	{
		for (i=0; i<(m_cRangeBox->m_baddress).GetLength(); i++)
			cRange.baddress[i] = m_cRangeBox->m_baddress[i];
		cRange.baddress[i] = '\0';
	}
	cRange.bline = m_cRangeBox->m_bline;
	cRange.eline = m_cRangeBox->m_eline;
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&cRange, ptStart, ptEnd);
	if (ptStart!=ptEnd)
		m_TextView->ConvertRange(ptStart, ptEnd);	

	delete m_cRangeBox;
	m_cRangeBox = NULL;
	Pted_Close_ProcessWindow(this);
	if (ptStart!=ptEnd)
		Update_undo_redomenu(1,0);
	return;
}
/***********************************************************************
c
c   SUBROUTINE:  ConvertRange(PtedRangeStruct cRange)
c
c   FUNCTION:  This function convert all text in current window
c				in specified range
c
c   INPUT:  cRange: range structure
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::ConvertRange(PtedRangeStruct cRange)
{
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&cRange, ptStart, ptEnd);
	if (ptStart!=ptEnd)
	{
		if (m_processor)
		{
			return;
		}
		else
		{
			Pted_disply_ProcessWindow("Pted Input/Output Conversion", this);
			Pted_Display_as_percent(1, this);
		}
		m_TextView->ConvertRange(ptStart, ptEnd);
		Pted_Close_ProcessWindow(this);
		Update_undo_redomenu(1,0);
	}
}
/***********************************************************************
c
c   SUBROUTINE:  OnConvertBadblocks
c
c   FUNCTION:  This function called when user select "Bad Block"
c				from Convert Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertBadblocks() 
{
	if (m_processor)
	{
		return;
	}
	else
	{
		Pted_disply_ProcessWindow("Pted Bad Block processing...", this);
		Pted_Display_as_percent(1, this);
	}

	CPtedChildWindow* child;
	if (m_pParent==NULL)
		child = new CPtedChildWindow(Pted_MainDlg, "Bad Blocks", 0, 3);
	else
		child = new CPtedChildWindow(m_pParent, "Bad Blocks", 0, 3);
	child->Create(IDD_WINDOWDIALOG);

	CWaitCursor wait;
	PtedRangeStruct cRange;
	cRange.begin = 1;
	cRange.end = 1;
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&cRange, ptStart, ptEnd);
	int stat = 0;
	if (ptStart!=ptEnd)
		stat = m_TextView->ConvertBadBlockRange(ptStart, ptEnd, m_ftype, child->m_TextView);	
	if (stat)
	{
		child->ShowWindow(TRUE);
		if (m_pParent==NULL)
		{
			Pted_MainDlg->Add_Child(child);
		}
		else
			((CPtedMainWindow*)m_pParent)->Add_Child(child);
		child->m_TextView->Get_TextBuffer()->SetModified(0);
	}
	else
	{
		delete child;
	}
	Pted_Close_ProcessWindow(this);
}
/***********************************************************************
c
c   SUBROUTINE:  OnConvertBadblocks2
c
c   FUNCTION:  This function called when user select "Bad Block Range"
c              from Convert Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertBadblocks2() 
{
	int i;
	DWORD flags = TED_RANGE_BEGIN1 | TED_RANGE_BEGIN6 | TED_RANGE_BEGIN7
				| TED_RANGE_BEGIN2 | TED_RANGE_BEGIN3 | TED_RANGE_END5
				| TED_RANGE_BEGIN4 | TED_RANGE_BEGIN5 | TED_RANGE_END1
				| TED_RANGE_END2 | TED_RANGE_END3 | TED_RANGE_END4;


	PtedRangeStruct cRange;
	if (m_TextView->IsSelection())
		cRange.begin = 2;
	else
		cRange.begin = 1;
	cRange.end = 1;
	cRange.baddress[0] = '\0';
	cRange.bstring[0] = '\0';
	cRange.enumber[0] = '\0';
	cRange.eaddress[0] = '\0';
	cRange.estring[0] = '\0';

	PtedRangeBox *RangeBox = new PtedRangeBox(this, flags, &cRange);
	if (RangeBox->DoModal()==IDCANCEL)
	{
		delete RangeBox;
		return;
	}

	if (RangeBox->m_begin1==1)
		cRange.begin = 1;
	if (RangeBox->m_begin2==1)
		cRange.begin = 2;
	if (RangeBox->m_begin3==1)
		cRange.begin = 3;
	if (RangeBox->m_begin4==1)
		cRange.begin = 4;
	if (RangeBox->m_begin5==1)
		cRange.begin = 5;
	if (RangeBox->m_begin6==1)
		cRange.begin = 6;
	if (RangeBox->m_begin7==1)
		cRange.begin = 7;
	if (RangeBox->m_end1==1)
		cRange.end = 1;
	if (RangeBox->m_end2==1)
		cRange.end = 2;
	if (RangeBox->m_end3==1)
		cRange.end = 3;
	if (RangeBox->m_end4==1)
		cRange.end = 4;
	if (RangeBox->m_end5==1)
		cRange.end = 5;
	
	if (RangeBox->m_estring!="")
	{
		for (i=0; i<(RangeBox->m_estring).GetLength(); i++)
			cRange.estring[i] = RangeBox->m_estring[i];
		cRange.estring[i] = '\0';
	}
	if (RangeBox->m_bstring!="")
	{
		for (i=0; i<(RangeBox->m_bstring).GetLength(); i++)
			cRange.bstring[i] = RangeBox->m_bstring[i];
		cRange.bstring[i] = '\0';
	}
	if (RangeBox->m_enumber!="")
	{
		for (i=0; i<(RangeBox->m_enumber).GetLength(); i++)
			cRange.enumber[i] = RangeBox->m_enumber[i];
		cRange.enumber[i]= '\0';	
	}
	if (RangeBox->m_eaddress!="")
	{
		for (i=0; i<(RangeBox->m_eaddress).GetLength(); i++)
			cRange.eaddress[i] = RangeBox->m_eaddress[i];
		cRange.eaddress[i] = '\0';
	}
	if (RangeBox->m_baddress!="")
	{
		for (i=0; i<(RangeBox->m_baddress).GetLength(); i++)
			cRange.baddress[i] = RangeBox->m_baddress[i];
		cRange.baddress[i] = '\0';
	}
	cRange.bline = RangeBox->m_bline;
	cRange.eline = RangeBox->m_eline;

	if (m_processor)
	{
		return;
	}
	else
	{
		Pted_disply_ProcessWindow("Pted Bad Block", this);
		Pted_Display_as_percent(-50, this);
	}
	CPtedChildWindow* child;
	if (m_pParent==NULL)
		child = new CPtedChildWindow(Pted_MainDlg, "Bad Blocks", 0, 3);
	else
		child = new CPtedChildWindow(m_pParent, "Bad Blocks", 0, 3);
	child->Create(IDD_WINDOWDIALOG);

	CWaitCursor wait;
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&cRange, ptStart, ptEnd);
	int stat = 0;
	if (ptStart!=ptEnd)
		stat = m_TextView->ConvertBadBlockRange(ptStart, ptEnd, m_ftype, child->m_TextView);	
	if (stat)
	{
		child->ShowWindow(TRUE);
		if (m_pParent==NULL)
		{
			Pted_MainDlg->Add_Child(child);
		}
		else
			((CPtedMainWindow*)m_pParent)->Add_Child(child);
		child->m_TextView->Get_TextBuffer()->SetModified(0);
	}
	else
	{
		delete child;
		Disp_Msg("No bad blocks found", 3);
	}
	Pted_Close_ProcessWindow(this);
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertFormat
c
c   FUNCTION:  This function called when user select "Format"
c				from Convert Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertFormat() 
{
	CWaitCursor wait;
	PtedRangeStruct cRange;
	cRange.begin = 1;
	cRange.end = 1;
	CPoint ptStart, ptEnd;
	FormatRange(cRange);
}
/***********************************************************************
c
c   SUBROUTINE:  OnConvertFormat2
c
c   FUNCTION:  This function called when user select "Format Range"
c              from Convert Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertFormat2() 
{
	int i;
	DWORD flags = TED_RANGE_BEGIN1 | TED_RANGE_BEGIN6 | TED_RANGE_BEGIN7
				| TED_RANGE_BEGIN2 | TED_RANGE_BEGIN3 | TED_RANGE_END5
				| TED_RANGE_BEGIN4 | TED_RANGE_BEGIN5 | TED_RANGE_END1
				| TED_RANGE_END2 | TED_RANGE_END3 | TED_RANGE_END4;
	PtedRangeStruct cRange;
	if (m_TextView->IsSelection())
		cRange.begin = 2;
	else
		cRange.begin = 1;
	cRange.end = 1;
	cRange.baddress[0] = '\0';
	cRange.bstring[0] = '\0';
	cRange.enumber[0] = '\0';
	cRange.eaddress[0] = '\0';
	cRange.estring[0] = '\0';

	PtedRangeBox *RangeBox = new PtedRangeBox(this, flags, &cRange);
	if (RangeBox->DoModal()==IDCANCEL)
	{
		delete RangeBox;
		return;
	}
	if (m_processor)
	{
		return;
	}
	else
	{
		Pted_disply_ProcessWindow("Pted Format", this);
		Pted_Display_as_percent(1, this);
	}
	CWaitCursor wait;

	if (RangeBox->m_begin1==1)
		cRange.begin = 1;
	if (RangeBox->m_begin2==1)
		cRange.begin = 2;
	if (RangeBox->m_begin3==1)
		cRange.begin = 3;
	if (RangeBox->m_begin4==1)
		cRange.begin = 4;
	if (RangeBox->m_begin5==1)
		cRange.begin = 5;
	if (RangeBox->m_begin6==1)
		cRange.begin = 6;
	if (RangeBox->m_begin7==1)
		cRange.begin = 7;
	if (RangeBox->m_end1==1)
		cRange.end = 1;
	if (RangeBox->m_end2==1)
		cRange.end = 2;
	if (RangeBox->m_end3==1)
		cRange.end = 3;
	if (RangeBox->m_end4==1)
		cRange.end = 4;
	if (RangeBox->m_end5==1)
		cRange.end = 5;
	
	if (RangeBox->m_estring!="")
	{
		for (i=0; i<(RangeBox->m_estring).GetLength(); i++)
			cRange.estring[i] = RangeBox->m_estring[i];
		cRange.estring[i] = '\0';
	}
	if (RangeBox->m_bstring!="")
	{
		for (i=0; i<(RangeBox->m_bstring).GetLength(); i++)
			cRange.bstring[i] = RangeBox->m_bstring[i];
		cRange.bstring[i] = '\0';
	}
	if (RangeBox->m_enumber!="")
	{
		for (i=0; i<(RangeBox->m_enumber).GetLength(); i++)
			cRange.enumber[i] = RangeBox->m_enumber[i];
		cRange.enumber[i]= '\0';	
	}
	if (RangeBox->m_eaddress!="")
	{
		for (i=0; i<(RangeBox->m_eaddress).GetLength(); i++)
			cRange.eaddress[i] = RangeBox->m_eaddress[i];
		cRange.eaddress[i] = '\0';
	}
	if (RangeBox->m_baddress!="")
	{
		for (i=0; i<(RangeBox->m_baddress).GetLength(); i++)
			cRange.baddress[i] = RangeBox->m_baddress[i];
		cRange.baddress[i] = '\0';
	}
	cRange.bline = RangeBox->m_bline;
	cRange.eline = RangeBox->m_eline;

	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&cRange, ptStart, ptEnd);
	if (ptStart!=ptEnd)
		m_TextView->ConvertFormatRange(ptStart, ptEnd);	

	delete RangeBox;
	Pted_Close_ProcessWindow(this);
	if (ptStart!=ptEnd)
		Update_undo_redomenu(1,0);
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  FormatRange(PtedRangeStruct cRange)
c
c   FUNCTION:  This function Format all text in current window
c				in specified range
c
c   INPUT:  cRange: range structure
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::FormatRange(PtedRangeStruct cRange)
{
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&cRange, ptStart, ptEnd);
	if (ptStart!=ptEnd)
	{
		if (m_processor)
		{
			return;
		}
		else
		{
			Pted_disply_ProcessWindow("Pted Format", this);
			Pted_Display_as_percent(1, this);
		}
		m_TextView->ConvertFormatRange(ptStart, ptEnd);	
		Pted_Close_ProcessWindow(this);
		Update_undo_redomenu(1,0);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertUnformat
c
c   FUNCTION:  This function called when user select "Unformat"
c				from Convert Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertUnformat() 
{
	CWaitCursor wait;
	PtedRangeStruct cRange;
	cRange.begin = 1;
	cRange.end = 1;
	UnformatRange(cRange);
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertUnformat2
c
c   FUNCTION:  This function called when user select "Unformat Range"
c				from Convert Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertUnformat2() 
{
	int i;
	DWORD flags = TED_RANGE_BEGIN1 | TED_RANGE_BEGIN6 | TED_RANGE_BEGIN7
				| TED_RANGE_BEGIN2 | TED_RANGE_BEGIN3 | TED_RANGE_END5
				| TED_RANGE_BEGIN4 | TED_RANGE_BEGIN5 | TED_RANGE_END1
				| TED_RANGE_END2 | TED_RANGE_END3 | TED_RANGE_END4;


	PtedRangeStruct cRange;
	if (m_TextView->IsSelection())
		cRange.begin = 2;
	else
		cRange.begin = 1;
	cRange.end = 1;
	cRange.baddress[0] = '\0';
	cRange.bstring[0] = '\0';
	cRange.enumber[0] = '\0';
	cRange.eaddress[0] = '\0';
	cRange.estring[0] = '\0';

	PtedRangeBox *RangeBox = new PtedRangeBox(this, flags, &cRange);
	if (RangeBox->DoModal()==IDCANCEL)
	{
		delete RangeBox;
		return;
	}
	if (m_processor)
	{
		return;
	}
	else
	{
		Pted_disply_ProcessWindow("Pted Unformat", this);
		Pted_Display_as_percent(1, this);
	}
	CWaitCursor wait;

	if (RangeBox->m_begin1==1)
		cRange.begin = 1;
	if (RangeBox->m_begin2==1)
		cRange.begin = 2;
	if (RangeBox->m_begin3==1)
		cRange.begin = 3;
	if (RangeBox->m_begin4==1)
		cRange.begin = 4;
	if (RangeBox->m_begin5==1)
		cRange.begin = 5;
	if (RangeBox->m_begin6==1)
		cRange.begin = 6;
	if (RangeBox->m_begin7==1)
		cRange.begin = 7;
	if (RangeBox->m_end1==1)
		cRange.end = 1;
	if (RangeBox->m_end2==1)
		cRange.end = 2;
	if (RangeBox->m_end3==1)
		cRange.end = 3;
	if (RangeBox->m_end4==1)
		cRange.end = 4;
	if (RangeBox->m_end5==1)
		cRange.end = 5;
	
	if (RangeBox->m_estring!="")
	{
		for (i=0; i<(RangeBox->m_estring).GetLength(); i++)
			cRange.estring[i] = RangeBox->m_estring[i];
		cRange.estring[i] = '\0';
	}
	if (RangeBox->m_bstring!="")
	{
		for (i=0; i<(RangeBox->m_bstring).GetLength(); i++)
			cRange.bstring[i] = RangeBox->m_bstring[i];
		cRange.bstring[i] = '\0';
	}
	if (RangeBox->m_enumber!="")
	{
		for (i=0; i<(RangeBox->m_enumber).GetLength(); i++)
			cRange.enumber[i] = RangeBox->m_enumber[i];
		cRange.enumber[i]= '\0';	
	}
	if (RangeBox->m_eaddress!="")
	{
		for (i=0; i<(RangeBox->m_eaddress).GetLength(); i++)
			cRange.eaddress[i] = RangeBox->m_eaddress[i];
		cRange.eaddress[i] = '\0';
	}
	if (RangeBox->m_baddress!="")
	{
		for (i=0; i<(RangeBox->m_baddress).GetLength(); i++)
			cRange.baddress[i] = RangeBox->m_baddress[i];
		cRange.baddress[i] = '\0';
	}
	cRange.bline = RangeBox->m_bline;
	cRange.eline = RangeBox->m_eline;

	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&cRange, ptStart, ptEnd);
	if (ptStart!=ptEnd)
		m_TextView->ConvertUnFormatRange(ptStart, ptEnd);	
	delete RangeBox;
	Pted_Close_ProcessWindow(this);
	if (ptStart!=ptEnd)
		Update_undo_redomenu(1,0);
	return;
}
/***********************************************************************
c
c   SUBROUTINE:  UnformatRange(PtedRangeStruct cRange)
c
c   FUNCTION:  This function Unformat all text in current window
c				in specified range
c
c   INPUT:  cRange: range structure
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::UnformatRange(PtedRangeStruct cRange)
{
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&cRange, ptStart, ptEnd);
	if (ptStart!=ptEnd)
	{
		if (m_processor)
		{
			return;
		}
		else
		{
			Pted_disply_ProcessWindow("Pted Unformat", this);
			Pted_Display_as_percent(1, this);
		}
		m_TextView->ConvertUnFormatRange(ptStart, ptEnd);	
		Pted_Close_ProcessWindow(this);
		Update_undo_redomenu(1,0);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertLength
c
c   FUNCTION:  This function called when user select "Length"
c				from Convert Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertLength() 
{
	int llen, filelen;

	CWaitCursor wait;
	CString tmpstr;	

	llen = m_TextView->GetLineCount();
	filelen = m_TextView->GetTextLength(0,0,llen-1,-1);

	double flen = (filelen/10.0)/12.0;
	double mlen = (filelen/10.0)*25.4/1000.0;	
	char msg[200];
	sprintf(msg, "%d lines\n%d characters\nApproximately %.2f feet / %.2f meter of tape", 
				llen, filelen, flen, mlen);
	if (MessageBox(msg, "File Length", MB_OK)==IDOK)
	{
		return;	
	}	

}
/***********************************************************************
c
c   SUBROUTINE:  OnConvertLength2
c
c   FUNCTION:  This function called when user select "Length Range"
c				from Convert Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertLength2() 
{
	int i, clen;
	DWORD flags = TED_RANGE_BEGIN1 | TED_RANGE_BEGIN6 | TED_RANGE_BEGIN7
				| TED_RANGE_BEGIN2 | TED_RANGE_BEGIN3 | TED_RANGE_END5
				| TED_RANGE_BEGIN4 | TED_RANGE_BEGIN5 | TED_RANGE_END1
				| TED_RANGE_END2 | TED_RANGE_END3 | TED_RANGE_END4;


	PtedRangeStruct cRange;
	if (m_TextView->IsSelection())
		cRange.begin = 2;
	else
		cRange.begin = 1;
	cRange.end = 1;
	cRange.baddress[0] = '\0';
	cRange.bstring[0] = '\0';
	cRange.enumber[0] = '\0';
	cRange.eaddress[0] = '\0';
	cRange.estring[0] = '\0';

	PtedRangeBox *RangeBox = new PtedRangeBox(this, flags, &cRange);
	if (RangeBox->DoModal()==IDCANCEL)
	{
		delete RangeBox;
		return;
	}
	CWaitCursor wait;

	if (RangeBox->m_begin1==1)
		cRange.begin = 1;
	if (RangeBox->m_begin2==1)
		cRange.begin = 2;
	if (RangeBox->m_begin3==1)
		cRange.begin = 3;
	if (RangeBox->m_begin4==1)
		cRange.begin = 4;
	if (RangeBox->m_begin5==1)
		cRange.begin = 5;
	if (RangeBox->m_begin6==1)
		cRange.begin = 6;
	if (RangeBox->m_begin7==1)
		cRange.begin = 7;
	if (RangeBox->m_end1==1)
		cRange.end = 1;
	if (RangeBox->m_end2==1)
		cRange.end = 2;
	if (RangeBox->m_end3==1)
		cRange.end = 3;
	if (RangeBox->m_end4==1)
		cRange.end = 4;
	if (RangeBox->m_end5==1)
		cRange.end = 5;
	
	if (RangeBox->m_estring!="")
	{
		for (i=0; i<(RangeBox->m_estring).GetLength(); i++)
			cRange.estring[i] = RangeBox->m_estring[i];
		cRange.estring[i] = '\0';
	}
	if (RangeBox->m_bstring!="")
	{
		for (i=0; i<(RangeBox->m_bstring).GetLength(); i++)
			cRange.bstring[i] = RangeBox->m_bstring[i];
		cRange.bstring[i] = '\0';
	}
	if (RangeBox->m_enumber!="")
	{
		for (i=0; i<(RangeBox->m_enumber).GetLength(); i++)
			cRange.enumber[i] = RangeBox->m_enumber[i];
		cRange.enumber[i]= '\0';	
	}
	if (RangeBox->m_eaddress!="")
	{
		for (i=0; i<(RangeBox->m_eaddress).GetLength(); i++)
			cRange.eaddress[i] = RangeBox->m_eaddress[i];
		cRange.eaddress[i] = '\0';
	}
	if (RangeBox->m_baddress!="")
	{
		for (i=0; i<(RangeBox->m_baddress).GetLength(); i++)
			cRange.baddress[i] = RangeBox->m_baddress[i];
		cRange.baddress[i] = '\0';
	}
	cRange.bline = RangeBox->m_bline;
	cRange.eline = RangeBox->m_eline;
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&cRange, ptStart, ptEnd);

	int llen = ptEnd.y - ptStart.y + 1;
	clen = m_TextView->GetTextLength(ptStart.y,ptStart.x,ptEnd.y,ptEnd.x);

	double flen = (clen/10.0)/12.0;
	double mlen = (clen/10.0)*25.4/1000.0;
	char msg[200];
	sprintf(msg, "%d lines\n%d characters\nApproximately %.2f feet / %.2f meter of tape", 
				llen, clen, flen, mlen);
	if (MessageBox(msg, "File Length", MB_OK)==IDOK)
	{
		delete RangeBox;			
		return;	
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertResequence
c
c   FUNCTION:  This function called when user select "Resequence"
c				from Convert Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertResequence() 
{
	PtedReseqDialog *ResBox = new PtedReseqDialog();
	(ResBox->m_sRange).begin = 1;
	(ResBox->m_sRange).end = 1;
	(ResBox->m_sRange).baddress[0] = '\0';
	(ResBox->m_sRange).bstring[0] = '\0';
	(ResBox->m_sRange).enumber[0] = '\0';
	(ResBox->m_sRange).eaddress[0] = '\0';
	(ResBox->m_sRange).estring[0] = '\0';

	if (ResBox->DoModal()==IDCANCEL)
	{
		delete ResBox;
		return;
	}
	CWaitCursor wait;
	ReseqRange(ResBox->m_sRange, ResBox->m_bseq, ResBox->m_seqinc,
				ResBox->m_seqn, ResBox->m_nonly);
	delete ResBox;
	return;
}
/***********************************************************************
c
c   SUBROUTINE:  ReseqRange(PtedRangeStruct sRange, int bseq, int seqinc,
c				int seqn, int nonly)
c
c   FUNCTION:  This function Reseqence all text in current window
c				in specified range
c
c   INPUT:  cRange: range structure
c			bseq:  beginning sequence number
c			seqinc: sequence increment
c			seqn:	output sequence number every seqn
c			nonly:  sequence numbered block only?		
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::ReseqRange(PtedRangeStruct sRange, int bseq, int seqinc,
				int seqn, int nonly)
{
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&sRange, ptStart, ptEnd);
	if (ptStart!=ptEnd)
	{
		if (m_processor)
		{
			return;
		}
		else
		{
			Pted_disply_ProcessWindow("Pted Resequence", this);
			Pted_Display_as_percent(1, this);
		}
		m_TextView->ReseqRange(ptStart, ptEnd, bseq, seqinc,seqn,nonly);
		Pted_Close_ProcessWindow(this);
		Update_undo_redomenu(1,0);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertSetregister
c
c   FUNCTION:  This function called when user select "Set Register"
c				from Convert Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnConvertSetregister() 
{
	int i;
	PtedSetDialog *SetBox = new PtedSetDialog();
	if (SetBox->DoModal()==IDCANCEL)
	{
		delete SetBox;
		return;
	}
	CWaitCursor wait;
	char setstring[500];
	int len = (SetBox->m_settext).GetLength();
	for (i=0; i<len; i++)
		setstring[i] = (SetBox->m_settext)[i];
	setstring[i] = '\0';
	Ptd_setreg(setstring);	
}
