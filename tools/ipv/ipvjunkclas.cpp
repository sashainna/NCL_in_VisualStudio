/************************************************************************
**
**   FILE NAME: wsntjunkfv.cpp
**
**	 Description - Functions and implementations for
**		CNCLFormView class (junk routine)
**
**	 CONTAINS: 
**		class functions of CNCLFormView class
**
**    COPYRIGHT 2015 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			ipvjunkclas.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			05/18/15 , 11:20:55
***********************************************************************
*/
#include "wsntstdafx.h"
#include "nclipv.h"
#include "wsntdoc.h"
#include "wsntfrmview.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

CNCLMnDropTarget2::CNCLMnDropTarget2()
{
}
DWORD CNCLMnDropTarget2::GotDrag()
{
	return 0;	
}
void CNCLMnDropTarget2::GotLeave()
{
}
DWORD CNCLMnDropTarget2::GotEnter()
{
	return 0;	
}
void CNCLMnDropTarget2::GotDrop()
{
}
/////////////////////////////////////////////////////////////////////////////
// CNCLFormView

IMPLEMENT_DYNCREATE(CNCLFormView, CFormView)

BEGIN_MESSAGE_MAP(CNCLFormView, CFormView)
	//{{AFX_MSG_MAP(CNCLFormView)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard printing commands
END_MESSAGE_MAP()

/***********************************************************************
c
c   FUNCTION: OnDeleteSecBut(WPARAM wParam, LPARAM lParam)
c
c          message callback for delete a select section button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
LRESULT CNCLFormView::OnDeleteSecBut(WPARAM wParam, LPARAM lParam)
{
	return 0;
}

/***********************************************************************
c
c   FUNCTION: OnSectionSel(UINT id)
c
c          Callback function for command
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormView::OnSectionSel(UINT id)
{
	return;
}
void CNCLFormView::OnSectionSelItem(int page)
{
	return;
}

/***********************************************************************
**   FUNCTION: CNCLFormView
**		Constructor of class CNCLFormView
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormView::CNCLFormView() : CFormView(CNCLFormView::IDD)
{
}

/***********************************************************************
**
**   FUNCTION: ~CNCLFormView
**              Destructor of class CNCLFormView, free space.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormView::~CNCLFormView()
{
}

int CNCLFormView::get_valid_id()
{
	return -1;
}
/***********************************************************************
**
**   FUNCTION: LoadFormItem(UD_FSTRUCT *fstruct)
**			Create all form item according to the form structure
**		
**	 INPUT:  fstruct: form structure
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLFormView::LoadFormItem(UD_FSTRUCT *fstruct)
{
}

void CNCLFormView::SetSecInfo(int indx, char *name, char *color)
{
}
void CNCLFormView::SetSectionAttr(int indx, CString label, CString color)
{
}
	
void CNCLFormView::OnDeleteSec(int indx)
{
}
void CNCLFormView::OnInsertSec(int itemnum)
{
}

void CNCLFormView::OnChangeSecColor(int itemnum)
{
}


/***********************************************************************
c
c   FUNCTION: PreTranslateMessage(MSG* pMsg) 
c
c       translate window messages before they are dispatch
c		we don't want to display Combo box dropdown box
c
c   INPUT:  pMsg   Points to a MSG structure that contains the 
c					message to process.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CNCLFormView::PreTranslateMessage(MSG* pMsg)
{
	return CFormView::PreTranslateMessage(pMsg);
}

void CNCLFormView::UpdatePropertySize(int cx, int cy)
{
}

void CNCLFormView::UpdatePropertyPos(int cx, int cy)
{
}

void CNCLFormView::UpdatePropertyHSPTSize(CRect sizerec, float prect[4])
{
}

void CNCLFormView::OpenPropertyPage(CNCLFormProp *prop_dlg, int flag)
{
}

void CNCLFormView::OnUndo()
{
}

void CNCLFormView::OnRedo()
{
}

BOOL CNCLFormView::PreCreateWindow(CREATESTRUCT& cs)
{
	return CFormView::PreCreateWindow(cs);
}

void CNCLFormView::CreateForm()
{
}

void CNCLFormView::OnDestroy() 
{
	CView::OnDestroy();	
}

void CNCLFormView::SizeForm(int type)
{
}
void CNCLFormView::OnInitialUpdate()
{
	CFormView::OnInitialUpdate();
}

void CNCLFormView::OnSize(UINT nType, int cx, int cy) 
{
	CFormView::OnSize(nType, cx, cy);
}

BOOL CNCLFormView::OnEraseBkgnd(CDC* pDC) 
{
	return TRUE;
}

void CNCLFormView::OnDraw(CDC* pDC)
{
}

int CNCLFormView::GetToggle()
{	
	return 0;
}

void CNCLFormView::CreateSectionButton(CRect rect)
{
}
void CNCLFormView::OnMouseMove(UINT nFlags, CPoint point) 
{		
	CFormView::OnMouseMove(nFlags, point);
}

void CNCLFormView::OnItemMouseMove(int itemnum)
{
}

void CNCLFormView::SetSection(int type)
{
}
void CNCLFormView::SetType(int type)
{
}
void CNCLFormView::OnDragDropSection(int secnum, char *drop_text)
{
}
void CNCLFormView::OnChangeSecProperty(int itemnum, int flag)
{
}
/////////////////////////////////////////////////////////////////////////////
// CNCLFormView diagnostics

#ifdef _DEBUG
void CNCLFormView::AssertValid() const
{
	CFormView::AssertValid();
}


CNCLFormDoc* CNCLFormView::GetDocument() // non-debug version is inline
{
	return (CNCLFormDoc*)m_pDocument;
}
#endif 
