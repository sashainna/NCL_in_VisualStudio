/************************************************************************
c
c   FILE NAME: wsntModalFrame.cpp
c
c	 CONTAINS: 
c		Functions for the class CNCLModalFrame and CModalFrameDlg
c
c     COPYRIGHT 2014 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c         wsntModalFrame.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c         04/29/15 , 15:12:19
c**********************************************************************
*/
#include "stdafx.h"
#include <afxpriv.h>
#include "wsntModalFrame.h"
#include "wsntFdsnFrm.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNCLModalFrame::

/***********************************************************************
**
**   FUNCTION: End(CFrameWnd * pModalFrameWnd, int nResult)
**
**		Function to end a ModalFrame (close a modal frame dialog)
**   
**		INPUT:  pModalFrameWnd: The Modal frame to be closed
**				nResult: result to be pass on
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLModalFrame::End(CFrameWnd * pModalFrameWnd, int nResult)
{
	ASSERT(pModalFrameWnd && ::IsWindow(pModalFrameWnd->m_hWnd));
	if (CModalFrameDlg * pLock = CModalFrameDlg::FromHandle(pModalFrameWnd->m_hWnd))
	{
		pLock->EndModal(nResult);
		return;
	}
	ASSERT(false);
}

/***********************************************************************
**
**   FUNCTION: Run(CRuntimeClass & FrameWndClass,
					  bool ParentPlacement,
					  UINT nIDResource,
					  char *filename, int flag,
					  DWORD dwDefaultStyle,
					  CWnd * pParentWnd,
					  CCreateContext * pContext)
**
**		Function to open a ModalFrame and get the return value
**   
**		INPUT:  
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CNCLModalFrame::Run(CRuntimeClass & FrameWndClass,
					  bool ParentPlacement,
					  UINT nIDResource,
					  char *filename, int flag,
					  DWORD dwDefaultStyle,
					  CWnd * pParentWnd,
					  CCreateContext * pContext)
{
	ASSERT(!(dwDefaultStyle & WS_CHILD));
	if (dwDefaultStyle & WS_CHILD)
		return 0;

	ASSERT(nIDResource != 0); // must have a resource ID to load from
	ASSERT(&FrameWndClass);
	ASSERT(FrameWndClass.IsDerivedFrom(RUNTIME_CLASS(CFrameWnd)));

	CModalFrameDlg dlg(pParentWnd);
	
	dlg.m_pFrameWndClass = &FrameWndClass;
	dlg.m_ParentPlacement = ParentPlacement;
	dlg.m_nIDResource = nIDResource;
	dlg.m_dwDefaultStyle = dwDefaultStyle;
	dlg.m_pContext = pContext;
	strcpy(dlg.m_filename, filename);
	dlg.m_flag = flag;
	dlg.DoModal();
	return dlg.m_result;
}

/////////////////////////////////////////////////////////////////////////////
// CModalFrameDlg::

/***********************************************************************
c
c   FUNCTION: CModalFrameDlg(CWnd * pParentWnd)
c
c              Constructor of class CModalFrameDlg
c
c   INPUT:  pParentWnd: parent window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CModalFrameDlg::CModalFrameDlg(CWnd * pParentWnd)
	: m_pParentWnd(pParentWnd)
	, m_pFrameWnd(NULL)
	, m_WndProc(NULL)
{
	if (this == &m_barier)
		m_prev = m_next = this;
	else
		(((m_next = m_barier.m_next)->m_prev = this)->m_prev = &m_barier)->m_next = this;
}

/***********************************************************************
c
c   FUNCTION: ~CModalFrameDlg(CWnd * pParentWnd)
c
c             Deconstructor of class CModalFrameDlg
c
c   INPUT:  pParentWnd: parent window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CModalFrameDlg::~CModalFrameDlg()
{
	if (this != &m_barier)
	{
		(m_prev->m_next = m_next)->m_prev = m_prev;
		m_prev = m_next = NULL;
	}
}

CModalFrameDlg CModalFrameDlg::m_barier(NULL);

CModalFrameDlg * CModalFrameDlg::FromHandle(HWND hwnd)
{
	for (CModalFrameDlg * c = m_barier.m_next; c != &m_barier; c = c->m_next)
	{
		if (c->m_hWnd == hwnd)
		{
			ASSERT(c->m_pFrameWnd && c->m_pFrameWnd->m_hWnd == hwnd);
			return c;
		}
	}
	return NULL;
}

/**********************************************************************
**    I_FUNCTION :  FrameSubclassProc(HWND hwnd, UINT uMsg,
**					WPARAM wParam,	LPARAM lParam)
**		Provides a Windows procedure for a this object.
**		 It dispatches messages through the window's message map.       
**
**    PARAMETERS   
**       INPUT  : hwnd: window with the message
**				uMsg: window message to be processed.
**				wParam:   Provides additional information used in
**						 processing the message
**				lParam:   Provides additional information used in 
**							processing the message. 
**
**       OUTPUT :  
**          none
**    RETURNS      : The return value depends on the message.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
LRESULT CALLBACK CModalFrameDlg::FrameSubclassProc(HWND hwnd,
													UINT uMsg,
													WPARAM wParam,
													LPARAM lParam)
{
	CModalFrameDlg * pLock = FromHandle(hwnd);
	if (uMsg == WM_NCDESTROY)
	{
		if (pLock->m_nFlags & WF_CONTINUEMODAL)
			pLock->EndModal(-1);

		ASSERT(::GetWindowLongPtr(hwnd, GWLP_WNDPROC) == LONG_PTR(FrameSubclassProc));

		WNDPROC prevWndProc = pLock->m_WndProc;
		pLock->m_WndProc = NULL;
		::SetWindowLongPtr(hwnd, GWLP_WNDPROC, (LONG_PTR)prevWndProc);

		pLock->m_hWnd = NULL;
		pLock->m_pFrameWnd = NULL;

		return CallWindowProc(prevWndProc, hwnd, uMsg, wParam, lParam);
	}
	if (uMsg == WM_KICKIDLE)
	{
		CWnd * pFrame = pLock->m_pFrameWnd;
		if (pFrame->IsWindowVisible())
		{
			AfxCallWndProc(pFrame, pFrame->m_hWnd,
				WM_IDLEUPDATECMDUI, (WPARAM)true, 0);
			pFrame->SendMessageToDescendants(WM_IDLEUPDATECMDUI,
				(WPARAM)true, 0, true, true);
		}
	}
	return CallWindowProc(pLock->m_WndProc, hwnd, uMsg, wParam, lParam);
}

/***********************************************************************
c
c   FUNCTION: CreateFrame(CWnd * pParentWnd)
c
c             Create the frame window
c
c   INPUT:  pParentWnd: parent window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CFrameWnd * CModalFrameDlg::CreateFrame(CWnd * pParentWnd)
{
	CFrameWnd * pFrame = static_cast<CFrameWnd *>(m_pFrameWndClass->CreateObject());

	if (pFrame == NULL)
	{
		MessageBox("Dynamic create of frame Dialog failed.\n", "Error", MB_OK);
		return NULL;
	}
	((CNCLFdsnFrame*)pFrame)->SetFileflag(m_filename, m_flag);
	if (!pFrame->LoadFrame(m_nIDResource, m_dwDefaultStyle, pParentWnd, m_pContext))
	{
		MessageBox("Couldn't create a frame.\n", "Error", MB_OK);
		return NULL;
	}
		
	WNDPROC wndproc = FrameSubclassProc;
	m_WndProc = (WNDPROC)SetWindowLongPtr(*pFrame, GWLP_WNDPROC, LONG_PTR(wndproc));
	m_hWnd = (m_pFrameWnd = pFrame)->m_hWnd;
	return pFrame;
}

/***********************************************************************
c
c   FUNCTION: EndModal(int nResult)
c
c             End the modal loop with rhe value nResult
c
c   INPUT:  nResult: end loop value
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CModalFrameDlg::EndModal(int nResult)
{
	ASSERT(::IsWindow(m_hWnd));
	if (m_nFlags & (WF_MODALLOOP|WF_CONTINUEMODAL))
		EndModalLoop(nResult);
}

/***********************************************************************
c
c   FUNCTION: DoModal()
c
c             DoModal loop
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CModalFrameDlg::DoModal()
{
	if (CWinApp* pApp = AfxGetApp())
		pApp->EnableModeless(false);
	HWND hWndTop;
	HWND hWndParent = CWnd::GetSafeOwner_(m_pParentWnd->GetSafeHwnd(), &hWndTop);

	m_result = 0;
	bool bEnableParent = false;
	if (hWndParent != NULL && ::IsWindowEnabled(hWndParent))
	{
		::EnableWindow(hWndParent, false);
		bEnableParent = true;
	}
	m_nModalResult = -1;
	m_nFlags |= WF_CONTINUEMODAL;
	CFrameWnd * pFrameWnd;
	TRY
	{
		{
			if (pFrameWnd = CreateFrame(CWnd::FromHandle(hWndParent)))
			{
				if (m_pParentWnd && m_ParentPlacement)
				{
					WINDOWPLACEMENT wp = {0};
					wp.length = sizeof(wp);
					if (CWnd * pParentWnd = m_pParentWnd->GetTopLevelParent())
					{
						pParentWnd->GetWindowPlacement(&wp);
						pFrameWnd->SetWindowPlacement(&wp);
					}
				}
				if (m_pContext && m_pContext->m_pCurrentDoc)
				{
					CDocument * pDocument = m_pContext->m_pCurrentDoc;
					CDocTemplate * pTemplate = pDocument->GetDocTemplate();
					pTemplate->InitialUpdateFrame(pFrameWnd, pDocument);
				}
				else
				{
					pFrameWnd->InitialUpdateFrame(NULL, true);
				}
			}
		}
		if (!m_hWnd || !::IsWindow(m_hWnd))
		{
			MessageBox("Failed to create new frame.\n", "Error", MB_OK);
			EndModal(-2);
		}
		if (m_nFlags & WF_CONTINUEMODAL)
		{
			DWORD dwFlags = MLF_SHOWONIDLE;
			if (GetStyle() & DS_NOIDLEMSG)
				dwFlags |= MLF_NOIDLEMSG;
			VERIFY(RunModalLoop(dwFlags) == m_nModalResult);
			m_result = ((CNCLFdsnFrame*)pFrameWnd)->m_rep;
		}
		if (m_hWnd != NULL)
			SetWindowPos(NULL, 0, 0, 0, 0, SWP_HIDEWINDOW|
				SWP_NOSIZE|SWP_NOMOVE|SWP_NOACTIVATE|SWP_NOZORDER);
	}
	CATCH_ALL(e)
	{
		e->Delete();
		m_nModalResult = -2;
	}
	END_CATCH_ALL

	if (bEnableParent)
		::EnableWindow(hWndParent, true);
	
	if (hWndParent != NULL && ::GetActiveWindow() == m_hWnd)
		::SetActiveWindow(hWndParent);
	if (m_hWnd)
	{
		DestroyWindow();
		ASSERT(!m_hWnd);
	}
	if (::IsWindow(hWndTop))
		::EnableWindow(hWndTop, true);
	if (CWinApp* pApp = AfxGetApp())
		pApp->EnableModeless(true);
	return m_nModalResult;
}

/////////////////////////////////////////////////////////////////////////////
