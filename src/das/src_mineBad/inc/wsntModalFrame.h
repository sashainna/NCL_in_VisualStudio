/************************************************************************
**
**   FILE NAME: wsntModalFrame.h
**
**       Description - Functions and struct declarations for
**              CNCLModalFrame class - form view used for form item property disply 
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntModalFrame.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:14
**********************************************************************
*/
#if !defined(WSNTMODALFRAME_INCLUDED_)
#define WSNTMODALFRAME_INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif

struct CNCLModalFrame
{
	static void End(CFrameWnd * FrameWnd, int nResult);
	static int Run(CRuntimeClass & FrameWndClass,
					bool ParentPlacement,
					UINT nIDFrameResource,
					char *filename, int flag,
					DWORD dwDefaultStyle = WS_OVERLAPPEDWINDOW | FWS_ADDTOTITLE,
					CWnd * pParentWnd = NULL,
					CCreateContext * pContext = NULL);
};

namespace
{
class CModalFrameDlg : public CWnd
{
public:
	CModalFrameDlg(CWnd * pParentWnd);
	~CModalFrameDlg();

	int DoModal();
	void EndModal(int nResult);
	int m_result;

	bool	m_ParentPlacement;
	CRuntimeClass *		m_pFrameWndClass;
	UINT				m_nIDResource;
	DWORD				m_dwDefaultStyle;
	CCreateContext *	m_pContext;
	int m_flag;
	char m_filename[1024];

	static CModalFrameDlg * FromHandle(HWND hwndFrame);

private:
	CModalFrameDlg * m_prev;
	CModalFrameDlg * m_next;
	CWnd *		m_pParentWnd;
	CFrameWnd *	m_pFrameWnd;
	WNDPROC		m_WndProc;

	static LRESULT CALLBACK FrameSubclassProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	static CModalFrameDlg m_barier;

	CFrameWnd * CreateFrame(CWnd * pParentWnd);
};
}

/////////////////////////////////////////////////////////////////////////////
#endif
