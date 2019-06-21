/************************************************************************
**
**   FILE NAME: wsntdrawbutton.cpp
**
**	 Description - Functions implementation for
**		CNCLDrawButton class 
**	 CONTAINS: 
**		all functions declared in wsntdrawbutton.h
**
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdrawbutton.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:22
**
************************************************************************
*/
#include "wsntstdafx.h"
#include "wsntdrawbutton.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

// CNCLDrawButton
IMPLEMENT_DYNAMIC(CNCLDrawButton, CButton)

/***********************************************************************
c
c   SUBROUTINE:  CNCLDrawButton
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLDrawButton::CNCLDrawButton() 
{  
  m_crColor = RGB(255,255,255);
  m_crOldColor = RGB(0,0,0);
  m_colordef = 0;
  m_colordef_old = 0;
} 

CNCLDrawButton::~CNCLDrawButton()
{
} 

BEGIN_MESSAGE_MAP(CNCLDrawButton, CButton)
	//{{AFX_MSG_MAP(CNCLDrawButton)
	ON_WM_GETDLGCODE()
	ON_WM_SYSCOLORCHANGE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
**
**   SUBROUTINE: Attach(const UINT nID, CWnd* pParent, const COLORREF BGColor, const COLORREF FGColor)
**
**   FUNCTION:  Call this function to attach a window field to CNCLDrawButton. 
**
**   INPUT:  
**			UINT nID: button ID to be attached
**			CWnd* pParent: paent window
**			COLORREF BGColor: button background color	
**			COLORREF FGColor: button foreground color	
**
**   OUTPUT: none
**	 RETURN: If the function succeeds, the return value is true
**			If the function fails, the return value is FALSE
**
***********************************************************************/
BOOL CNCLDrawButton::Attach(const UINT nID, CWnd* pParent, const COLORREF BGColor, const COLORREF FGColor)
{
	if (!SubclassDlgItem(nID, pParent))
		return FALSE;

	m_crOldColor = FGColor;
	m_crColor = BGColor; 
	return TRUE;
} 

/***********************************************************************
**
**   SUBROUTINE: SetButtondef(int flag)
**
**   FUNCTION:  set the button as default colored flag. 
**
**   INPUT:  
**			int flag: flag to be set
**				1: set the button as default colored
**
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLDrawButton::SetButtondef(int flag)
{
	m_colordef = flag;
	Invalidate();
}
/***********************************************************************
**
**   SUBROUTINE: SetButtonColor(COLORREF crColor)
**
**   FUNCTION:  set the button current colore. 
**
**   INPUT:  
**			COLORREF crColor: color to be set
**
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLDrawButton::SetButtonColor(COLORREF crColor)
{
	m_colordef = 0;
	m_crColor = crColor;
	Invalidate();
}
/***********************************************************************
**
**   SUBROUTINE: DrawItem(LPDRAWITEMSTRUCT lpDIS)
**
**   FUNCTION:  draw the button. 
**
**   INPUT:  
**			lpDIS: button information
**
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLDrawButton::DrawItem(LPDRAWITEMSTRUCT lpDIS)
{
	CDC* pDC = CDC::FromHandle(lpDIS->hDC);
	CRect rectClient;
	GetClientRect(rectClient);
	CDC dc;
	dc.CreateCompatibleDC(pDC);
	CBitmap bmpMem;
	bmpMem.CreateCompatibleBitmap(pDC, rectClient.Width(), rectClient.Height());
	CBitmap* pBmpOld = dc.SelectObject(&bmpMem);

	dc.FillSolidRect(rectClient,::GetSysColor(COLOR_BTNFACE));

	UINT state = lpDIS->itemState; 
	CRect btnRect;
	btnRect.CopyRect(&lpDIS->rcItem); 
	      
	DrawFilledRect(&dc, btnRect, m_crColor); 
    DrawFrame(&dc, btnRect);
	pDC->BitBlt(rectClient.left, rectClient.top,	rectClient.Width(), rectClient.Height(), &dc, 0, 0, SRCCOPY);	
	dc.SelectObject(pBmpOld);
} 
/***********************************************************************
**
**   SUBROUTINE: DrawFrame(CDC *DC, CRect rect)
**
**   FUNCTION:  draw the button frame. 
**
**   INPUT:  
**			CDC *DC: 
**			CRect rect
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLDrawButton::DrawFrame(CDC *DC, CRect rect)
{ 
	DC->Draw3dRect(rect,RGB(255,255,255),RGB(0,0,0));
	rect.DeflateRect(1,1,1,1);
	DC->Draw3dRect(rect,gfxData.m_crBtnFace,gfxData.m_crBtnShadow);
}
/***********************************************************************
**
**   SUBROUTINE: DrawFilledRect(CDC *DC, CRect rect, COLORREF color)
**
**   FUNCTION:  draw the button inside using curent color. 
**
**   INPUT:  
**			CDC *DC: 
**			CRect rect
**			COLORREF color: current color
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLDrawButton::DrawFilledRect(CDC *DC, CRect rect, COLORREF color)
{ 
	rect.DeflateRect(4,4,4,4);

	CPoint ptCenter = rect.CenterPoint();
	CRect rc;
	rc = rect;
	rc.bottom = ptCenter.y;

	if (m_colordef)
		DrawFilledDef(DC, rc);
	else
	{
		CBrush B;
		B.CreateSolidBrush(color);
		DC->FillRect(rc, &B);
	}
	rc = rect;
	rc.top = ptCenter.y;
	if (m_colordef_old)
		DrawFilledDef(DC, rc);
	else
	{
		CBrush B1;
		B1.CreateSolidBrush(m_crOldColor);
		DC->FillRect(rc,&B1);
	}
}
/***********************************************************************
**
**   SUBROUTINE: DrawFilledDef(CDC *DC, CRect rect)
**
**   FUNCTION:  draw the button inside as default colored. 
**
**   INPUT:  
**			CDC *DC: 
**			CRect rect
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLDrawButton::DrawFilledDef(CDC *DC, CRect rect)
{ 
	CRect rc;
	COLORREF one = RGB(255,255,255);
	COLORREF two = RGB(150,150,150);
	CBrush B1;
	CBrush B2;
	B1.CreateSolidBrush(one);
	B2.CreateSolidBrush(two);

	int wid = (int)(rect.Width()/10.0+0.5);
	int hgt = rect.Height()/3;

	rc = rect;

	int i;
	for (i = 0; i<5; i++)
	{
		rc.left = rect.left + 2*i*wid;
		rc.right = rc.left+wid;
		rc.bottom = rect.top + hgt;
		DC->FillRect(rc, &B1);

		rc.left = rc.right;
		rc.right = rc.left+wid;
		DC->FillRect(rc, &B2);
	}
/*
......second row
*/
	rc = rect;
	for (i = 0; i<5; i++)
	{
		rc.left = rect.left + 2*i*wid;
		rc.right = rc.left+wid;
		rc.top = rect.top + hgt;
		rc.bottom = rc.top + hgt;
		DC->FillRect(rc, &B2);
		rc.left = rc.right;
		rc.right = rc.left+wid;
		DC->FillRect(rc, &B1);
	}
	rc = rect;
	for (i = 0; i<5; i++)
	{
		rc.left = rect.left + 2*i*wid;
		rc.right = rc.left+wid;
		rc.top = rect.top + 2*hgt;
		DC->FillRect(rc, &B1);
		rc.left = rc.right;
		rc.right = rc.left+wid;
		DC->FillRect(rc, &B2);
	}
}

UINT CNCLDrawButton::OnGetDlgCode() 
{
	return DLGC_WANTARROWS;
}

/***********************************************************************
**
**   SUBROUTINE: OnSysColorChange
**
**   FUNCTION:  called when system color changed
**
**   INPUT:  
**			none
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLDrawButton::OnSysColorChange() 
{
	CButton::OnSysColorChange();

	Invalidate ();
	UpdateWindow ();
}
