/************************************************************************
**
**   FILE NAME: wsntclrcell.cpp
**
**	 Description - Functions implementation for
**		CNCLColorCell class 
**	 CONTAINS: 
**		all functions declared in wsntclrcell.h
**
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntclrcell.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:20
**
************************************************************************
*/
#include "wsntstdafx.h"
#include "wsntclrcell.h"
#include "afxpriv.h"
#include "math.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define FORMMIN(X,Y)				(((X) < (Y)) ? (X) : (Y))
#define FORMMAX(X,Y)				(((X) > (Y)) ? (X) : (Y))

NCL_CLR_GLOBAL gfxData;

NCL_CLR_GLOBAL::NCL_CLR_GLOBAL()
{
	OnSysColorChange();	
}

NCL_CLR_GLOBAL::~NCL_CLR_GLOBAL()
{
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
void NCL_CLR_GLOBAL::OnSysColorChange()
{
	m_crBtnFace = GetSysColor(COLOR_BTNFACE);
	m_crBtnShadow = GetSysColor(COLOR_BTNSHADOW);
	m_crBtnDkShadow = GetSysColor(COLOR_3DDKSHADOW);
	m_crBtnLight = GetSysColor(COLOR_3DLIGHT);
	m_crBtnHilite = GetSysColor(COLOR_BTNHIGHLIGHT);
	m_crBtnText = GetSysColor(COLOR_BTNTEXT);
	m_crTextGrayed = GetSysColor (COLOR_GRAYTEXT);
	m_crWindowFrame = GetSysColor(COLOR_WINDOWFRAME);
	
	m_crHilite = GetSysColor(COLOR_HIGHLIGHT);
	m_crTextHilite = GetSysColor(COLOR_HIGHLIGHTTEXT);
	m_crWindowText = GetSysColor(COLOR_WINDOWTEXT);
	m_crBtn3dShadow = GetSysColor(COLOR_3DSHADOW);
	m_crBtn3dHiLight = GetSysColor(COLOR_3DHILIGHT);
	m_crAcCaption = GetSysColor(COLOR_ACTIVECAPTION);
	m_crInAcCaption = GetSysColor(COLOR_INACTIVECAPTION);
	m_crInAcCaptionText = GetSysColor(COLOR_INACTIVECAPTIONTEXT);
	m_crDesktop = GetSysColor(COLOR_DESKTOP);
	
	m_cr3dFace = gfxData.m_cr3dFace;

	OSVERSIONINFO osvi;
    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	
	::GetVersionEx (&osvi);
	if (osvi.dwMajorVersion >= 5)
	{
		m_crTextHot = GetSysColor (COLOR_HOTLIGHT);
	}
	
	CWindowDC dc (NULL);
	m_nBitsPerPixel = dc.GetDeviceCaps (BITSPIXEL);
	
}
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CNCLColorCell, CObject, 0)
/***********************************************************************
c
c   SUBROUTINE:  CNCLColorCell
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLColorCell::CNCLColorCell()
{
	m_rcPosition = CRect(0,0,0,0);
	m_crCell = RGB(255,255,255);
	m_pBrushCell = NULL;
}

CNCLColorCell::~CNCLColorCell()
{
	RelaseBrushObject();
}

/***********************************************************************
c
c   SUBROUTINE:  SetCellColor(const COLORREF &cr) 
c
c   FUNCTION:  Set color cell color
c
c   INPUT:  cr: color to be set
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLColorCell::SetCellColor(const COLORREF &cr) 
{ 
	RelaseBrushObject();
	m_crCell = cr; 
}

/***********************************************************************
c
c   SUBROUTINE:  CreateBrush(CDC* pDC)
c
c   FUNCTION:  create brush for this color cell
c
c   INPUT:  pDC: 
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CBrush* CNCLColorCell::CreateBrush(CDC* pDC)
{
	pDC;
	CBrush* pBrush = new CBrush();
	pBrush->CreateSolidBrush(m_crCell);
	return pBrush;
}

/***********************************************************************
c
c   SUBROUTINE:  GetBrush(CDC* pDC)
c
c   FUNCTION:  get brush of this color cell
c
c   INPUT:  pDC: 
c			
c   OUTPUT: none
c
c	RETURN: brush
c
c***********************************************************************
*/
CBrush* CNCLColorCell::GetBrush(CDC* pDC)
{
	if (m_pBrushCell == NULL)
		m_pBrushCell = CreateBrush(pDC);
	return m_pBrushCell;
}

/***********************************************************************
c
c   SUBROUTINE:  RelaseBrushObject
c
c   FUNCTION:  release brush of this color cell
c
c   INPUT:  pDC: 
c			
c   OUTPUT: none
c
c	RETURN: brush
c
c***********************************************************************
*/
void CNCLColorCell::RelaseBrushObject()
{
	if(m_pBrushCell != NULL)
	{
		delete m_pBrushCell;
	}
	m_pBrushCell = NULL;
}

void CNCLColorCell::PrepareDC(CDC* pDC)
{
	pDC->SaveDC();
	pDC->SetBkMode(TRANSPARENT);
	CBrush* pBrush = GetBrush(pDC);
	pDC->SelectObject(pBrush);
	pDC->SelectStockObject(NULL_PEN);
}
/***********************************************************************
**
**   FUNCTION: OnDraw(CDC* pDC)
**
**       The framework calls this function to perform 
**		 drawinf of the window
**
**   INPUT:  CDC* pDC: device context
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLColorCell::OnDraw(CDC *pDC)
{
	PrepareDC(pDC);
	ASSERT_VALID (pDC);
	pDC->FillSolidRect(m_rcPosition,gfxData.m_crBtnFace);
	CRect rcPos;
	rcPos = m_rcPosition;
	rcPos.DeflateRect(2,2,2,2);
	
	pDC->Rectangle(rcPos);
	pDC->Draw3dRect(rcPos,gfxData.m_crBtn3dShadow,gfxData.m_crBtn3dHiLight);

	rcPos.DeflateRect(1,1,1,1);
	pDC->Draw3dRect(rcPos,gfxData.m_crBtn3dShadow,gfxData.m_crBtn3dHiLight);

	ClearDC(pDC);
}

/***********************************************************************
**
**   FUNCTION: OnDrawSelect(CDC *pDC, int focus)
**
**       drawing the cell as select
**
**   INPUT:  CDC* pDC: device context
**				focus: if the cell have the focus
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLColorCell::OnDrawSelect(CDC *pDC, int focus)
{
	CBrush* pBrBlack = CBrush::FromHandle ((HBRUSH) ::GetStockObject (BLACK_BRUSH));
	ASSERT_VALID (pBrBlack);

	CRgn rgn;
		
	CRect rcPos;
	rcPos = m_rcPosition;
	rcPos.DeflateRect(1,1,1,1);
	rgn.CreateRectRgnIndirect(&rcPos);
	pDC->FrameRgn(&rgn, pBrBlack, 1, 1);

	if (focus==0)
		return;
	CRect focusRect;
	focusRect = m_rcPosition;
	focusRect.InflateRect(1,1,1,1);
	pDC->DrawFocusRect(&focusRect);
}

void CNCLColorCell::ClearDC(CDC* pDC)
{
	pDC->RestoreDC(-1);
}

/***********************************************************************
c
c   FUNCTION: HitTest( CPoint point )
c
c       Call this function to find out if the point is within color cell
c
c   INPUT: 
c			point:  Specifies the x- and y-coordinate of point need to be tested. 
c
c   OUTPUT :   None
c   RETURN:    true: within color cell
c
**********************************************************************/
BOOL CNCLColorCell::HitTest(CPoint point)
{
	CRgn rgn;
	rgn.CreateRectRgn(m_rcPosition.left,m_rcPosition.top,m_rcPosition.right,m_rcPosition.bottom);
	return rgn.PtInRegion (point);
}

