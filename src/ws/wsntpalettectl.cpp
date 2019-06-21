/************************************************************************
**
**   FILE NAME: wsntpalettectl.cpp
**
**	 Description - Functions implementation for
**		CNCLPaletteControl class 
**	 CONTAINS: 
**		all functions declared in wsntpalettectl.h
**
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntpalettectl.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:29
**
************************************************************************
*/
#include "wsntstdafx.h"
#include "wsgl.h"
#include "wsntpalettectl.h"
#include "wsntpalettewnd.h"
#include "afxpriv.h"
#include <math.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define NUM_LEVELS			7
#define TAN30				0.57735026918962F
#define YOFFSET				(1.5F * TAN30)
#define PI					3.14159265358979

static const float cfxFOOffset[] = { -0.5, -1.0, -0.5, 0.5, 1.0, 0.5 };
static const float cfyFOOffset[] = { YOFFSET, 0.0, -YOFFSET, -YOFFSET, 0.0, YOFFSET };

static const COLORREF crWhite = RGB (255, 255, 255);
static const COLORREF crBlack = RGB (0, 0, 0);

extern int UW_clr_selected;
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CNCLPaletteControl, CObject, 0)
/***********************************************************************
c
c   SUBROUTINE:  CNCLPaletteControl
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLPaletteControl::CNCLPaletteControl()
{
	m_flag = 0;
	m_hitcell = -1;
	m_rcPosition = CRect(0,0,0,0);
	m_pParent = NULL;
	m_crCurColor = RGB(0,0,0);
	ConvertRGBToHSL (m_crCurColor, &m_dblHue, &m_dblSat, &m_dblLum);
}

CNCLPaletteControl::~CNCLPaletteControl()
{
}
/////////////////////////////////////////////////////////////////////////////
/***********************************************************************
c
c   SUBROUTINE:  InitColorPalette
c
c   FUNCTION:  initial color palette
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLPaletteControl::InitColorPalette()
{
	ComputeColors();
	CRect rcPos = GetRect();
	rcPos.DeflateRect(5,5,5,5);
	AdjustCells(rcPos);
}

void CNCLPaletteControl::InvalRect(CRect rcPos)
{
	rcPos.InflateRect(2,2,2,2);
	m_pParent->InvalidateRect(rcPos);
}

void CNCLPaletteControl::Invalidate(BOOL bRedraw /* = FALSE */)
{
	m_pParent->Invalidate(bRedraw);
}

/***********************************************************************
c
c   SUBROUTINE:  SetHLS
c
c   FUNCTION:  set color palette HLS value
c
c   INPUT:  hue, luminance, Saturation: HLS value to set
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLPaletteControl::SetHLS(double hue, double luminance, double saturation)
{
	if (hue != -1)
		m_dblHue			= hue;
	
	if (saturation != -1)
		m_dblSat	= saturation;
	
	if (luminance != -1)
		m_dblLum	= luminance;
	
	COLORREF crColor = GetRGBFromHLSExtend(m_dblHue, m_dblSat, m_dblLum);
	SetRGB(crColor);
}

/***********************************************************************
c
c   SUBROUTINE:  GetHLS
c
c   FUNCTION:  get color palette HLS value
c
c   INPUT:  none
c			
c   OUTPUT: hue, luminance, Saturation: HLS value
c
c***********************************************************************
*/
void CNCLPaletteControl::GetHLS(double *hue, double *luminance, double *saturation)
{
	ConvertRGBToHSL(m_crCurColor, &m_dblHue, &m_dblSat, &m_dblLum);

	*hue		= m_dblHue;
	*luminance	= m_dblLum;
	*saturation = m_dblSat;
}

/***********************************************************************
c
c   SUBROUTINE:  SetRGB (COLORREF ref)
c
c   FUNCTION:  set color palette RGB value
c
c   INPUT:  ref: RGB value to set
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLPaletteControl::SetRGB (COLORREF ref)
{
	ConvertRGBToHSL (ref, &m_dblHue, &m_dblSat, &m_dblLum);
	CNCLColorCell* pOldSelCell = NULL;
	CNCLColorCell* pSelCell = NULL;
	BOOL bCurSel = FALSE;
	BOOL bOldSel = FALSE;
	int total_cell;
	if (m_flag==0)
		total_cell = 16;
	else
		total_cell = 48;
	for ( int i = 0 ; i < total_cell ; i++ ) 
	{
		if (m_crCells[i].GetCellColor() == m_crCurColor)
		{
			bOldSel = TRUE;
			pOldSelCell = &m_crCells[i];
			if(bOldSel && bCurSel)
			{
				break;
			}
		}
		if (m_crCells[i].GetCellColor() == ref)
		{
			bCurSel = TRUE;
			pSelCell = &m_crCells[i];
			if(bOldSel && bCurSel)
			{
				break;
			}
		}
	}	
	if(pSelCell != NULL)
	{
		m_crCurColor = pSelCell->GetCellColor();
		InvalRect(pSelCell->GetRect());
	}
	if(pOldSelCell != NULL)
	{
		InvalRect(pOldSelCell->GetRect());
	}
}

void CNCLPaletteControl::UpdateAll()
{
	InitColorPalette();
}
/***********************************************************************
c
c   SUBROUTINE:  Create(CWnd *pWnd,COLORREF crDefault,CRect &rcPos,int flag)
c
c   FUNCTION:  This function Create a CNCLPaletteControl window
c
c   INPUT:  pWnd: parent window
c			rcPos: window position
c			crDefault: default color
c			flag: window type flag
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLPaletteControl::Create(CWnd *pWnd,COLORREF crDefault,CRect &rcPos,int flag)
{
	m_flag = flag;
	m_rcPosition = rcPos;
	m_pParent = pWnd;
	m_crCurColor = crDefault;
	InitColorPalette();
}

/***********************************************************************
c
c   FUNCTION: HitTest( CPoint point )
c
c       Call this function to find out which color cell the user is click in
c
c   INPUT: 
c			point:  Specifies the x- and y-coordinate of point need to be tested. 
c
c   OUTPUT :   None
c   RETURN:    color cell. 
c
**********************************************************************/
CNCLColorCell* CNCLPaletteControl::HitTest(CPoint point)
{
	CNCLColorCell* pHitCell = NULL;
	BOOL bHit = FALSE;
	BOOL bSel = FALSE;
	int total_cell;
	if (m_flag==0)
		total_cell = 16;
	else
		total_cell = 48;
	for ( int i = 0 ; i < total_cell ; i++ ) 
	{
		if (m_crCells[i].HitTest(point))
		{
			bHit = TRUE;
			pHitCell = &m_crCells[i];
			if (m_hitcell!=i)
				m_hitcell = i;
				break;
		}
	}
	
	if(pHitCell != NULL)
	{
		if (m_flag==0)
			UW_clr_selected = 0;
		else
			UW_clr_selected = 1;
		m_crCurColor = pHitCell->GetCellColor();
		InvalRect(pHitCell->GetRect());
	}

	return pHitCell;
}
/***********************************************************************
**
**   FUNCTION: OnDraw(CDC* pDC)
**
**       calls this function to perform 
**		 color control display
**
**   INPUT:  CDC* pDC: device context
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteControl::OnDraw(CDC *pDC)
{
	CRect rectClient;
	rectClient = m_rcPosition;
	
	pDC->FillSolidRect(rectClient, gfxData.m_crBtnFace);
	
	CNCLColorCell* pSelCell = NULL;
	
	int total_cell;
	if (m_flag==0)
		total_cell = 16;
	else
		total_cell = 48;
	for (int i = 0; i < total_cell; i++)
	{
		m_crCells[i].OnDraw(pDC);
	}
	CWnd *win;
	if (m_hitcell>=0)	
		pSelCell = &m_crCells[m_hitcell];
	if (pSelCell != NULL)
	{
		if (((CNCLPaletteWnd*)m_pParent)->m_focus==0)
			pSelCell->OnDrawSelect (pDC, 0);
		else
			pSelCell->OnDrawSelect (pDC, 1);
		if (m_flag==0)
			UW_clr_selected = 0;
		else
			UW_clr_selected = 1;
	}
}
/***********************************************************************
**
**   FUNCTION: ComputeColors
**
**       set all color cell colors
**
**   INPUT:  none
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteControl::ComputeColors()
{
	CRect rectClient;
	rectClient = m_rcPosition;
	rectClient.bottom = rectClient.bottom - 30;
	
	if (rectClient.Height () < rectClient.Width ())
	{
		rectClient.DeflateRect ((rectClient.Width () - rectClient.Height ()) / 2, 0);
	}
	else
	{
		rectClient.DeflateRect (0, (rectClient.Height () - rectClient.Width ()) / 2);
	}
	
	ASSERT (abs (rectClient.Height () - rectClient.Width ()) <= 1);
	

	if (m_flag==0)
	{	
		for (int i = 0; i < 16; i++)
		{
			COLORREF color = RGB(uw_color_table[i][0], 
								uw_color_table[i][1], uw_color_table[i][2]);
			m_crCells[i].SetCellColor(color);
		}
	}
	else
	{	
		for (int i = 16; i < 64; i++)
		{
			COLORREF color = RGB(uw_color_table[i][0], 
								uw_color_table[i][1], uw_color_table[i][2]);
			m_crCells[i-16].SetCellColor(color);
		}
	}
}

/***********************************************************************
**
**   FUNCTION: AdjustCells(CRect rcPos)
**
**       adjust all color cell position
**
**   INPUT:  rcPos: 
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteControl::AdjustCells(CRect rcPos)
{
	CRect rcTotal = rcPos;
	
	double MaxPixWide = double(rcTotal.Width());
	double maxPixHigh = double(rcTotal.Height());
	
	double dX;
	int cols, rows, total_cell;
	dX = 0;
	
	if (m_flag==0)
	{
		cols = 8;
		rows = 2;
		total_cell = 16;
	}
	else
	{
		cols = 8;
		rows = 6;
		total_cell = 48;
	}
	int *widthArray = new int[cols+1];
	{
		double dlta = (double)(1/(double)cols);
		for ( int i = 0 ; i < cols+1 ; i++ ) 
		{
			int x = rcTotal.left + int(double((MaxPixWide)*dlta*i));
			widthArray[i] = x;
		}
	}
	int *heightArray = new int[rows+1];
	{
		double dlta = double(1/(double)rows);
		for ( int i = 0 ; i < rows+1 ; i++ ) 
		{
			heightArray[i] = rcTotal.top + int(double((maxPixHigh)*dlta*i));
		}
	}
	
	CRect rcTemp;
	CRect rcOther;
	int x, y;
	for (int i = 0 ; i < total_cell ; i++ )
	{
		x = i % cols;
		y = i / cols ;
		rcTemp =  CRect(widthArray[x],heightArray[y],widthArray[x+1],heightArray[y+1]);
		m_crCells[i].SetRect(&rcTemp);
	}
	delete []heightArray;
	delete []widthArray;
}

/***********************************************************************
**
**   FUNCTION: OnLButtonUp(UINT nFlags, CPoint pt)
**
**       Left mouse button up callback
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteControl::OnLButtonUp(UINT nFlags, CPoint point) 
{
	int total_cell;
	if (m_flag==0)
		total_cell = 16;
	else
		total_cell = 48;
	for ( int i = 0 ; i < total_cell ; i++ ) 
	{
		if ( m_crCells[i].HitTest(point) )
		{
			OnSelectOK((WPARAM)(m_crCells[i].GetCellColor()),0);
			return;
		}
	}
}

/***********************************************************************
**
**   FUNCTION: OnSelectOK(WPARAM wParam, LPARAM lParam)
**
**       selecting color cell callback
**
**   INPUT:  wParam:
**			lParam:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteControl::OnSelectOK(WPARAM wParam, LPARAM lParam)
{
	m_pParent->PostMessage( WM_NCL_SELECTCOLOROK, wParam, lParam );
	if (m_flag==0)
		UW_clr_selected = 0; 
	else
		UW_clr_selected = 1; 
}

/***********************************************************************
**
**   FUNCTION: OnLButtonDown(UINT nFlags, CPoint pt)
**
**       Left mouse button down callback
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteControl::OnLButtonDown(UINT nFlags, CPoint point) 
{	
	CNCLColorCell* pSel = HitTest(point);
	if(pSel != NULL)
	{
		OnSelectOK((WPARAM)(pSel->GetCellColor()),0);
	}
}

/***********************************************************************
**
**   FUNCTION: OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
**
**       key down callback
**
**   INPUT:  nFlags:
**				nChar:
**				nRepCnt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteControl::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	nFlags;
	nRepCnt;
	int ks = GetKeyState( VK_CONTROL );
	ks >>= ((sizeof(int)*8)-1);

	int nCurSel  = 0;
	int total_cell;
	if (m_flag==0)
		total_cell = 16;
	else
		total_cell = 48;
	for ( int i = 0 ; i < total_cell ; i++ ) 
	{
		if (m_crCells[i].GetCellColor() == m_crCurColor)
		{
			nCurSel = i;
			break;
		}
	}
	int nNewCell = nCurSel;
	switch(nChar) 
	{
	case VK_LEFT:
		{
			if(nCurSel == 0) 
			{
				nNewCell = total_cell-1;
			}
			else if(nCurSel >= 0)
			{
				nNewCell --;
			}

			SetRGB(m_crCells[nNewCell].GetCellColor());
			m_pParent->PostMessage( WM_NCL_SELECTCOLOROK, (WPARAM)m_crCells[nNewCell].GetCellColor(), 0 );
		}
		break;
		
	case VK_RIGHT:
		{
			if(nCurSel == (total_cell-1)) 
			{
				nNewCell = 0;
			}
			else if(nCurSel < (total_cell-1))
			{
				nNewCell ++;
			}

			SetRGB(m_crCells[nNewCell].GetCellColor());
			m_pParent->PostMessage( WM_NCL_SELECTCOLOROK, (WPARAM)m_crCells[nNewCell].GetCellColor(), 0 );
		}
		break;
		
	case VK_DOWN:
		{
			if(nCurSel == (total_cell-1)) 
			{
				nNewCell = 0;
			}
			else if(nCurSel < (total_cell-1))
			{
				nNewCell ++;
			}

			SetRGB(m_crCells[nNewCell].GetCellColor());
			m_pParent->PostMessage( WM_NCL_SELECTCOLOROK, (WPARAM)m_crCells[nNewCell].GetCellColor(), 0 );
		}
		break;
		
	case VK_UP:
		{
			if(nCurSel == 0) 
			{
				nNewCell = total_cell-1;
			}
			else if(nCurSel >= 0)
			{
				nNewCell --;
			}

			SetRGB(m_crCells[nNewCell].GetCellColor());
			m_pParent->PostMessage( WM_NCL_SELECTCOLOROK, (WPARAM)m_crCells[nNewCell].GetCellColor(), 0 );
		}
		break;

	case VK_RETURN:
		{
		}
		break;

	case VK_CANCEL:
	case VK_ESCAPE:
		{
		}
		break;

	case VK_TAB:
		{			
			CWnd* pParent = m_pParent;
			if( pParent != NULL )
			{
				CWnd* pPrevFocus = pParent->GetParent()->GetNextDlgTabItem(m_pParent, TRUE);
				if(pPrevFocus != NULL)
					pPrevFocus->SetFocus();
			}
			
		}
		return;
	}
}
/***********************************************************************
**
**   FUNCTION: GetRGBFromHue(float rm1, float rm2, float rh) 
**
**       get the RGB coor fom the hue value
**
**   INPUT:  rm1m rm2,rh:
**
**   OUTPUT :   None
**   RETURN:    RGB color
**
**********************************************************************/
BYTE CNCLPaletteControl::GetRGBFromHue(float rm1, float rm2, float rh)
{
	if (rh > 360.0f)
		rh -= 360.0f;
	else if (rh < 0.0f)
		rh += 360.0f;
	
	if (rh <  60.0f)
		rm1 = rm1 + (rm2 - rm1) * rh / 60.0f;   
	else if (rh < 180.0f)
		rm1 = rm2;
	else if (rh < 240.0f)
		rm1 = rm1 + (rm2 - rm1) * (240.0f - rh) / 60.0f;      
	
	return static_cast<BYTE>(rm1 * 255);
}

/***********************************************************************
**
**   FUNCTION: GetRGBFromHLS( double H, double L, double S )
**
**       get the RGB coor fom the HLS value
**
**   INPUT:  H L,S:
**
**   OUTPUT :   None
**   RETURN:    RGB color
**
**********************************************************************/
COLORREF CNCLPaletteControl::GetRGBFromHLS( double H, double L, double S )
{
	double r, g, b;
	double m1, m2;
	
	if(S==0) 
	{
		r=g=b=L;
	} 
	else 
	{
		if(L <=0.5)
			m2 = L*(1.0+S);
		else
			m2 = L+S-L*S;
		m1 = 2.0*L-m2;
		r = GetRGBFromHue((float)m1, (float)m2, (float)(H+1.0/3.0));
		g = GetRGBFromHue((float)m1, (float)m2, (float)H);
		b = GetRGBFromHue((float)m1, (float)m2, (float)(H-1.0/3.0));
	}
	return RGB((BYTE)(r*255), (BYTE)(g*255), (BYTE)(b*255));
}
/***********************************************************************
**
**   FUNCTION: GetRGBFromHLSExtend( double H, double L, double S )
**
**       get the RGB coor fom the HLS value
**
**   INPUT:  H L,S:
**
**   OUTPUT :   None
**   RETURN:    RGB color
**
**********************************************************************/
COLORREF CNCLPaletteControl::GetRGBFromHLSExtend( double H, double L, double S )
{
	WORD R, G, B;
	
	if (S == 0.0)
	{
		R = G = B = unsigned char(L * 255.0);
	}
	else
	{
		float rm1, rm2;
		
		if (L <= 0.5f)
			rm2 = (float)(L + L * S);
		else
			rm2 = (float)(L + S - L * S);
		
		rm1 = (float)(2.0f * L - rm2);
		
		R = GetRGBFromHue(rm1, rm2, (float)(H + 120.0f));
		G = GetRGBFromHue(rm1, rm2, (float)(H));
		B = GetRGBFromHue(rm1, rm2, (float)(H - 120.0f));
	}
	
	return RGB(R, G, B);
}

/***********************************************************************
**
**   FUNCTION: ConvertRGBToHSL( COLORREF rgb, double *H, double *S, double *L )
**
**       convert the RGB coor to the HLS value
**
**   INPUT:  rgb: RGB color
**
**   OUTPUT :   H, L, S: HLS color
**   RETURN:    none
**
**********************************************************************/
void CNCLPaletteControl::ConvertRGBToHSL( COLORREF rgb, double *H, double *S, double *L )
{   
	double delta;
	double r = (double)GetRValue(rgb)/255;
	double g = (double)GetGValue(rgb)/255;
	double b = (double)GetBValue(rgb)/255;   
	double cmax = max(r, max(g, b));
	double cmin = min(r, min(g, b));   
	*L=(cmax+cmin)/2.0;   
	
	if(cmax==cmin) 
	{
		*S = 0;      
		*H = 0; // it's really undefined   
	} 
	else 
	{
		if(*L < 0.5) 
			*S = (cmax-cmin)/(cmax+cmin);      
		else
			*S = (cmax-cmin)/(2.0-cmax-cmin);      
		
		delta = cmax - cmin;
		if(r==cmax) 
			*H = (g-b)/delta;      
		else if(g==cmax)
			*H = 2.0 +(b-r)/delta;
		else          
			*H=4.0+(r-g)/delta;
		*H /= 6.0; 
		if(*H < 0.0)
			*H += 1;  
	}
}
/***********************************************************************
**
**   FUNCTION: Deselect
**
**       de-select the color cell 
**
**   INPUT:  none
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLPaletteControl::Deselect()
{
	m_hitcell = -1;
	m_pParent->Invalidate(TRUE);
}

/***********************************************************************
**
**   FUNCTION: SetSelCellColor(COLORREF color)
**
**       set the select color cell color
**
**   INPUT:  color: color to be set
**   OUTPUT :   none
**   RETURN:    none
**
**********************************************************************/
void CNCLPaletteControl::SetSelCellColor(COLORREF color)
{
	m_crCells[m_hitcell].SetCellColor(color);
}
