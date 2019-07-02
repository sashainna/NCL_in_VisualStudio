/************************************************************************
**
**   FILE NAME: wsntpalettectl.h
**
**       Description - Functions and struct declarations for
**              CNCLPaletteControl class
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntpalettectl.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:20
**********************************************************************
*/
#if !defined(NCLPALETTECTL_H)
#define NCLPALETTECTL_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "wsntclrcell.h"

class CNCLPaletteControl : public CObject
{
protected:
	DECLARE_SERIAL(CNCLPaletteControl);
public:
	CNCLPaletteControl();
	virtual ~CNCLPaletteControl();
	virtual void Create(CWnd *pWnd,COLORREF crDefault,CRect &rcPos,int flag = 0);
	virtual void InitColorPalette();
	virtual void AdjustCells(CRect rcPos);
	virtual void ComputeColors();
	virtual void OnDraw(CDC *pDC);
	virtual void UpdateAll();
	COLORREF	GetRGB () const				
	{	
		return m_crCurColor; 
	}
	void		SetRGB(COLORREF ref);
	void		SetHLS (double hue,double luminance, double saturation);
	void		GetHLS (double *hue,double *luminance, double *saturation);
	virtual CNCLColorCell* HitTest(CPoint point);
	virtual void OnSelectOK(WPARAM wParam, LPARAM lParam);
	void Deselect();

protected:
	CNCLColorCell m_crCells[64];
	int m_hitcell;

public:	
	void SetSelCellColor(COLORREF color);
	void Setselcell(int cell) { m_hitcell = cell; }
	int Getselcell() { return m_hitcell; }
	void SetWnd(CWnd *pWnd)					
	{ 
		m_pParent = pWnd; 
	}
	void Invalidate(BOOL bRedraw = FALSE);
	void InvalRect(CRect rcPos);
	void OnLButtonDown(UINT nFlags, CPoint point); 
	void OnLButtonUp(UINT nFlags, CPoint point);
	void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	CRect GetRect() const						
	{ return m_rcPosition; }
	void  SetRect(const CRect rc)				
	{ m_rcPosition = rc; }
 
	BYTE GetRGBFromHue(float rm1, float rm2, float rh);
	COLORREF GetRGBFromHLS( double H, double L, double S );
	COLORREF GetRGBFromHLSExtend( double H, double L, double S );
	void ConvertRGBToHSL( COLORREF rgb, double *H, double *S, double *L );
protected:
	CRect				m_rcPosition;
	COLORREF			m_crCurColor;
	CWnd*				m_pParent;
	double				m_dblLum;
	double				m_dblSat;
	double				m_dblHue;
	int m_flag;
};

#endif // !defined(NCLPALETTECTL_H)
