/************************************************************************
**
**   FILE NAME: wsntlumchooser.h
**
**       Description - Functions and struct declarations for
**              CNCLLumChooser class
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntlumchooser.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:19
**********************************************************************
*/
#ifndef NCLLUMCHOOSER_H
#define NCLLUMCHOOSER_H

#include <memory>
#include "wsntctlsetting.h"
#pragma once

/////////////////////////////////////////////////////////////////////////////
// CNCLLumChooser window

using namespace Gdiplus;
using namespace ColorPickCtrlSetting;
using namespace std;

class CNCLLumChooser : public CWnd
{
public:
	enum {NM_COLORCHANGE};
	CNCLLumChooser();
	virtual ~CNCLLumChooser();

	BOOL	Create (DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext = NULL ); 
	void	SetColor(double hue, double lum, double sat);
	void	SetColor(CHls& hls);
	void	SetLum(double v);
	void	SetHueSat(double hue, double sat);
	CHls&	GetColor();

private:
	BOOL		m_bCapture;
	CHls		m_color;
	auto_ptr<Bitmap>		m_spOffScreen;
	auto_ptr<Bitmap>		m_spTriangle;
	int			m_iColorAreaWidth;
	Rect		m_size;

	void	ResetBitmap();
	void	DrawColor();
	void	ClearTrangle(Graphics *pg);
	void	ResetTrangleAndLum(CPoint &mouse);
	void	NotifyColorChanged();

	int		TrangleLeftLimit();
	int		TrangleRightLimit();
	int		GetTrangleYPos();
	int		GetColorAreaHeight();
	
	void	FirstTimePaint();
	void	Paint();
	void	(CNCLLumChooser::*InternalPaint)();

protected:
	//{{AFX_MSG(CNCLLumChooser)
	afx_msg void OnPaint();
	afx_msg void OnDestroy();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnKillFocus(CWnd* pNewWnd);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

inline void CNCLLumChooser::SetHueSat(double hue, double sat)
{
	if(m_color.GetHue() == hue && m_color.GetSaturation() == sat)
		return;
	m_color.SetHue(hue);
	m_color.SetSaturation(sat);
	if(m_spOffScreen.get()) 
		DrawColor();
	if(m_hWnd != NULL) 
		Invalidate();
}

inline void CNCLLumChooser::SetLum(double v)
{
	if(m_color.GetLuminance() == v)
		return;
	
	if(m_spOffScreen.get()) 
	{
		Graphics g(m_spOffScreen.get());
		ClearTrangle(&g);
		m_color.SetLuminance(v);
		g.DrawImage(m_spTriangle.get(), TrangleLeftLimit(), GetTrangleYPos());
	}
	else
		m_color.SetLuminance(v);
	if(m_hWnd != NULL) 
		Invalidate();
}

inline void CNCLLumChooser::SetColor(double hue, double lum, double sat)
{
	SetRedraw(FALSE);
	SetHueSat(hue, sat);
	SetLum(lum);
	SetRedraw(TRUE);
	if(m_hWnd != NULL)
		Invalidate();
}
inline void CNCLLumChooser::SetColor(CHls& hls)
{
	SetColor(hls.GetHue(), hls.GetLuminance(), hls.GetSaturation());
}

inline CHls& CNCLLumChooser::GetColor()
{
	return m_color;
}
#endif
