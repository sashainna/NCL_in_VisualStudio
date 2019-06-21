/************************************************************************
**
**   FILE NAME: wsnthuesatchooser.h
**
**       Description - Functions and struct declarations for
**              CNCLHueSatChooser class
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnthuesatchooser.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:19
**********************************************************************
*/
#ifndef NCLHUESATCHOOSER_H_
#define NCLHUESATCHOOSER_H_

#include <memory>
#include "wsntctlsetting.h"

#pragma once

using namespace Gdiplus;
using namespace ColorPickCtrlSetting;
using namespace std;

class CNCLHueSatChooser : public CWnd
{
private:

public:
	enum {NM_COLORCHANGE};

	CNCLHueSatChooser();
	virtual ~CNCLHueSatChooser();

	BOOL	Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext=NULL);
	double	GetHue() {return m_color.GetHue();}
	double	GetSat() {return m_color.GetSaturation();}
	void	SetColor(CHls &hls);

private:
	BOOL				m_bCapture;
	auto_ptr<Bitmap>	m_spOffScreen;
	auto_ptr<Bitmap>	m_spSaved;
	CHls				m_color;
	Rect				m_size;
	Rect				m_colorArea;
	
	void ResetBitmap();
	void DrawEdge();
	void DrawColor();
	void NotifyColorChanged();
	void UpdateSize();
	
	void FirstTimePaint();
	void Paint();
	void (CNCLHueSatChooser::*InternalPaint)();
protected:
	
	//{{AFX_MSG(CNCLHueSatChooser)
	afx_msg void OnDestroy();
	afx_msg void OnPaint();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnKillFocus(CWnd* pNewWnd);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	//{{AFX_VIRTUAL(CNCLHueSatChooser)
	//}}AFX_VIRTUAL
};

inline void CNCLHueSatChooser::SetColor(CHls &hls)
{
	m_color = hls;
	if(m_hWnd != NULL) 
		Invalidate();
}
#endif
