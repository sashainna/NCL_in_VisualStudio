/************************************************************************
**
**   FILE NAME: wsntclrcell.h
**
**       Description - Functions and struct declarations for
**              CNCLColorCell class
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntclrcell.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:15
**********************************************************************
*/
#if !defined(NCLCOLORCELL_H)
#define NCLCOLORCELL_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// Color change message.
#define WM_NCL_SELECTCOLOROK					WM_USER+220	// Select color ok.

struct NCL_CLR_GLOBAL
{
	NCL_CLR_GLOBAL();
	~NCL_CLR_GLOBAL();

	COLORREF m_crBtnFace;
	COLORREF m_crBtnHilite;
	COLORREF m_crBtnShadow;
	COLORREF m_crBtnText;
	COLORREF m_crBtnDkShadow;
	COLORREF m_crBtn3dShadow;
	COLORREF m_crBtn3dHiLight;
	COLORREF m_crBtnLight;
	COLORREF m_cr3dFace;
	COLORREF m_crAcCaption;
	COLORREF m_crInAcCaption;
	COLORREF m_crInAcCaptionText;
	COLORREF m_crDesktop;

	COLORREF m_crWindowFrame;
	COLORREF m_crHilite;
	COLORREF m_crWindowText;
	COLORREF m_crTextGrayed;
	COLORREF m_crTextHilite;
	COLORREF m_crTextHot;
	int m_cxEdge;
	int m_cyEdge;
	int m_cxBorder;
	int m_cxScreen;
	int m_cyScreen;
	int m_cxVScroll;
	int m_cxFrame;
	int m_cyFrame;
	int m_cxFixedFrame;
	int	m_nBitsPerPixel;
	void OnSysColorChange();
};

extern NCL_CLR_GLOBAL gfxData;
class CNCLColorCell : public CObject
{
protected:
	DECLARE_SERIAL(CNCLColorCell);
public:
	CNCLColorCell();
	virtual ~CNCLColorCell();
	virtual BOOL HitTest(CPoint point);

	// Creates a GDI brush object. The caller is responsible for freeing this memory! 
	CBrush* CreateBrush(CDC* pDC = NULL);
	CBrush* GetBrush(CDC* pDC = NULL);
	void RelaseBrushObject();
	virtual void PrepareDC(CDC* pDC);
	virtual void OnDraw(CDC *pDC);
	virtual void OnDrawSelect(CDC *pDC, int focus);

	// Frees GDI objects and restores the state of the device context.
	virtual void ClearDC(CDC* pDC);
	void	SetRect(const CRect &rcPos)			{ m_rcPosition = rcPos; }
	CRect	GetRect() const
	{
		return m_rcPosition;
	};
	void	SetCellColor(const COLORREF &cr);
	COLORREF GetCellColor() const				{ return m_crCell; }

protected:

	CRect		m_rcPosition;
	COLORREF	m_crCell;
	CBrush*		m_pBrushCell;
};

#endif // !defined(NCLCOLORCELL_H)
