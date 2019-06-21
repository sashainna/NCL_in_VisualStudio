/************************************************************************
**
**   FILE NAME: wsntbmpbtn.h
**
**       Description - Functions and struct declarations for
**              CNCLBmpButton class
**    COPYRIGHT 2011 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntbmpbtn.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:15
**********************************************************************
*/
#ifndef NTBMPBTN_H
#define NTBMPBTN_H

class CNCLBmpButton : public CButton
{
DECLARE_DYNAMIC(CNCLBmpButton)
public:
	CNCLBmpButton(); 
	virtual ~CNCLBmpButton(); 
	void SetType(int type)
	{
		m_type = type;
	}
	
protected:
	HBITMAP m_hBitmap;
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
	void DrawFrame(CDC *DC, CRect R, int Inset);
	void DrawFilledRect(CDC *DC, CRect R, COLORREF color);
	void DrawLine(CDC *DC, CRect EndPoints, COLORREF color);
	void DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color);
	void DrawDownArrow(CDC *DC, int left, int top, int flag);
	void DrawUpArrow(CDC *DC, int left, int top, int flag);
	void DrawNormal(CRect rect);
	void DrawHighLight(CRect rect);
	HCURSOR m_current_cursor;
	int m_type;
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	DECLARE_MESSAGE_MAP()
	friend class CNCLCmdBar;
private:
};
#endif 
