/************************************************************************
**
**   FILE NAME: wsntclrbtn.h
**
**       Description - Functions and struct declarations for
**              CNCLColorButton class (color button for form)
**    COPYRIGHT 2006 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntclrbtn.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:15
**********************************************************************
*/
#ifndef NTCOLORBTN_H
#define NTCOLORBTN_H

class CNCLColorButton : public CButton
{
DECLARE_DYNAMIC(CNCLColorButton)
public:
	CNCLColorButton(); 
	virtual ~CNCLColorButton(); 

	void set_color(
		const COLORREF BGColor = RGB(192, 192, 192),		// gray button
		const COLORREF FGColor = RGB(1, 1, 1)				// black text 
	);

	
protected:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
	void DrawFrame(CDC *DC, CRect R, int Inset);
	void DrawFilledRect(CDC *DC, CRect R, COLORREF color);
	void DrawLine(CDC *DC, CRect EndPoints, COLORREF color);
	void DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color);
	void DrawButtonText(CDC *DC, CRect R, const char *Buf, COLORREF TextColor);

	COLORREF GetFGColor() { return m_fg; }	
	COLORREF GetBGColor() { return m_bg; }

private:
	COLORREF m_fg, m_bg;
};
#endif 
