/********************************************************************* 
**  NAME:  wsntpmenu.h
**
**			interface of CNCLMenu class 
**
**	CONTAINS: CNCLMenu class functions and structure
**			declarations
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntpmenu.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:21      
*********************************************************************/
#ifndef NCLMENU_H
#define NCLMENU_H

class CNCLMenu : public CMenu
{
	DECLARE_DYNAMIC(CNCLMenu)
// Construction
public:
	CNCLMenu();

// Attributes
public:
	CDC m_dcMem;            // Compatible Memory DC
	HBITMAP m_hchkBitmap[100];
	HBITMAP m_hBitmap[100];    // Bitmap to display
	HBITMAP m_hCBitmap;        // current bitmap handler
	int m_bmpnum[100];
	HBITMAP m_hBmpOld;    // Handle of old bitmap to save
	int m_firstdraw;
	UINT m_id[100];
	COLORREF m_bkcolor;
	int m_idnum;
	int m_dragidnum;
	int m_parent, m_subnum, m_iconsize;
// Operations
public:
	void SetBackColor(int r, int g, int b);
	void SetBitmap(HBITMAP hbmap, HBITMAP chkhbmap, int num, UINT id, int bnum);

	void SetIdnum (int num) { m_idnum = num; };
	int GetIdNum() { return m_idnum; };
	void SetDragIdnum (int num) { m_dragidnum = num; };
	int GetDragIdnum() { return m_dragidnum; };
	void Set_mid (int indx, UINT id) { m_id[indx] = id; };
	int HasAllMenuIcon();
	int HasNoMenuIcon();
// Implementation
public:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
	virtual void MeasureItem(LPMEASUREITEMSTRUCT lpMIS);
};

/////////////////////////////////////////////////////////////////////////////
#endif
