/************************************************************************
**
**   FILE NAME: wsntpicsel.h
**
**       Description - Functions and struct declarations for
**              CNCLPicSelWin class, it's a window display a picture file
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntpicsel.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:20
**********************************************************************
*/
#ifndef NCLPICSELWIN_H_
#define NCLPICSELWIN_H_

#pragma once
	
#include "wsntgraphic.h"
#include "wsnttooltip.h"

enum {TRANGLEHEIGHT		= 12};
enum {TRANGLEWIDTH		= 10};
enum {BORDER_WIDTH		= 1};
enum {PADDING			= TRANGLEHEIGHT/2+1 - BORDER_WIDTH*2};

class CNCLPicSelWin : public CWnd
{
public:
	CNCLPicSelWin(char *name, char* filename, CWnd *parent);
	virtual ~CNCLPicSelWin();

	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext=NULL);
	void Reset_picture(char* filename);
	void set_picarea(UD_PICAREA picarea, UINT pID);
	char m_name[40];

private:
	CString m_filename;
	HBITMAP m_Bmap;
	int m_buttondown;
	CPoint	m_StartPoint;
	char *m_tiptext;
	CNCLToolTip m_RectTips;
	int m_loadimage, m_bmpwid, m_bmphgt;
	BOOL m_bCapture;
	CRect m_area[100];
	int m_area_num, m_draw_areanum;
	UINT m_picID[100];
	char *m_params[100];
	char *m_tooltext[100];
	void UpdateSize();
	void DrawHightRect(CPoint point);
	void Paint();
	CDC	*m_pDC;
	CNCLgraphic m_picture;
	CWnd *m_parent;
	BOOL m_MouseTrack;

	int ifarea_notchg(CPoint point);
	int GetAreaNum(CPoint point);
	int IsCursorOn();

protected:
	
	//{{AFX_MSG(CNCLPicSelWin)
	afx_msg void OnDestroy();
	afx_msg void OnPaint();
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnMouseLeave();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	//{{AFX_VIRTUAL(CNCLPicSelWin)
	//}}AFX_VIRTUAL
};
#endif
