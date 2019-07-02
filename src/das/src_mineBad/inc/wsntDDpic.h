/************************************************************************
**
**   FILE NAME: wsntDDpic.h
**
**       Description - Functions and struct declarations for
**              CNCLDDPicWin class, it's a window display a picture file (drag&drop)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDpic.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			05/04/15 , 11:45:31
**********************************************************************
*/
#ifndef NCLDDPICWIN_H_
#define NCLDDPICWIN_H_

#pragma once
	
#include "wsntgraphic.h"
#include "wsntmDropTarget2.h"
#include "wsntFormProp.h"

enum {TRANGLEHEIGHT		= 12};
enum {TRANGLEWIDTH		= 10};
enum {BORDER_WIDTH		= 1};
enum {PADDING			= TRANGLEHEIGHT/2+1 - BORDER_WIDTH*2};

class CNCLDDPicWin : public CWnd
{
	DECLARE_DYNAMIC(CNCLDDPicWin)

public:
	CNCLDDPicWin(char *name, char* filename, CWnd *parent);
	virtual ~CNCLDDPicWin();
	void	InitDrag();

	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext=NULL);
	void Reset_picture(char* filename);
	void OnDragDropCallback(CPoint pt, char *input_text);
	void SetParent(CWnd *parent) { m_parent = parent; };
	void SetProperty(CNCLFormProp *prop_dlg);
	CNCLFormProp *GetPropertyPage()
	{
		return m_prop_dlg;
	}
	void SetItemNo(int itemno);
	void DrawGuides(CPoint org_pt, POINT ptCursor);
	void SetHotSpotRect(CRect rect);
	void ReseteHotSpotRect();
	void DrawHotspotRect(CDC *dc, CRect &rect);
	int Handle_Hotspot_event(int evt, UINT nFlags, CPoint point);
	void Reset_selrec(CRect rect);
	void DrawSelect();
	void Reset_picture_area();

protected:
	CNCLFormProp* m_prop_dlg;
	
	//{{AFX_MSG(CNCLDDPicWin)
	afx_msg void OnDestroy();
	afx_msg void OnPaint();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnMove(int x, int y);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	//{{AFX_VIRTUAL(CNCLDDPicWin)
	//}}AFX_VIRTUAL
private:
	int m_x, m_y;
	CPoint m_mpoint;
	CString m_filename;
	char m_name[40];

	int m_itemno;
	HBITMAP m_Bmap;
	CNCLMnDropTarget2		*m_TargetDrop;
	CPoint					m_StartPoint;
	UINT					m_TimerID;
	CImageList *m_DragImage;
	CImageList *m_DragAllImage;
	int m_dragimg_create;

	CRect m_area[100], m_selsize[10], m_sizerec, m_hotspot_rect;
	int m_area_num, m_draw_areanum;
	int m_hittest, m_sizedir, m_resize;
	int m_buttondown;
	HCURSOR m_cursor[11];

	UINT m_picID[100];
	void UpdateSize();
	void Paint();
	CDC	*m_pDC;
	CNCLgraphic m_picture;
	CWnd *m_parent;

	friend class CNCLMnDropTarget2;
	friend class CNCLDDform;
};
#endif
