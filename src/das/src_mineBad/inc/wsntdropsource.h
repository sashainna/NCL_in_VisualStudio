/********************************************************************* 
**  NAME:  wsntdropsource.h
**
**  Description - Functions and struct declarations for
**              CNCLDropSource class (Class that inherits from IDropTarget)
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**     wsntdropsource.h , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**     04/29/15 , 15:07:17
*********************************************************************/
#ifndef WSNTDROPSOURCE_H
#define WSNTDROPSOURCE_H

interface CNCLDropSource : public COleDropSource
{
public:
	CNCLDropSource();
	~CNCLDropSource();
	CImageList *m_DragImage;
	CWnd *m_parent;
	CPoint m_point;
	CRect m_rect;
	CPoint m_ptCursor, m_dragpt;
	void SetImageList(CImageList *imagelist);
	void SetDragPt(CPoint point)
	{
		m_dragpt = point;
	}
	void SetAttWindow(CWnd *wnd, CPoint point, CRect rect);

	BOOL	OnBeginDrag(CWnd* pWnd);
	SCODE	GiveFeedback(DROPEFFECT dropEffect);
	SCODE	QueryContinueDrag(BOOL bEscapePreessed, DWORD dwkeyState);
	void AdjustGrid(POINT &ptCursor);
}; 

#endif
