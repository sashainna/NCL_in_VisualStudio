/************************************************************************
**
**   FILE NAME: wsntstatbtn2.h
**
**       Description - Functions and struct declarations for
**              CNCLSecButton class
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntsecbtn.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 18:01:12
**********************************************************************
*/
#ifndef NTSECBTN_H
#define NTSECBTN_H
#include "wsntgraphic.h"
#include "wsnttooltip.h"
#include "wsntmDropTarget2.h"

class CNCLSecButton : public CButton
{
DECLARE_DYNAMIC(CNCLSecButton)
public:
	CNCLSecButton(); 
	virtual ~CNCLSecButton(); 
	void	InitDrag();
	void OnDragDropCallback(CPoint pt, char *input_text);
	void SetType(int type)
	{
		m_type = type;
	}
	int GetType()
	{
		return m_type;
	}
	void Reset_Bitmap(char* filename);
	void SetBitmapFile(char *filename);
	char *GetBitmapFile()
	{
		return m_filename.GetBuffer();
	};
	void SetText(CString text);
	char *GetText()
	{
		return m_text.GetBuffer();
	};
	void DrawNormal2();
	void DrawHighLight2();
	void SetParent(CWnd *parent)
	{
		m_parent = parent;
	};
	char *GetPicture()
	{
		return (char*)(m_picture.m_picture);
	}
	void SetButID(UINT fid);
	void SetTColor(COLORREF &color)
	{
		m_tcolor = color;
	};
	void SetBoldFont(int bold);

	UINT GetButID()
	{
		return m_fid;
	}
	void UpdateLabel(CString newlabel);
	void SetStatText(char *text)
	{
		m_stattext = text;
	}
	CString &GetStatText()
	{
		return m_stattext;
	}
	void ShowToolTip(CPoint point);
	void CloseToolTip();
	void set_itemnum(int num)
	{
		m_itemnum = num;
	};
	void ShowDropBox(int flag);
	void SetPageNum(int page)
	{
		m_page = page;
	}
	int GetPageNum()
	{
		return m_page;
	}
protected:
	CString m_tiptext;
	COLORREF m_tcolor;
	UINT m_fid;
	int m_tbold;
	int m_page;
	CString m_text, m_stattext;
	CString m_filename;
	CNCLgraphic m_picture;
	HBITMAP m_Bmap;
	int m_focus, m_disabled;
	CWnd *m_parent;
	CFont m_txtfont;
	CNCLToolTip m_ToolTips;
	int m_itemnum;
	int m_select, m_boxflag;
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
	void DrawFrame(CDC *DC, CRect R, int Inset);
	void DrawFilledRect(CDC *DC, CRect R, COLORREF color);
	void DrawLine(CDC *DC, CRect EndPoints, COLORREF color);
	void DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color);
	void DrawBitmap(CDC *DC);
	void DrawNormal(CRect rect);
	void DrawHighLight(CRect rect, int in_flag = 1);
	void DrawBorders(CDC* pDC, CRect& rect);
	void DrawButtonText(CDC *DC, CRect R, const char *Buf, COLORREF TextColor);

	HCURSOR m_current_cursor;
	int m_type;
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg int  OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);	
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);	
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void OnDestroy();
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	DECLARE_MESSAGE_MAP()
	UINT					m_TimerID;
	CNCLMnDropTarget2		*m_TargetDrop;
	CPoint					m_StartPoint;
	friend class CNCLFormView;
	friend class CNCLForm;
	friend class CNCLFormScrollView;
	friend class CNCLDDform;
	friend class CNCLMnDropTarget2;
private:
};
#endif 
