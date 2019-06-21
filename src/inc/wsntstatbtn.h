/************************************************************************
**
**   FILE NAME: wsntstatbtn.h
**
**       Description - Functions and struct declarations for
**              CNCLStatButton class
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntstatbtn.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:21
**********************************************************************
*/
#ifndef NTSTATBTN_H
#define NTSTATBTN_H
#include "wsntgraphic.h"
#include "wsnttooltip.h"

class CNCLStatButton : public CButton
{
DECLARE_DYNAMIC(CNCLStatButton)
public:
	CNCLStatButton(); 
	virtual ~CNCLStatButton(); 
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
	void UpdateStatusBar(CPoint pt, char* input_text);
protected:
	CString m_tiptext;
	UINT m_fid;
	CString m_text, m_stattext;
	CString m_filename;
	CNCLgraphic m_picture;
	HBITMAP m_Bmap;
	int m_focus;
	CWnd *m_parent;
	CFont m_txtfont;
	CNCLToolTip m_ToolTips;
	int m_barnum, m_itemnum;

	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
	void DrawFrame(CDC *DC, CRect R, int Inset);
	void DrawFilledRect(CDC *DC, CRect R, COLORREF color);
	void DrawLine(CDC *DC, CRect EndPoints, COLORREF color);
	void DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color);
	void DrawBitmap(CDC *DC);
	void DrawNormal(CRect rect);
	void DrawHighLight(CRect rect, int in_flag = 1);
	HCURSOR m_current_cursor;
	int m_type;
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg int  OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);	
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);	
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	DECLARE_MESSAGE_MAP()

	CNCLMnDropTarget		*m_TargetDrop;
	CPoint					m_StartPoint;
	UINT					m_TimerID;
	friend class CNCLMnDropTarget;
	friend class CNCLStatusBar;

private:
};
#endif 
