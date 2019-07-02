/********************************************************************* 
**  NAME:  wsnttmenu.h
**
**			interface of CToolmenu class functions
**	
**	CONTAINS: CToolmenu class functions and structure
**			declaration
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnttmenu.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:21
*********************************************************************/
#ifndef NCLTOOLMENU_H
#define NCLTOOLMENU_H

#include "wsncltoolbar.h"
#include "wsntmdroptarget.h"
struct tbblabel
{
	UINT id;
	char label[80];
};

class CToolmenu : public CNCLToolBar
{
// Constructor
public:
	CToolmenu(int menunum, int type=1);
	void SetColumns(UINT nColumns, int wrap);
	void SetButSizes(SIZE bsize);
	int GetButSize_X() { return m_sizeButton.cx; };
	int GetButSize_Y() { return m_sizeButton.cy; };
	void SetBitmapSize(int cx, int cy);
	int HasAllMenuIcon();
// Attributes
public:
	int m_mtype;
	int m_icon;
	int m_created;
	int pos[5];
	int m_sizex, m_sizey;
	int m_floating;
	int m_first;
	int m_defchg;
// Operations
public:
	void SetLabels(tbblabel* labels=NULL, BOOL bShow=TRUE);
	void ShowLabels(BOOL bShow=TRUE);
	void SetPos(int pos[3]);
	void SetTBSize(int cx, int cy);
	int GetStringNum(CString strTemp);
	void SetDropId(int bindx, int nID) { m_dropid[bindx] = nID; };
	int GetDropId(int bindx) { return m_dropid[bindx]; };
	void SetFuncId(int bindx, int nID) { m_funcid[bindx] = nID; };
	int GetFuncId(int bindx) { return m_funcid[bindx]; };
	void GetButSize (SIZE &bsize) { bsize.cx = m_sizeButton.cx;
									bsize.cy = m_sizeButton.cy; };
	void UpdateMenuBar(CPoint pt, char* input_text);
	void MovedMenuItem(int itemnum, int addmenu, int additem, int choice);
	int GetItemIndx_hit(CPoint point, int &hit);
	void HandleRButtonUp(int flag);

// Implementation
public:
	virtual ~CToolmenu();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	CNCLMnDropTarget		*m_TargetDrop;
	UINT					m_TimerID;
	friend class CNCLMnDropTarget;

protected:
	CRect	m_rBigBtn;
	CRect	m_rSmallBtn; 
	CBrush* m_ptoolBkBrush;
	int m_dropid[100];
	int m_funcid[100];

// Generated message map functions
protected:
	virtual void PostNcDestroy();
	//{{AFX_MSG(CToolmenu)
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
#endif
