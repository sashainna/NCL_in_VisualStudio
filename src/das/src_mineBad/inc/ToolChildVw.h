/************************************************************************
c
c   FILE NAME: ToolChildView.h
c
c	 CONTAINS: 
c		Defined all child view class
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       ToolChildVw.h , 25.3
c    DATE AND TIME OF LAST  MODIFICATION
c       12/08/15 , 13:58:45
c
c**********************************************************************
*/
#ifndef TOOLCHILDVIEW_H
#define TOOLCHILDVIEW_H

#include "toolibDoc.h"
#include "ToolDispDlg.h"
#include "toollstctl.h"
#include "toolibdata.h"
/////////////////////////////////////////////////////////////////////////////

class CChildView1 : public CFormView
{
	DECLARE_DYNCREATE(CChildView1)
protected:
	CChildView1();           // protected constructor used by dynamic creation
	~CChildView1();
	IPicture* m_picture;
	HBITMAP m_hBmp;
	BYTE* m_BufferBytes;
	HDC m_memdc;
	void LoadPictureFile(LPCTSTR szFile);
	CFont m_fntPage;
	int m_init;
	BOOL CreatePicBitmap(CDC *pDC);
	void FreePictureData();
	BOOL LoadPictureData(BYTE* pBuffer, int nSize);
// Form Data
public:
	//{{AFX_DATA(CChildView1)
	enum { IDD = IDD_CHILD1 };
	int m_cuttype, m_def_cut;
	CString m_tool, m_desp;
	CString m_def1, m_def2, m_def3, m_def4, m_def5, m_def6, m_def7;
	CString m_parm1, m_parm2, m_parm3, m_parm4, m_parm5, m_parm6, m_parm7;
	CString m_cdef1, m_cdef2, m_cdef3, m_cdef4, m_cdef5, m_cdef6, m_cdef7;
	CString m_cparm1, m_cparm2, m_cparm3, m_cparm4, m_cparm5, m_cparm6, m_cparm7;
	char m_file[256], m_load_file[256];

	//}}AFX_DATA

// Attributes
public:
	CToolibDoc* GetDocument()
	{
		ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CToolibDoc)));
		return (CToolibDoc*) m_pDocument;
	}

// Operations
public:
	int IsDocModified();
	void UpdateCutType();
	afx_msg void OnTacceptEditChange();
// Implementation
protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	virtual void OnInitialUpdate();

	// Generated message map functions
	//{{AFX_MSG(CChildView1)
	afx_msg void OnDefine_Cutter();
	afx_msg void OnPsuedo_Cutter();
    afx_msg void OnTCuttype();
	afx_msg void OnPaint();
	afx_msg void OnTCutEditChange();
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
//////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
class CChildView2 : public CFormView
{
	DECLARE_DYNCREATE(CChildView2)
protected:
	CChildView2();           // protected constructor used by dynamic creation
	CFont m_fntPage;

// Form Data
public:
	int m_segment, m_moving, m_shade, m_loadcom, 
		m_symbol_ck, m_shank_ck, m_holder_ck;
	CString m_symbol, m_holder, m_shank, m_tooldraw;
	CString m_com[42];

	CToolDispDlg symbol_value, shank_value, holder_value;
	//{{AFX_DATA(CChildView2)
	enum { IDD = IDD_CHILD2 };
	//}}AFX_DATA

// Attributes
public:
	
	CToolibDoc* GetDocument()
	{
		ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CToolibDoc)));
		return (CToolibDoc*) m_pDocument;
	}
	afx_msg void OnSymbol();
	afx_msg void OnShank();
	afx_msg void OnHolder();
// Operations
public:

	int IsDocModified();
	void SetSymbol(CString sym, int type);
	void SetShade(int pos);
	void load_cutter_disp();
	void adjust_ddata_field();
// Implementation
protected:
	virtual ~CChildView2();
	CBrush* m_pEditBkBrush;
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	virtual void OnInitialUpdate();
	void OnToolDisplay(int flag);
	// Generated message map functions
	//{{AFX_MSG(CChildView2)
	afx_msg void OnDispLoad();
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	afx_msg void OnDefine_Command();
	afx_msg void OnTooldraw();
	afx_msg void OnSymbol_disp();
	afx_msg void OnShank_disp();
	afx_msg void OnHolder_disp();
	afx_msg void OnShdchg();
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
//////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
class CChildView3 : public CFormView
{
	DECLARE_DYNCREATE(CChildView3)
protected:
	CChildView3();           // protected constructor used by dynamic creation
// Form Data
	CFont m_fntPage;
	CTLListCtrl m_listctl;
	CImageList *m_rimglist;
public:
	int m_cuttype;
	void UpdateList(double toolnum=-1, int pos=-1);
	void UpdateTool(TL_tooldata_rec tool_data);
	int GetSelPos(double * toolnum);
	void SetSelPos(int pos=0);
	void SelListPos(int pos = 0);
	//{{AFX_DATA(CChildView3)
	enum { IDD = IDD_CHILD3 };
	//}}AFX_DATA

// Attributes
public:
	CToolibDoc* GetDocument()
			{
				ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CToolibDoc)));
				return (CToolibDoc*) m_pDocument;
			}

// Operations
public:
	void tool_deleteone(double toolno);

// Implementation
protected:
	virtual ~CChildView3();
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	virtual void OnInitialUpdate();
	afx_msg void OnSize(UINT nType, int cx, int cy);
	// Generated message map functions
	//{{AFX_MSG(CChildView3)
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	afx_msg void OnSelList();
	afx_msg void OnToolSelected(NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void OnToolColumnSelected(NMHDR *pNMHDR, LRESULT *pResult);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
#endif
