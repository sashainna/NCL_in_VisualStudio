/*********************************************************************
**    NAME         :  OGView.h
**       CONTAINS:
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       OGView.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:06
*********************************************************************/

#if !defined(AFX_OGVIEW_H__882A98B6_F913_424C_A692_EA5E21EFB28E__INCLUDED_)
#define AFX_OGVIEW_H__882A98B6_F913_424C_A692_EA5E21EFB28E__INCLUDED_
#pragma once

#include "ogcom.h"

class CPoint3D
{
public:
	float x, y, z;
	CPoint3D () { x=y=z=0; }
	CPoint3D (float c1, float c2, float c3)
	{
		x = c1;		z = c2;		y = c3;
	}
	CPoint3D& operator=(const CPoint3D& pt)
	{
		x = pt.x;	z = pt.z;	y = pt.y;
		return *this;
	}
	CPoint3D (const CPoint3D& pt)
	{
		*this = pt;
	}
};


class COGView : public CView
{
protected:
	COGView();
	DECLARE_DYNCREATE(COGView)
public:
	int m_cutmoving;
	int m_wid, m_hgt;
	int m_method, m_action, m_cancel, m_mousex, m_mousey, m_mouseox, m_mouseoy;
	CClientDC	*m_pDC;
	//======== New data
	long	m_BkClr;	// Background color
	HGLRC	m_hRC;		// Rendering context OpenGL
	HGLRC	m_hOverlayRC;
	HDC	m_hdc;		// Windows device context
	GLfloat	m_AngleX;	// Rotation angle (around X-axis)
	GLfloat m_AngleY;	// Rotation angle (around Y-axis)
	GLfloat	m_AngleView;	// Perspective angle
	GLfloat	m_fRangeX;	// Graphics dimension (along X-axis)
	GLfloat m_fRangeY;	// Graphics dimension (along Y-axis)
	GLfloat m_fRangeZ;	// Graphics dimension (along Z-axis)
	GLfloat	m_dx;		// Displacement quantum (along X-axis)
	GLfloat m_dy;		// Displacement quantum (along Y-axis)
	GLfloat	m_xTrans;	// Displacement (along X-axis)
	GLfloat m_yTrans;	// Displacement (along Y-axis)
	GLfloat m_zTrans;	// Displacement (along Z-axis)

	GLfloat m_cutxTrans, m_cutyTrans, m_cutzTrans;

	GLenum	m_FillMode;	// Polygon filling mode
	bool	m_bCaptured;	// Mouse capture flag
	bool	m_bRightButton;	// Right mouse button flag
	bool	m_bQuad;	// Flag of using GL_QUAD (instead of GL_QUAD_STRIP)
	CPoint	m_pt;		// Current mouse position
	UINT	m_xSize;	// Current client window sixe (along X-axis)
	UINT	m_zSize;	// Current client window sixe (along -axis)
	vector < CPoint3D > m_cPoints;	// Graphics dimension (along X-axis)
	int	m_LightParam[11];	// Graphics dimension (along X-axis)
	int m_frame;

	OG_methods my_method;

	//======== Public methods
	COGDoc* GetDocument() { return DYNAMIC_DOWNCAST(COGDoc,m_pDocument); }
	virtual ~COGView();
	int check_event();
	static void check_interrupt(int *);
	static void get_event(int *, int *, int *);

	//======== New methods
	void DrawScene();	// Prepare and store the image
	void DrawCutter();	// Prepare and store the image
	void DrawCutter2();	// Prepare and store the image
	void DrawCutter_frame();
	void DefaultGraphic();	// Create and save the default plot
	//===== Take the data from buffer and store in m_cPoints
	void SetGraphPoints(BYTE* buff, DWORD nSize);
	void SetLightParam (short lp, int nPos);	// Set lighting parameters
	void GetLightParams(int *pPos);			// Get lighting parameters
	void SetLight();				// Set the lighting
	void SetBkColor();				// Set background color

	//{{AFX_VIRTUAL(COGView)
	public:
	virtual void OnDraw(CDC* pDC);
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL
protected:
	//{{AFX_MSG(COGView)
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSize(UINT nType, int cx, int cy);

	afx_msg void OnStyleRedraw();
	afx_msg void OnStylePixel();
	afx_msg void OnBufferFront();
	afx_msg void OnBufferBack();
	afx_msg void OnBufferBoth();
	afx_msg void OnBufferOverlay();
	afx_msg void OnCopySwap();
	afx_msg void OnCopySmall();
	afx_msg void OnCopyLarge();
	afx_msg void OnCopyFull();
	afx_msg void OnDepthEnable();
	afx_msg void OnDepthDisable();
	afx_msg void OnCutterWire();
	afx_msg void OnCutterShaded();
	afx_msg void OnActionCutter();
	afx_msg void OnActionAll();
	afx_msg void OnActionDrag();
	afx_msg void OnActionLine();
	afx_msg void OnActionBox();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
private:
	void MenuDefault(int menu, int item, int nitem);
	void MenuCheck(int menu, int item, int flag);
};

static COGView *my_this;

//{{AFX_INSERT_LOCATION}}
#endif
