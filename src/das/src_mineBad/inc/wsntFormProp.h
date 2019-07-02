/************************************************************************
**
**   FILE NAME: wsntFormProp.h
**
**       Description - Functions and struct declarations for
**              CNCLFormProp class
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntFormProp.h , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**			08/18/15 , 08:38:51
**********************************************************************
*/
#if !defined(WSNTFORMPROP_INCLUDE)
#define WSNTFORMPROP_INCLUDE

#if _MSC_VER >= 1000
#pragma once
#endif 
#include "wsntres.h"
#include "udforms.h"
#include "wsntwininfo.h"

// CNCLFormProp property

class CNCLFormProp
{
public:
	CNCLFormProp(int dtype, int itype = -1, CWnd* pParent = NULL);
	virtual ~CNCLFormProp();
	void adddata(int num, UD_DASIN *data);
	void CopyPropertyPage(CNCLFormProp *prop_dlg_from);
	void GetWinInfo(CNCLWinInfo *info);
	void SetWinInfo(CNCLWinInfo *info);
	CWnd *m_parent;
	int m_dtype, m_itype;
	CString m_label;
	int m_pos[2], m_size[2];
	double m_font;
	UD_DASIN m_range[2]; 
	int m_range_flag;
	int m_type, m_input, m_len, m_prec, m_active, m_input_itemno;
	int m_page, m_justified;
	CString m_limit, m_choices, m_color, m_pcolor;
/*
......added for picture field
*/
/*	CString m_pic_label;
	CString m_pic_tooltip;
	CRect m_pic_rect; //percentage, not pixel
	CString m_pic_params;
*/
	UD_PICAREA *m_picarea;
	int m_picarea_no;
	int m_pic_act_area;
	void UpdatePropertySize(int cx, int cy);
	void SetLabelValue(CString label, CString pcolor)
	{
		m_label = label;
		m_pcolor = pcolor;
	};
//	int GetPicRect(char *pic_label, CRect *rect, int indx);
	int GetPicRect(char *pic_label, float rect[4], int indx);
	int Add_picarea();
	void Delete_picarea(int indx);
	int GetActivePicArea();
	void free_picdata();
	int SetNewPicRect(char *pic_label, float rect[4], int indx);

protected:
};
#endif // !defined(WSNTFORMPROP_INCLUDE)
