/************************************************************************
**
**   FILE NAME: wsntWininfo.h
**
**       Description - Functions and struct declarations for
**              CNCLWinInfo class (Class for window information)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntwininfo.h , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**			08/18/15 , 08:55:28
**********************************************************************
*/
#if !defined(WSNTWININFO__INCLUDED_)
#define WSNTWININFO__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

extern "C" char * uu_malloc(int);
extern "C" void uu_free(char*);

class CNCLWinInfo
{
public:
	CNCLWinInfo()  // standard constructor
	{
		m_action = -1;
		m_action_fldno = 0;
		m_dtype = -1;
		m_page = -1;
	}
	void CopyInfo(CNCLWinInfo *info_from)
	{
/*
.....same object
*/
		if (info_from==this)
			return;
		m_dtype = info_from->m_dtype;
		m_itype = info_from->m_itype;
		m_label = info_from->m_label;
		m_pos[0] = info_from->m_pos[0];
		m_size[0] = info_from->m_size[0];
		m_pos[1] = info_from->m_pos[1];
		m_size[1] = info_from->m_size[1];
		m_range[0] = info_from->m_range[0];
		m_range[1] = info_from->m_range[1];
		m_range_flag = info_from->m_range_flag;
		m_font = info_from->m_font;
		m_type = info_from->m_type;
		m_input = info_from->m_input;
		m_len = info_from->m_len; 
		m_prec = info_from->m_prec;
		m_active = info_from->m_active;
		m_input_itemno = info_from->m_input_itemno;
		m_limit = info_from->m_limit;
		m_choices = info_from->m_choices;
		m_color = info_from->m_color;
		m_pcolor = info_from->m_pcolor;
		m_page = info_from->m_page;
		m_action = info_from->m_action;
		m_action_fldno = info_from->m_action_fldno;
		m_justified = info_from->m_justified;
		int k;
		if ((m_picarea_no>0)&&(m_picarea!=NULL))
		{
			for (k=0; k<m_picarea_no; k++)
			{
				if (m_picarea[k].params!=NULL)
					uu_free(m_picarea[k].params);
				if (m_picarea[k].tooltext!=NULL)
					uu_free(m_picarea[k].tooltext);
			}
			uu_free((char*)m_picarea);
		}
		m_picarea_no = info_from->m_picarea_no;
		if ((m_picarea_no>0)&&(info_from->m_picarea!=NULL))
		{
			m_picarea = (UD_PICAREA *) uu_malloc(m_picarea_no*sizeof(UD_PICAREA));
			for (k=0; k<m_picarea_no; k++)
			{
				if (info_from->m_picarea[k].params!=NULL)
				{
					m_picarea[k].params = (char*) uu_malloc((100)*sizeof(char));
					strcpy(m_picarea[k].params, info_from->m_picarea[k].params);
				}
				else
					m_picarea[k].params = NULL;
				if (info_from->m_picarea[k].tooltext!=NULL)
				{
					m_picarea[k].tooltext = (char*) uu_malloc((100)*sizeof(char));
					strcpy(m_picarea[k].tooltext, info_from->m_picarea[k].tooltext);
				}
				else
					m_picarea[k].tooltext = NULL;
				strcpy(m_picarea[k].name, info_from->m_picarea[k].name);
				m_picarea[k].xmin = info_from->m_picarea[k].xmin;
				m_picarea[k].ymin = info_from->m_picarea[k].ymin;
				m_picarea[k].xmax = info_from->m_picarea[k].xmax;
				m_picarea[k].ymax = info_from->m_picarea[k].ymax;
			}
		}
		else
		{
			m_picarea = NULL;
			m_picarea_no = 0;
			m_pic_act_area = 0;
		}
	}

public:
/*
......this field added for undo/redo action
......default -1, no action.
......0: for save undo/redo affected number of field
......1: adding a field (new)
......when save in the top for undo/redo
......2: moving fields
......3: delete a field
......edit field
......4: position/size change
......5: label change
......6: font change
......7: input type change
......8: m_input_itemno change
......9: m_choices change
......10: m_color, m_pcolor changes
......11: m_page (section change)
......12: m_len change
......13: m_active change
......14: m_limit change
......15: m_prec change
......16: m_range change
......17: picture field data changed
......
......20: section addition
......21: section delete
......30: Add item by replace the existing one: delete an existing one then add the new one
......		(delete and add action)
......31: Moving item by replace the existing one: delete an existing one then moving the current one
......		into the new place (delete and moving action)
......40: hotspot action, bew created by dragging. if edit picture data, action=17
*/
	int m_action;
/*
.....this is the number of fields affected by active
.....for example, delete multi-fields one time but we will 
.....save it one by one into undo list, so we need to know 
.....how many field affect by one action
*/
	int m_action_fldno;
	CString m_label;
	int m_pos[2], m_size[2];
	double m_font;
	UD_DASIN m_range[2]; 
	int m_range_flag;
/*
.....we will also used those number to save undo info
....when save undo info, when m_dtype = -1, mean saved
....total undo item and window item info
*/
	int m_dtype, m_itype;
	int m_type, m_input, m_len, m_prec, m_active, m_input_itemno;
	CString m_limit, m_choices, m_color, m_pcolor;
	int m_page;
	int m_justified;
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
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(WSNTWININFO__INCLUDED_)
