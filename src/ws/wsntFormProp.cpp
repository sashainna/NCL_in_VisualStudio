/************************************************************************
**
**   FILE NAME: wsntformprop.cpp
**
**	 Description - Functions and implementations for
**		CNCLFormProp class
**
**	 CONTAINS: 
**		class functions of CNCLFormProp class
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntFormProp.cpp , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**			08/18/15 , 08:37:54
***********************************************************************
*/
#include "stdafx.h"
#include "wsntFormProp.h"
#include "dtypes.h"
#include "xenv1.h"
#include "wsntfrmview.h"

/***********************************************************************
**   FUNCTION: CNCLFormProp
**		Constructor of class CNCLFormFrm
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormProp::CNCLFormProp(int dtype, int itype, CWnd* pParent /*=NULL*/)
{
	m_dtype = dtype;
	m_itype = itype;

	m_label = "";
	m_pos[0] = 0;
	m_pos[1] = 0;
	m_size[0] = 50;
	m_size[1] = 17;

	m_type = UD_DASSTRING;
	m_input = 1; /* FORM_STRING */
	
	m_limit = "";

	m_choices = "";
	m_len = 8;
	m_prec = 1;
	m_range_flag = 0;

	m_pcolor = "Default, Default"; 
	m_color = "Default, Default"; 
	m_font = 1.0;
	m_active = 0;
	m_input_itemno = -1;
	m_parent = pParent;
	m_page = 0;

	m_picarea = NULL;
	m_picarea_no = 0;
	m_pic_act_area = 0;
/*
....horz
*/
	m_justified = 0;
}

CNCLFormProp::~CNCLFormProp()
{
}
/***********************************************************************
**
**   FUNCTION: adddata(int num, UD_DASIN *data)
**
**       this function add choice data value into local 
**			choices string
**
**   INPUT:  data: choice data
**				num: number of choice items
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLFormProp::adddata(int num, UD_DASIN *data)
{
	int len;
	for (int j=0; j<num; j++)
	{
		m_choices += "\"";
		m_choices += data[j].dstr;
		if (j==num-1)
			m_choices += "\"";
		else
			m_choices += "\",";
	}
}

/***********************************************************************
**
**   FUNCTION: CopyPropertyPage(CNCLFormProp *prop_dlg_from)
**
**       this function copy local input property structure into local 
**			property value
**
**   INPUT:  prop_dlg_from: input property structure
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLFormProp::CopyPropertyPage(CNCLFormProp *prop_dlg_from)
{
/*
.....same object
*/
	if (prop_dlg_from==this)
		return;
	m_dtype = prop_dlg_from->m_dtype;
	m_itype = prop_dlg_from->m_itype;
	m_label = prop_dlg_from->m_label;
	m_pos[0] = prop_dlg_from->m_pos[0];
	m_size[0] = prop_dlg_from->m_size[0];
	m_pos[1] = prop_dlg_from->m_pos[1];
	m_size[1] = prop_dlg_from->m_size[1];
	m_range[0] = prop_dlg_from->m_range[0];
	m_range[1] = prop_dlg_from->m_range[1];
	m_range_flag = prop_dlg_from->m_range_flag;
	m_font = prop_dlg_from->m_font;
	m_type = prop_dlg_from->m_type;
	m_input = prop_dlg_from->m_input;
	m_len = prop_dlg_from->m_len; 
	m_prec = prop_dlg_from->m_prec;
	m_active = prop_dlg_from->m_active;
	m_input_itemno = prop_dlg_from->m_input_itemno;
	m_limit = prop_dlg_from->m_limit;
	m_choices = prop_dlg_from->m_choices;
	m_color = prop_dlg_from->m_color;
	m_pcolor = prop_dlg_from->m_pcolor;
	m_page = prop_dlg_from->m_page;
	m_justified = prop_dlg_from->m_justified;
/*
	m_pic_label = prop_dlg_from->m_pic_label;
	m_pic_tooltip = prop_dlg_from->m_pic_tooltip;
	m_pic_rect.left = prop_dlg_from->m_pic_rect.left;
	m_pic_rect.right = prop_dlg_from->m_pic_rect.right;
	m_pic_rect.top = prop_dlg_from->m_pic_rect.top;
	m_pic_rect.bottom = prop_dlg_from->m_pic_rect.bottom;
	m_pic_params = prop_dlg_from->m_pic_params;
*/
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
		m_picarea = NULL;
	}
	m_picarea_no = prop_dlg_from->m_picarea_no;
	m_pic_act_area = prop_dlg_from->m_pic_act_area;
	if ((m_picarea_no>0)&&(prop_dlg_from->m_picarea!=NULL))
	{
		m_picarea = (UD_PICAREA *) uu_malloc(m_picarea_no*sizeof(UD_PICAREA));
		for (k=0; k<m_picarea_no; k++)
		{
			if (prop_dlg_from->m_picarea[k].params!=NULL)
			{
				m_picarea[k].params = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_picarea[k].params, prop_dlg_from->m_picarea[k].params);
			}
			else
				m_picarea[k].params = NULL;
			if (prop_dlg_from->m_picarea[k].tooltext!=NULL)
			{
				m_picarea[k].tooltext = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_picarea[k].tooltext, prop_dlg_from->m_picarea[k].tooltext);
			}
			else
				m_picarea[k].tooltext = NULL;
			strcpy(m_picarea[k].name, prop_dlg_from->m_picarea[k].name);
			m_picarea[k].xmin = prop_dlg_from->m_picarea[k].xmin;
			m_picarea[k].ymin = prop_dlg_from->m_picarea[k].ymin;
			m_picarea[k].xmax = prop_dlg_from->m_picarea[k].xmax;
			m_picarea[k].ymax = prop_dlg_from->m_picarea[k].ymax;
		}
	}
	else
	{
		m_picarea = NULL;
		m_picarea_no = 0;
		m_pic_act_area = 0;
	}
}

/***********************************************************************
**
**   FUNCTION: GetWinInfo(CNCLWinInfo *info)
**
**       this function assign local 
**			property value into output window information
**
**   INPUT:  info: input window information
**
**   OUTPUT :   info: output window information
**   RETURN:    None
**
**********************************************************************/
void CNCLFormProp::GetWinInfo(CNCLWinInfo *info)
{
	info->m_dtype = m_dtype;
	info->m_itype = m_itype;
	info->m_label = m_label;
	info->m_pos[0] = m_pos[0];
	info->m_pos[1] = m_pos[1];
	info->m_size[0] = m_size[0];
	info->m_size[1] = m_size[1];
	info->m_range[0] = m_range[0];
	info->m_range[1] = m_range[1];
	info->m_range_flag = m_range_flag;
	info->m_font = m_font;
	info->m_type = m_type;
	info->m_input = m_input;
	info->m_len = m_len;
	info->m_prec = m_prec;
	info->m_active = m_active;
	info->m_input_itemno = m_input_itemno;
	info->m_limit = m_limit;
	info->m_choices = m_choices;
	info->m_color = m_color;
	info->m_pcolor = m_pcolor;
	info->m_page = m_page;
	info->m_justified = m_justified;
/*
	info->m_pic_label = m_pic_label;
	info->m_pic_tooltip = m_pic_tooltip;
	info->m_pic_rect.left = m_pic_rect.left;
	info->m_pic_rect.right = m_pic_rect.right;
	info->m_pic_rect.top = m_pic_rect.top;
	info->m_pic_rect.bottom = m_pic_rect.bottom;
	info->m_pic_params = m_pic_params;
*/
	int k;
	info->m_picarea_no = m_picarea_no;
	info->m_pic_act_area = m_pic_act_area;
	if ((m_picarea_no>0)&&(m_picarea!=NULL))
	{
		info->m_picarea = (UD_PICAREA *) uu_malloc(m_picarea_no*sizeof(UD_PICAREA));
		for (k=0; k<m_picarea_no; k++)
		{
			if (m_picarea[k].params!=NULL)
			{
				info->m_picarea[k].params = (char*) uu_malloc((100)*sizeof(char));
				strcpy(info->m_picarea[k].params, m_picarea[k].params);
			}
			else
				info->m_picarea[k].params = NULL;
			if (m_picarea[k].tooltext!=NULL)
			{
				info->m_picarea[k].tooltext = (char*) uu_malloc((100)*sizeof(char));
				strcpy(info->m_picarea[k].tooltext, m_picarea[k].tooltext);
			}
			else
				info->m_picarea[k].tooltext = NULL;
			strcpy(info->m_picarea[k].name, m_picarea[k].name);
			info->m_picarea[k].xmin = m_picarea[k].xmin;
			info->m_picarea[k].ymin = m_picarea[k].ymin;
			info->m_picarea[k].xmax = m_picarea[k].xmax;
			info->m_picarea[k].ymax = m_picarea[k].ymax;
		}
	}
	else
	{
		info->m_picarea = NULL;
		info->m_picarea_no = 0;
		info->m_pic_act_area = 0;
	}
}
/***********************************************************************
**
**   FUNCTION: SetWinInfo(CNCLWinInfo *info)
**
**       this function assign input window information into local 
**			property value
**
**   INPUT:  info: input window information
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLFormProp::SetWinInfo(CNCLWinInfo *info)
{
	m_dtype = info->m_dtype;
	m_itype = info->m_itype;
	m_label = info->m_label;
	m_pos[0] = info->m_pos[0];
	m_pos[1] = info->m_pos[1];
	m_size[0] = info->m_size[0];
	m_size[1] = info->m_size[1];
	m_range[0] = info->m_range[0];
	m_range[1] = info->m_range[1];
	m_range_flag = info->m_range_flag;
	m_font = info->m_font;
	m_type = info->m_type;
	m_input = info->m_input;
	m_len = info->m_len;
	m_prec = info->m_prec;
	m_active = info->m_active;
	m_input_itemno = info->m_input_itemno;
	m_limit = info->m_limit;
	m_choices = info->m_choices;
	m_color = info->m_color;
	m_pcolor = info->m_pcolor;
	m_page = info->m_page;
	m_justified = info->m_justified;
/*
	m_pic_label = info->m_pic_label;
	m_pic_tooltip = info->m_pic_tooltip;
	m_pic_rect.left = info->m_pic_rect.left;
	m_pic_rect.right = info->m_pic_rect.right;
	m_pic_rect.top = info->m_pic_rect.top;
	m_pic_rect.bottom = info->m_pic_rect.bottom;
	m_pic_params = info->m_pic_params;
*/
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
		m_picarea = NULL;
	}
	m_picarea_no = info->m_picarea_no;
	m_pic_act_area = info->m_pic_act_area;
	if ((m_picarea_no>0)&&(info->m_picarea!=NULL))
	{
		m_picarea = (UD_PICAREA *) uu_malloc(m_picarea_no*sizeof(UD_PICAREA));
		for (k=0; k<m_picarea_no; k++)
		{
			if (info->m_picarea[k].params!=NULL)
			{
				m_picarea[k].params = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_picarea[k].params, info->m_picarea[k].params);
			}
			else
				m_picarea[k].params = NULL;
			if (info->m_picarea[k].tooltext!=NULL)
			{
				m_picarea[k].tooltext = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_picarea[k].tooltext, info->m_picarea[k].tooltext);
			}
			else
				m_picarea[k].tooltext = NULL;
			strcpy(m_picarea[k].name, info->m_picarea[k].name);
			m_picarea[k].xmin = info->m_picarea[k].xmin;
			m_picarea[k].ymin = info->m_picarea[k].ymin;
			m_picarea[k].xmax = info->m_picarea[k].xmax;
			m_picarea[k].ymax = info->m_picarea[k].ymax;
		}
	}
	else
	{
		m_picarea = NULL;
		m_pic_act_area = 0;
		m_picarea_no = 0;
	}
}

int CNCLFormProp::Add_picarea()
{
	CNCLFormProp *info = new CNCLFormProp(4);
	info->CopyPropertyPage(this);
	int indx = -1;
	if (m_picarea_no>0)
		indx = m_picarea_no;
	if (m_picarea_no==0)
		indx = 0;

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
		m_picarea = NULL;
	}
	m_picarea_no = info->m_picarea_no + 1;
	if (m_picarea_no>0)
	{
		m_picarea = (UD_PICAREA *) uu_malloc(m_picarea_no*sizeof(UD_PICAREA));
	}
	else
		return -1;
	k = 0;
	if ((m_picarea_no>0)&&(info->m_picarea!=NULL))
	{
		for (k=0; k<info->m_picarea_no; k++)
		{
			if (info->m_picarea[k].params!=NULL)
			{
				m_picarea[k].params = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_picarea[k].params, info->m_picarea[k].params);
			}
			else
				m_picarea[k].params = NULL;
			if (info->m_picarea[k].tooltext!=NULL)
			{
				m_picarea[k].tooltext = (char*) uu_malloc((100)*sizeof(char));
				strcpy(m_picarea[k].tooltext, info->m_picarea[k].tooltext);
			}
			else
				m_picarea[k].tooltext = NULL;
			strcpy(m_picarea[k].name, info->m_picarea[k].name);
			m_picarea[k].xmin = info->m_picarea[k].xmin;
			m_picarea[k].ymin = info->m_picarea[k].ymin;
			m_picarea[k].xmax = info->m_picarea[k].xmax;
			m_picarea[k].ymax = info->m_picarea[k].ymax;
		}
	}
	m_picarea[k].params = (char*) uu_malloc(100*sizeof(char));
	m_picarea[k].params[0] = '\0';
	m_picarea[k].tooltext = (char*) uu_malloc(100*sizeof(char));
	m_picarea[k].tooltext[0] = '\0';
	m_picarea[k].name[0] = '\0';
	m_picarea[k].xmin = 0;
	m_picarea[k].ymin = 0;
	m_picarea[k].xmax = 20;
	m_picarea[k].ymax = 20;
	m_pic_act_area = indx;
	return indx;
}

void CNCLFormProp::Delete_picarea(int indx)
{
	int k, len;
	if (m_picarea[indx].params!=NULL)
		uu_free(m_picarea[indx].params);
	if (m_picarea[indx].tooltext!=NULL)
		uu_free(m_picarea[indx].tooltext);
	for (k=indx; (k<m_picarea_no-1)&&(k>=0); k++)
	{
		len =strlen(m_picarea[k+1].params);
		m_picarea[k].params = m_picarea[k+1].params;
		m_picarea[k].tooltext = m_picarea[k+1].tooltext;
		strcpy(m_picarea[k].name, m_picarea[k+1].name);
		m_picarea[k].xmin = m_picarea[k+1].xmin;
		m_picarea[k].ymin = m_picarea[k+1].ymin;
		m_picarea[k].xmax = m_picarea[k+1].xmax;
		m_picarea[k].ymax = m_picarea[k+1].ymax;
	}
	m_picarea_no--;
	if (m_picarea_no<=0)
		m_pic_act_area = 0;
	else
		m_pic_act_area = indx;
	if ((m_picarea_no==0)&&(m_picarea!=NULL))
	{
		uu_free((char*)m_picarea);
		m_picarea = NULL;
	}
}
//not use now
//return 1, already have picture define: always add it now
//return 0, new picture define
int CNCLFormProp::GetPicRect(char *pic_label, float rect[4], int indx)
{
	int i;
	if ((indx<0)||(m_picarea==NULL))
	{
/*
.....insert a picture area
*/
		indx = Add_picarea();
	}
	strcpy(m_picarea[indx].name, pic_label);
	rect[0] = m_picarea[indx].xmin = 0;
	rect[2] = m_picarea[indx].xmax = 20;
	rect[1] = m_picarea[indx].ymin = 0;
	rect[3] = m_picarea[indx].ymax = 20;
	return indx;
}


//return 1, already have picture define: always add it now
//return 0, new picture define
int CNCLFormProp::SetNewPicRect(char *pic_label, float rect[4], int indx)
{
	int i;
	if ((indx<0)||(m_picarea==NULL))
	{
/*
.....insert a picture area
*/
		indx = Add_picarea();
	}
	strcpy(m_picarea[indx].name, pic_label);
	m_picarea[indx].xmin = rect[0];
	m_picarea[indx].xmax = rect[2];
	m_picarea[indx].ymin = rect[1];
	m_picarea[indx].ymax = rect[3];
	return indx;
}

int CNCLFormProp::GetActivePicArea()
{
	if ((m_pic_act_area<0)||(m_picarea_no==0)||(m_picarea==NULL))
		return -1;
	if ((m_picarea[m_pic_act_area].name[0]=='\0')||(m_picarea[m_pic_act_area].xmin==-1000))
		return -1;
	else
		return m_pic_act_area;
}
void CNCLFormProp::free_picdata()
{
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
		m_picarea = NULL;
	}
	m_picarea_no = 0;
}
