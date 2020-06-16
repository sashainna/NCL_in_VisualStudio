//#if UU_COMP == UU_WIN2K
/************************************************************************
**		FILE NAME: toollstctl.cpp
**
**	 Description - Functions and implementations for
**		CTLListCtrl class (NCL table list)
**		This class right is basical empty but we may need it later
**		for our own feature (such as scrollbar, size the column/color...)
**	 CONTAINS:
**		member function of CTLListCtrl
**
**    COPYRIGHT 2008 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			toollstctl.cpp , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:48:17
**********************************************************************
*/
#include "toolibstdafx.h"
#include "zsysdep.h"
#include "toollstctl.h"
#include "toolgettxt2.h"
#include "toolibres.h"
#include "udforms.h"

extern "C" void ud_tlist_copy(UD_TLIST *list1, UD_TLIST *list2);
extern "C" int ud_free_tlist(UD_TLIST *formlist);
extern "C" char * uu_malloc(int size );
extern "C" void uu_free( char* );
extern "C" int ncl_filter_str2(char*name, char*filter);
extern "C" void ud_tlist_copy_idata(UD_ITEMDATA *data1, UD_ITEMDATA *data2);
extern "C" void ud_tlist_free_idata(UD_ITEMDATA *seldata);
extern "C" int ud_issame_idata(UD_ITEMDATA *data1, UD_ITEMDATA *data2);
extern "C" void ul_to_upper(char*str);
extern "C" void ud_tlist_delete(UD_TLIST *list, int pos);
extern "C" void ud_tlist_insert(UD_TLIST *list, int pos, UD_ITEMDATA *data);

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


#if defined _M_IX86
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='x86' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_IA64
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='ia64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_X64
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='amd64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#else
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")
#endif

/////////////////////////////////////////////////////////////////////////////
// CTLListCtrl

CTLListCtrl::CTLListCtrl()
{
	m_selitem = -1;
	m_change = 0;
	m_type = 0;
	m_filter_flag = 0;
	m_orig_list.num_col = 0;
	m_orig_list.num_item = 0;
	m_orig_list.sort = 0;
	m_current_list.num_col = 0;
	m_current_list.num_item = 0;
	for (int i=0; i<50; i++)
	{
		m_filter_type[i] = 0;
		m_filter[i] = "*";
	}
	m_frmfld = -1;
	m_update = 1;
}

CTLListCtrl::~CTLListCtrl()
{
	ud_free_tlist(&m_orig_list);
	ud_free_tlist(&m_current_list);
}

void CTLListCtrl::GetCurrentTlist(UD_TLIST *list)
{
	ud_tlist_copy(&m_current_list, list);
}

BEGIN_MESSAGE_MAP(CTLListCtrl, CListCtrl)
	//{{AFX_MSG_MAP(CTLListCtrl)
	ON_WM_NCCALCSIZE()
	ON_NOTIFY(HDN_DROPDOWN, 0, OnDropdown)
	ON_MESSAGE(ID_FILTER_TLIST, OnFilterTlist)
	ON_NOTIFY_REFLECT(NM_CUSTOMDRAW, OnCustomDraw)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTLListCtrl message handlers

//****************************************************************************************
LRESULT CTLListCtrl::OnFilterTlist(WPARAM wParam, LPARAM lParam)
{
/*
.....if m_filter[item] is "*", reset all
.....if not mark this column have filter
*/
	LVCOLUMN lvColumn;
	for (int i=0; i<7;i++)
	{
		lvColumn.mask = LVCF_TEXT;
		if ((m_filter[i]!="*")&&(m_filter[i]!=""))
		{
			if (i==0)
				lvColumn.pszText = _T("+Toolno");
			else if (i==1)
				lvColumn.pszText = _T("+Type");
			else if (i==2)
				lvColumn.pszText = _T("+Description");
			else if (i==3)
				lvColumn.pszText = _T("+Diameter");
			else if (i==4)
				lvColumn.pszText = _T("+Radius");
			else if (i==5)
				lvColumn.pszText = _T("+Height");
			else if (i==6)
				lvColumn.pszText = _T("+Angle");
		}
		else
		{
			if (i==0)
				lvColumn.pszText = _T("Toolno");
			else if (i==1)
				lvColumn.pszText = _T("Type");
			else if (i==2)
				lvColumn.pszText = _T("Description");
			else if (i==3)
				lvColumn.pszText = _T("Diameter");
			else if (i==4)
				lvColumn.pszText = _T("Radius");
			else if (i==5)
				lvColumn.pszText = _T("Height");
			else if (i==6)
				lvColumn.pszText = _T("Angle");
		}
		SetColumn(i, &lvColumn);
	}
	return 0;
}

void CTLListCtrl::OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS FAR* lpncsp) 
{
/*	ModifyStyle(WS_HSCROLL | WS_VSCROLL,0,0); */
	CListCtrl::OnNcCalcSize(bCalcValidRects, lpncsp);
}

void CTLListCtrl::SetTlist(UD_TLIST *list)
{
	ud_free_tlist(&m_orig_list);
	ud_tlist_copy(list, &m_orig_list);
	ud_free_tlist(&m_current_list);
	ud_tlist_copy(list, &m_current_list);
	for (int i=0; i<50; i++)
	{
		m_filter_type[i] = 0;
		m_filter[i] = "*";
	}
}

void CTLListCtrl::FilterList()
{
/*
......first filt the list and create a new UD_TLIST and reset the CTLListCtrl data
*/
	int i, j, k, len;
	char *buf, filter[80], filter_cap[80];
	char label[80], label_cap[80];
	UD_ITEMDATA sel_data;
	int answer = -1;
/*
......do not call item changed related function until all done
......because SetItemData will call item changed callback functions
......many time when we reset the item
*/
	m_update = 0;
	sel_data.itemnum = 0;
	if (m_current_list.num_item!=0)
	{
		answer = m_current_list.answer; 
		ud_tlist_copy_idata(&(m_current_list.data[answer]), &sel_data);
		ud_free_tlist(&m_current_list);
	}
/*
.....selected index
*/
	m_current_list.num_col = m_orig_list.num_col;
	if (m_current_list.num_col>0)
		m_current_list.col_label = (char**) uu_malloc(m_current_list.num_col*sizeof (char*));
	for (i=0; i<m_current_list.num_col;i++)
	{
		len = strlen (m_orig_list.col_label[i]);
		m_current_list.col_label[i] = (char*) uu_malloc((len+1)*sizeof(char));
		strcpy(m_current_list.col_label[i], m_orig_list.col_label[i]);
	}
	m_current_list.sort = m_orig_list.sort;
	m_current_list.answer = 0;
	if (m_orig_list.num_item>0)
		m_current_list.data = (UD_ITEMDATA*) uu_malloc(m_orig_list.num_item*sizeof (UD_ITEMDATA));
#ifdef _UNICODE	
	WCHAR *wbuf;
	for (i=0,j=0; i<m_orig_list.num_item; i++)
	{
		for (k=0; k<m_current_list.num_col;k++)
		{
			len = m_filter[k].GetLength();
			if (len>0)
			{
				wbuf = m_filter[k].GetBuffer(len);
				wcstombs(filter, wbuf, len);
				filter[len] = '\0';
			}
			else
				filter[0] = '\0';
			if ((filter[0]!='\0')&&(strcmp(filter, "*")!=0))
			{
				strcpy(label_cap, m_orig_list.data[i].data_items[k]);
				strcpy(filter_cap, filter);		
				ul_to_upper(label_cap);
				ul_to_upper(filter_cap);
				if (ncl_filter_str2(label_cap, filter_cap)!=0)
				{
/*
.....matched
*/
					continue;
				}
				else
					break;
			}
		}
		if (k==m_current_list.num_col)
		{
/*
......item go pass all filter, add to 
*/
			m_current_list.data[j].itemnum = m_current_list.num_col;
			m_current_list.data[j].data_items = 
					(char **) uu_malloc(m_current_list.num_col*sizeof(char*));
			for (k=0; k<m_current_list.num_col;k++)
			{
				len = strlen(m_orig_list.data[i].data_items[k]);
				m_current_list.data[j].data_items[k] = (char*)uu_malloc((len+1)*sizeof(char));
				strcpy(m_current_list.data[j].data_items[k], m_orig_list.data[i].data_items[k]);
			}
			if ((answer!=-1)&&(ud_issame_idata(&(m_current_list.data[j]), &sel_data)))
				m_current_list.answer = j - 1;
			j++;
		}
	}
	m_current_list.num_item = j;
	ud_tlist_free_idata(&sel_data);
/*
.....reset m_current_list to the listing
*/
	DeleteAllItems();
	CString tstr;
	for(j=0;j<m_current_list.num_item;j++)
	{
//		InsertItem(j,  m_current_list.data[j].data_items[0]);
		tstr = m_current_list.data[j].data_items[0];
		InsertItem(j,  tstr);
		for (int m=1; m<m_current_list.data[j].itemnum;m++)
		{
			tstr = m_current_list.data[j].data_items[m];
			SetItemText(j, m, tstr);
		}
		m_current_list.data[j].frmid = m_frmid;
		m_current_list.data[j].fldno = m_frmfld;
		SetItemData(j, (LPARAM)&(m_current_list.data[j]));
	}
#else
	for (i=0,j=0; i<m_orig_list.num_item; i++)
	{
		for (k=0; k<m_current_list.num_col;k++)
		{
			len = m_filter[k].GetLength();
			buf = m_filter[k].GetBuffer(len);
			strcpy(filter, buf);
			if ((filter[0]!='\0')&&(strcmp(filter, "*")!=0))
			{
				strcpy(label_cap, m_orig_list.data[i].data_items[k]);
				strcpy(filter_cap, filter);		
				ul_to_upper(label_cap);
				ul_to_upper(filter_cap);
				if (ncl_filter_str2(label_cap, filter_cap)!=0)
				{
/*
.....matched
*/
					continue;
				}
				else
					break;
			}
		}
		if (k==m_current_list.num_col)
		{
/*
......item go pass all filter, add to 
*/
			m_current_list.data[j].itemnum = m_current_list.num_col;
			m_current_list.data[j].data_items = 
					(char **) uu_malloc(m_current_list.num_col*sizeof(char*));
			for (k=0; k<m_current_list.num_col;k++)
			{
				len = strlen(m_orig_list.data[i].data_items[k]);
				m_current_list.data[j].data_items[k] = (char*)uu_malloc((len+1)*sizeof(char));
				strcpy(m_current_list.data[j].data_items[k], m_orig_list.data[i].data_items[k]);
			}
			if ((answer!=-1)&&(ud_issame_idata(&(m_current_list.data[j]), &sel_data)))
				m_current_list.answer = j - 1; 
			j++;
		}
	}
	m_current_list.num_item = j;
	ud_tlist_free_idata(&sel_data);
/*
.....reset m_current_list to the listing
*/
	DeleteAllItems();
	for(j=0;j<m_current_list.num_item;j++)
	{
		InsertItem(j,  m_current_list.data[j].data_items[0]);
		for (int m=1; m<m_current_list.data[j].itemnum;m++)
		{
			SetItemText(j, m, m_current_list.data[j].data_items[m]);
		}
		SetItemData(j, (LPARAM)&(m_current_list.data[j]));
	}
#endif
	m_update = 1;
/*
......have to set the focus to set a selection
*/
	SetFocus();
	if ((m_current_list.answer>=0)&&(m_current_list.answer<m_current_list.num_item))
	{
		SetItemState (m_current_list.answer, LVIS_SELECTED, LVIS_SELECTED);
		EnsureVisible(m_current_list.answer, FALSE);
	}
/*
......then post message to let parent know the list updated
*/
	PostMessage(ID_FILTER_TLIST, (WPARAM) NULL, (LPARAM)NULL);
//we post to parent here even for now, we don't need it.
	m_parent->PostMessage(ID_FILTER_TLIST, (WPARAM) NULL, (LPARAM)m_frmfld);
}
/*******************************************************************
**   E_FUNCTION : DeleteItemPos(int pos, int flag, UD_ITEMDATA *data)
**              This function delete/add a list position
**   PARAMETERS
**       INPUT  : 
**			pos: position to delete/add
**			flag: 0, add, 1, delete
**			data: data to be add
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
//need update m_orig_list too
void CTLListCtrl::DeleteItemPos(int pos, int flag, UD_ITEMDATA *data)
{
	if ((m_current_list.num_item<=0) && (flag==1))
		return;
	if (flag)
	{
		if (data->itemnum<=0)
			return;
/*
.....update the m_current_list
*/
		ud_tlist_delete(&m_current_list, pos);
		DeleteItem(pos);
	}
	else
	{
/*
.....update the m_current_list
*/
		ud_tlist_insert(&m_current_list, pos, data);
		CString tstr;
#ifdef _UNICODE	
		tstr = data->data_items[0];
#else
		tstr = data->data_items[0];
#endif
		InsertItem(pos,  tstr);
		for (int m=1; m<data->itemnum;m++)
		{
#ifdef _UNICODE	
			tstr = data->data_items[m];
#else
			tstr = data->data_items[m];
#endif
			SetItemText(pos, m, tstr);
		}
		SetItemData(pos, (LPARAM)data);
	}
}
void CTLListCtrl::DeleteColumnItem(int column, char *toolstr, int type)
{
	double num1, num2;
	int current_sel = m_current_list.answer;
	int pos = -1;
	int opos = -1;
	for(int j=0;j<m_current_list.num_item;j++)
	{
		if (type==1)
		{
			if (stricmp(m_current_list.data[j].data_items[column], toolstr)==0)
			{
				pos = j;
				break;
			}
		}
		else
		{
			num1 = atof(toolstr);
			num2 = atof(m_current_list.data[j].data_items[column]);
			if (num1==num2)
			{
				pos = j;
				break;
			}
		}
	}
/*
.....update the m_current_list
*/
	if (pos!=-1)
	{
		UD_ITEMDATA sel_data;
		ud_tlist_copy_idata(&(m_current_list.data[pos]), &sel_data);
		for (int i=0; i<m_orig_list.num_item; i++)
		{
			if (ud_issame_idata(&(m_orig_list.data[i]), &sel_data))
			{
				opos = i;
				break;
			}
		}
		ud_tlist_delete(&m_current_list, pos);
		DeleteItem(pos);
		if (m_current_list.answer>=m_current_list.num_item)
			m_current_list.answer = m_current_list.num_item - 1;
		ud_tlist_delete(&m_orig_list, opos);

		if (pos>current_sel)
			m_current_list.answer = current_sel;
		else
			m_current_list.answer = pos - 1;
		if (m_current_list.answer<0)
			m_current_list.answer = 0;
/*
......have to set the focus to set a selection
*/
		SetFocus();
		if ((m_current_list.answer>=0)&&(m_current_list.answer<m_current_list.num_item))
		{
			SetItemState (m_current_list.answer, LVIS_SELECTED, LVIS_SELECTED);
			EnsureVisible(m_current_list.answer, FALSE);
		}
		ud_tlist_free_idata(&sel_data);
	}
}
//update column data 
void CTLListCtrl::UpdateItemValue(int current_pos, int orignal_pos, UD_ITEMDATA *data)
{
	int len;
	CString tstr;
#ifdef _UNICODE	
	tstr = data->data_items[0];
#else
	tstr = data->data_items[0];
#endif
	SetItemText(current_pos, 0, tstr);
	for (int m=1; m<data->itemnum;m++)
	{
#ifdef _UNICODE	
		tstr = data->data_items[m];
#else
		tstr = data->data_items[m];
#endif
		SetItemText(current_pos, m, tstr);
	}
	for (int j=0; j<m_current_list.num_col; j++)
	{
		uu_free(m_current_list.data[current_pos].data_items[j]);
		len = strlen(data->data_items[j]);
		m_current_list.data[current_pos].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
		strcpy(m_current_list.data[current_pos].data_items[j], data->data_items[j]);
	}
	for (int j=0; j<m_orig_list.num_col; j++)
	{
		uu_free(m_orig_list.data[orignal_pos].data_items[j]);
		len = strlen(data->data_items[j]);
		m_orig_list.data[orignal_pos].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
		strcpy(m_orig_list.data[orignal_pos].data_items[j], data->data_items[j]);
	}
	m_current_list.answer = current_pos;
	SetFocus();
	if ((m_current_list.answer>=0)&&(m_current_list.answer<m_current_list.num_item))
	{
		SetItemState (m_current_list.answer, LVIS_SELECTED, LVIS_SELECTED);
		EnsureVisible(m_current_list.answer, FALSE);
	}
}
//current list match
int CTLListCtrl::FindColumnItemMatch1(int column, char *toolstr, int type)
{
	double num1, num2;
	int ret = 0;
	for(int j=0;j<m_current_list.num_item;j++)
	{
		if (type==1)
		{
			if (stricmp(m_current_list.data[j].data_items[column], toolstr)==0)
			{
				ret = j;
				break;
			}
		}
		else
		{
			num1 = atof(toolstr);
			num2 = atof(m_current_list.data[j].data_items[column]);
			if (num1==num2)
			{
				ret = j;
				break;
			}
		}
	}
	return ret;
}
//original list match
int CTLListCtrl::FindColumnItemMatch2(int column, char *toolstr, int type)
{
	double num1, num2;
	int ret = 0;
	for(int j=0;j<m_orig_list.num_item;j++)
	{
		if (type==1)
		{
			if (stricmp(m_orig_list.data[j].data_items[column], toolstr)==0)
			{
				ret = j;
				break;
			}
		}
		else
		{
			num1 = atof(toolstr);
			num2 = atof(m_orig_list.data[j].data_items[column]);
			if (num1==num2)
			{
				ret = j;
				break;
			}
		}
	}
	return ret;
}

int CTLListCtrl::SetSelPosOrig(int pos)
{
	UD_ITEMDATA sel_data;
	int answer = pos;
	if (pos<0)
		return -1;
	sel_data.itemnum = 0;
	if (m_orig_list.num_item!=0)
	{
		pos = -1;
		ud_tlist_copy_idata(&(m_orig_list.data[answer]), &sel_data);
		for (int i=0; i<m_current_list.num_item; i++)
		{
			if (ud_issame_idata(&(m_current_list.data[i]), &sel_data))
			{
				pos = i;
				break;
			}
		}
	}
	if (pos>=0)
		m_current_list.answer = pos;
/*
......have to set the focus to set a selection
*/
	SetFocus();
	if ((m_current_list.answer>=0)&&(m_current_list.answer<m_current_list.num_item))
	{
		SetItemState (m_current_list.answer, LVIS_SELECTED, LVIS_SELECTED);
		EnsureVisible(m_current_list.answer, FALSE);
	}
	return 0;
}

void CTLListCtrl::OnDropdown(NMHDR* pNMHDR, LRESULT* pResult) 
{
	int item;
	NMHEADER* nmkd = (NMHEADER*)pNMHDR;
	item = nmkd->iItem;
			
	int size[2];
	CRect rect;
	size[0] = 100;
	size[1] = 20;
	CString input_text = m_filter[item];
/*
.....if params is empty, use current value
*/
	int len = GetInputText(input_text, _T("Filter:"), size);
	if (len==-1)
		return;
	input_text.TrimLeft(_T("\t "));
	len = input_text.GetLength();
	int filt = 1;
	if (len>0)
	{
		if ((m_filter[item]=="")&&(input_text=="*"))
			filt = 0;
		if (m_filter[item]!=input_text)
		{
			m_filter[item] = input_text;
		}
		else
			filt = 0;
	}
	else
	{
		if ((m_filter[item]=="*")||(m_filter[item]==""))
			filt = 0;
		m_filter[item] = "";
	}
	if (filt)
		FilterList();
}
int CTLListCtrl::GetInputText(CString &input_text, CString prompt_text, int size[2])
{
	CNCLGetText2 *dlg = new CNCLGetText2(this);
#ifdef _UNICODE	
	char tmpstr[1000];
	WCHAR *wstr;
	int nc = input_text.GetLength();
	if (nc>0)
	{
		wstr = input_text.GetBuffer(nc);
		wcstombs(tmpstr, wstr, nc);
		tmpstr[nc] = '\0';
		dlg->SetTextString(tmpstr);
	}
	else
		dlg->SetTextString("");
	nc = prompt_text.GetLength();
	if (nc>0)
	{
		wstr = prompt_text.GetBuffer(nc);
		wcstombs(tmpstr, wstr, nc);
		tmpstr[nc] = '\0';
		dlg->SetPromptString(tmpstr);
	}
	else
		dlg->SetPromptString("");
#else
	dlg->SetTextString(input_text.GetBuffer());
	dlg->SetPromptString(prompt_text.GetBuffer());
#endif
	CPoint pt;
	GetCursorPos(&pt);
	pt.x = pt.x - size[0];
	dlg->SetPos(pt, size);
	int nResponse = dlg->DoModal();
	if (nResponse == IDOK)
	{
		dlg->GetTextString(input_text);
		delete dlg;
		return input_text.GetLength();
	}
	else
	{
		delete dlg;
		return -1;
	}
}

void CTLListCtrl::OnCustomDraw(NMHDR* pNMHDR, LRESULT* pResult)
{
	int row;
	int col;
	ENSURE(pNMHDR != NULL);
	LPNMLVCUSTOMDRAW lplvcd = (LPNMLVCUSTOMDRAW)pNMHDR;

	switch (lplvcd->nmcd.dwDrawStage)
	{
	case CDDS_PREPAINT:
		*pResult = CDRF_NOTIFYITEMDRAW;
		break;

	case CDDS_ITEMPREPAINT:
		*pResult = CDRF_NOTIFYSUBITEMDRAW;
		break;

	case CDDS_ITEMPREPAINT | CDDS_SUBITEM:
		{
			int iColumn = lplvcd->iSubItem;
			int iRow = (int) lplvcd->nmcd.dwItemSpec;
			if (m_selitem==iRow)
				lplvcd->clrTextBk = GetSysColor(COLOR_HIGHLIGHT);
			else
				lplvcd->clrTextBk = OnGetCellBkColor(iRow, iColumn);
			if (m_selitem==iRow)
				lplvcd->clrText = RGB(255,255,255);
			else
				lplvcd->clrText = OnGetCellTextColor(iRow, iColumn);

			if (iColumn == m_iSortedColumn && m_bMarkSortedColumn && lplvcd->clrTextBk == GetBkColor())
			{
				lplvcd->clrTextBk = m_clrSortedColumn;
			}

			HFONT hFont = OnGetCellFont( iRow, iColumn, (DWORD) lplvcd->nmcd.lItemlParam);
			if (hFont != NULL)
			{
				m_hOldFont = (HFONT) SelectObject(lplvcd->nmcd.hdc, hFont);
				ENSURE(m_hOldFont != NULL);

				*pResult = CDRF_NEWFONT | CDRF_NOTIFYPOSTPAINT;
			}
			else
			{
				*pResult = CDRF_DODEFAULT;
			}
		}
		break;

	case CDDS_ITEMPOSTPAINT | CDDS_SUBITEM:
		if (m_hOldFont != NULL)
		{
			SelectObject(lplvcd->nmcd.hdc, m_hOldFont);
			m_hOldFont = NULL;
		}

		*pResult = CDRF_DODEFAULT;
		break;
	}
}


IMPLEMENT_DYNAMIC(CTLListCtrl, CDialog)

//****************************************************************************************
//#endif
