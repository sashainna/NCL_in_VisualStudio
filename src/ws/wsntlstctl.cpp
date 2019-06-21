#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**		FILE NAME: CNCLListCtrl.cpp
**
**	 Description - Functions and implementations for
**		CNCLListCtrl class (NCL table list)
**		This class right is basical empty but we may need it later
**		for our own feature (such as scrollbar, size the column/color...)
**	 CONTAINS:
**		member function of CNCLListCtrl
**
**    COPYRIGHT 2008 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntlstctl.cpp , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:38:42
**********************************************************************
*/
#include "wsntstdafx.h"
#include "wsntlstctl.h"
#include "wsntgettxt2.h"
#include "wsntfsview.h"
#include "wsntres.h"
#include "udforms.h"

extern "C" void ud_tlist_copy(UD_TLIST *list1, UD_TLIST *list2);
extern "C" int ud_free_tlist(UD_TLIST *formlist);
extern "C" char * uu_malloc(int size );
extern "C" int ncl_filter_str2(char*name, char*filter);
extern "C" void ud_tlist_copy_idata(UD_ITEMDATA *data1, UD_ITEMDATA *data2);
extern "C" void ud_tlist_free_idata(UD_ITEMDATA *seldata);
extern "C" int ud_issame_idata(UD_ITEMDATA *data1, UD_ITEMDATA *data2);
extern "C" void ul_to_upper(char*str);

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNCLListCtrl

CNCLListCtrl::CNCLListCtrl()
{
	m_selitem = -1;
	m_change = 0;
	m_type = 0;
	m_filter_flag = 0;
	m_orig_list.sort = 0;
	m_orig_list.num_col = 0;
	m_orig_list.num_item = 0;
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

CNCLListCtrl::~CNCLListCtrl()
{
	ud_free_tlist(&m_orig_list);
	ud_free_tlist(&m_current_list);
}

void CNCLListCtrl::GetCurrentTlist(UD_TLIST *list)
{
	ud_tlist_copy(&m_current_list, list);
}

BEGIN_MESSAGE_MAP(CNCLListCtrl, CListCtrl)
	//{{AFX_MSG_MAP(CNCLListCtrl)
	ON_WM_NCCALCSIZE()
	ON_NOTIFY(HDN_DROPDOWN, 0, OnDropdown)
	ON_NOTIFY_REFLECT(NM_CUSTOMDRAW, OnCustomDraw)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/**********************************************************************
**    I_FUNCTION :  OnCustomDraw(NMHDR* pNMHDR, LRESULT* pResult)
**       command callback routine for NM_CUSTOMDRAW on this control list
**    PARAMETERS   
**       INPUT  : 
**				pNMHDR             
**       OUTPUT :  
**				pResult
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
/*
void CNCLListCtrl::OnCustomDraw(NMHDR *pNMHDR, LRESULT *pResult)
{
	NMLVCUSTOMDRAW* nmcd = (NMLVCUSTOMDRAW*)pNMHDR;
	*pResult = CDRF_DODEFAULT;	
	int row;
	int col;

	switch(nmcd->nmcd.dwDrawStage)
	{
		case CDDS_PREPAINT:
			*pResult = CDRF_NOTIFYITEMDRAW;
			return;

		case CDDS_ITEMPREPAINT:
			*pResult = CDRF_NOTIFYSUBITEMDRAW;
			return;
		
		case CDDS_SUBITEM|CDDS_ITEMPREPAINT:
		{
			*pResult = 0;

			row = nmcd->nmcd.dwItemSpec;
			col = nmcd->iSubItem;

			CString str = GetItemText(row,col);

			CRect rect;
			CDC* pDC=CDC::FromHandle(nmcd->nmcd.hdc);

			if(col>0)
				GetSubItemRect(row,col,LVIR_BOUNDS,rect);
			else
				GetItemRect(row,&rect,LVIR_LABEL);

			UINT uCode = DT_LEFT;
			
			if(row==1 && col==1)
			{
				UD_ITEMDATA *data = (UD_ITEMDATA *)GetItemData(row);
				if ((col!=0)&&(col<data->itemnum))
				{
					COLORREF color = RGB(220,220,220);

					if(GetFocus()==this)
						color = GetSysColor(COLOR_HIGHLIGHT);

					CBrush brush(color);
					pDC->FillRect(&rect,&brush);
				}
			}
			pDC->DrawText(str,&rect,uCode);
			*pResult = CDRF_SKIPDEFAULT;
			break;
		}
	}
}
*/

void CNCLListCtrl::OnCustomDraw(NMHDR* pNMHDR, LRESULT* pResult)
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

/////////////////////////////////////////////////////////////////////////////
// CNCLListCtrl message handlers

//****************************************************************************************
//****************************************************************************************
LRESULT CNCLListCtrl::OnFilterTlist(WPARAM wParam, LPARAM lParam)
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

COLORREF CNCLListCtrl::OnGetCellBkColor(int nRow, int nCol)
{
	return GetBkColor();
}
COLORREF CNCLListCtrl::OnGetCellTextColor(int /*nRow*/, int /*nColum*/) 
{
	return GetTextColor(); 
}

void CNCLListCtrl::OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS FAR* lpncsp) 
{
/*	ModifyStyle(WS_HSCROLL | WS_VSCROLL,0,0); */
	CListCtrl::OnNcCalcSize(bCalcValidRects, lpncsp);
}

void CNCLListCtrl::SetTlist(UD_TLIST *list)
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

void CNCLListCtrl::FilterList()
{
/*
......first filt the list and create a new UD_TLIST and reset the CNCLListCtrl data
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
		m_current_list.data[j].frmid = m_frmid;
		m_current_list.data[j].fldno = m_frmfld;
		SetItemData(j, (LPARAM)&(m_current_list.data[j]));
	}
	m_update = 1;
/*
......have to set the focus to set a selection
*/
	SetFocus();
	if ((m_current_list.answer>=0)&&(m_current_list.answer<m_current_list.num_item))
		SetItemState (m_current_list.answer, LVIS_SELECTED, LVIS_SELECTED);
/*
......then post message to let parent know the list updated
*/
	m_parent->PostMessage(ID_FILTER_TLIST, (WPARAM) m_frmid, (LPARAM)m_frmfld);
}

void CNCLListCtrl::OnDropdown(NMHDR* pNMHDR, LRESULT* pResult) 
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
	int len = GetInputText(input_text, "Filter:", size);
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
int CNCLListCtrl::GetInputText(CString &input_text, CString prompt_text, int size[2])
{
	CNCLGetText2 *dlg = new CNCLGetText2(this);
	dlg->SetTextString(input_text.GetBuffer());
	dlg->SetPromptString(prompt_text.GetBuffer());
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

IMPLEMENT_DYNAMIC(CNCLListCtrl, CDialog)

//****************************************************************************************
#endif
